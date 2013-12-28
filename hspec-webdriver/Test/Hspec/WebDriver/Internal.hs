{-# LANGUAGE ExistentialQuantification #-}
module Test.Hspec.WebDriver.Internal(
    TestCapabilities(..)
  , WdExample(..)
  , createSessionManager
  , createSessionManager'
  , withCaps
  , takeSession
  , putSessionId
) where

import Control.Concurrent.STM
import Control.Exception.Lifted (finally, onException)
import Control.Monad.IO.Class (liftIO)
import Data.Hashable
import Data.List (partition)
import Data.Maybe (fromJust)
import Data.Typeable
import Data.Word (Word16)
import System.IO.Unsafe (unsafePerformIO)
import Test.WebDriver
import Test.WebDriver.Classes
import Test.Hspec.Core (Params, Result)
import qualified Data.HashMap.Lazy as M

-- | Provides information about the browser capabilities you are using for testing.  You must make
-- an enumeration of all the webdriver capabilities you will be testing with.  For example,
--
-- >data TestCaps = Firefox
-- >              | FirefoxWithoutJavascript
-- >              | Chrome
-- >   deriving (Show, Eq, Bounded, Enum, Typeable)
--
-- (The 'Typeable' instance is needed for an @Eq@ instance on an existentially quantified type
-- over @TestCapabilities@.)
class (Eq c, Enum c, Typeable c) => TestCapabilities c where

    -- | Check if the 'Capabilities' match your enumeration.  Note that these capabilities
    -- will be the actual capabilities (with things like version information filled in)
    -- so you should not use @==@ to compare capabilities.  For example, to match Chrome
    -- you could use
    --
    -- > matchCaps Chrome (Capabilities { browser = Chrome _ _ _ _}) = True
    matchesCaps :: c -> Capabilities -> Bool

    -- | The capabilities to pass to 'createSession' when no existing session is found.
    newCaps :: c -> WD Capabilities

-- | A behavioral example for webdriver sessions.
class WdExample a where
    evaluateWdExamples :: a -> [(String, Params -> (IO () -> IO ()) -> IO Result)]

data SomeCap = forall c. TestCapabilities c => SomeCap c

instance Eq SomeCap where
    SomeCap c1 == SomeCap c2 = case cast c2 of
                                Just c2' -> c1 == c2'
                                Nothing -> False

instance Hashable SomeCap where
    hashWithSalt i (SomeCap c) = hashUsing fromEnum i c

data ManagedSessions = ManagedSessions
  { maxSessions :: Int
    -- ^ maximum number of sessions that should be created
  , managedSessions :: M.HashMap SomeCap (TVar ([SessionId],Int))
    -- ^ For each cap, store the list of unused sessions and a count of in-use sessions.
  , initialSessions :: [(SessionId,Capabilities)]
    -- ^ sessions which existed at program start and are not yet assigned.

  -- | settings for session
  , mwdHost :: String
  , mwdPort :: Word16
  , mwdBasePath :: String
  }

-- | Stores the managed sessions
sessionManager :: TVar (Maybe ManagedSessions)
{-# NOINLINE sessionManager #-}
sessionManager = unsafePerformIO (newTVarIO Nothing)

-- | Create and set the session manager
createWdMan :: Int -> Maybe (String, Word16, String) -> WD ()
createWdMan maxSess mSettings = do
    let (host,port,bpath) = case mSettings of
            Just s -> s
            Nothing -> (wdHost defaultSession, wdPort defaultSession, wdBasePath defaultSession)
    sess <- sessions

    let manager = ManagedSessions maxSess M.empty sess host port bpath

    liftIO $ atomically $ do
        mm <- readTVar sessionManager
        case mm of
            Just _ -> return () -- manager has already been created
            Nothing -> writeTVar sessionManager $ Just manager

-- | Create a new session manager using the default webdriver host (127.0.0.1), port (4444), and
-- basepath (@\/wd\/hub@).
--
-- The session manager hands out sessions to tests (in a thread-safe manner).  Threads ask for
-- sessions by an enumeration which is an instance of 'TestCapabilities', and the manager stores a
-- pool of sessions for each enumeration item.  When calling 'createSessionManager', the already
-- existing sessions are loaded and used as the initial sessions in the pools.  If a thread asks for
-- a session but none is available, one of two things happens:  if the total number of sessions for
-- this enumeration item is larger than the argument to 'createSessionManager', the thread will
-- block until a session is available.  If the total number of sessions for this enumeration item is
-- smaller, a new session will be created.  This is only relevant if you run tests in parallel,
-- since when running tests serially at most one session will be in use at any one time in any case.
-- Note that sessions are never closed by the manager.
--
-- If you do not call 'createSessionManager', when the very first test is run a new manager will be
-- created where the maximum number of sessions per enumeration item is one.
createSessionManager :: Int -- ^ threshold number of sessions per enumeration item beyond which new 
                            -- sessions are no longer created.  Note you can set this to zero so
                            -- that new sessions are never created; the only sessions used will be
                            -- those that already exist.
                     -> IO ()
createSessionManager maxSess = runWD defaultSession $ createWdMan maxSess Nothing

-- | Same as 'createSessionManager' but allows you to specify the webdriver host, port, and base
-- path for all sessions.
createSessionManager' :: Int  -- ^ maximum number of sessions per enumeration item
                      -> String -- ^ host
                      -> Word16 -- ^ port
                      -> String -- ^ base path
                      -> IO ()
createSessionManager' maxSess host port bpath = do
    let sess = WDSession { wdHost = host
                         , wdPort = port
                         , wdBasePath = bpath
                         , wdSessId = Nothing
                         , lastHTTPRequest = Nothing
                         }
    runWD sess $ createWdMan maxSess $ Just (host, port, bpath)

-- | Create a session
createSessionId :: SomeCap -> WD SessionId
createSessionId (SomeCap c) = do
    caps <- newCaps c
    sess <- createSession caps
    return $ fromJust $ wdSessId sess

-- | Searches for a session. Returns the action to be used to obtain the session.
findSession :: SomeCap -> ManagedSessions -> STM (WD SessionId)
findSession sc@(SomeCap c) m =
    case M.lookup sc $ managedSessions m of
        Just tvar -> do
            (sess,count) <- readTVar tvar
            case sess of
                (s:ss) -> do writeTVar tvar (ss, count + 1)
                             return $ return s

                [] | count >= maxSessions m -> retry -- retry blocks until a tvar changes

                   | otherwise -> do writeTVar tvar ([], count + 1)
                                     let create = createSessionId sc `onException` (liftIO $ atomically $ do
                                                    (s',cnt) <- readTVar tvar
                                                    writeTVar tvar (s', cnt - 1))
                                     return create

        Nothing -> do
            let (sess, unmanaged') = partition (\(_,cap) -> matchesCaps c cap) $ initialSessions m
            tvar <- newTVar (map fst sess, 0)
            let m' = m { initialSessions = unmanaged'
                       , managedSessions = M.insert sc tvar $ managedSessions m
                       }
            writeTVar sessionManager $ Just m'
            findSession sc m'

-- | Take a session out of the pool, using an existing unused session, creating a new session, or
-- blocking if the maximum number of sessions already exist.  The new session is set into the 'WD'
-- monad.  Note the session can leak if you do not properly call 'putSessionId'.
takeSession :: TestCapabilities s => s -> WD ()
takeSession s = do
    msess <- liftIO $ atomically $ do
        mm <- readTVar sessionManager
        case mm of
            Nothing -> return Nothing
            Just m -> do r <- findSession (SomeCap s) m
                         return $ Just (r, mwdHost m, mwdPort m, mwdBasePath m)
    case msess of
        Just (r, host, port, bpath) -> do
            let sess = WDSession { wdHost = host
                                 , wdPort = port
                                 , wdBasePath = bpath
                                 , wdSessId = Nothing
                                 , lastHTTPRequest = Nothing
                                 }
            putSession sess
            sid <- r
            putSession sess { wdSessId = Just sid }

        Nothing -> do
            createWdMan 1 Nothing
            takeSession s

-- | Add a session ID back into the pool.
putSessionId :: TestCapabilities s => s -> SessionId -> WD ()
putSessionId s sid = liftIO $ atomically $ do
    mm <- readTVar sessionManager
    let m = maybe (error "Cannot put a session to an uninitialized manager") id mm
    case M.lookup (SomeCap s) $ managedSessions m of
        Nothing -> error "Cannot put a session to a cap that does not exist"
        Just tvar -> do
            (ss,cnt) <- readTVar tvar
            writeTVar tvar (sid:ss,cnt-1)

-- | Find or create a new session, set it into the 'WD' monad, run the given action, and return the
-- session back into the pool once the action completes or an exception occurs.
withCaps :: TestCapabilities s => s -> WD a -> WD a
withCaps tc test = do
    takeSession tc
    sess <- getSession
    test `finally` putSessionId tc (fromJust $ wdSessId sess)
