{-# LANGUAGE CPP, DeriveDataTypeable, RankNTypes, FlexibleContexts, ScopedTypeVariables  #-}
module Test.Hspec.WebDriver.Internal (
  -- * State Sessions
    session
  , runState
  , with
  , SessionExample(..)
  , AbortSessionEx(..)
) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad (replicateM)
import Control.Monad.Trans.State (state, evalState, execState, execStateT, StateT)
import Data.Traversable (traverse)
import Data.Typeable (Typeable, cast)
import Data.IORef
import Test.Hspec
import Test.Hspec.Core hiding (describe, it)

import qualified Control.Exception as E

-- | Traverse a spec, but only if forceSpec has already been called
traverseTree :: Applicative f => (Item -> f Item) -> SpecTree -> f SpecTree
traverseTree f (SpecItem i) = SpecItem <$> f i
traverseTree f (SpecGroup msg ss) = SpecGroup msg <$> traverse (traverseTree f) ss
traverseTree f (SpecWithCleanup c ss) = SpecWithCleanup c <$> traverse (traverseTree f) ss

-- | Traverse a list of specs, but only if forceSpecs has already been called
traverseSpec :: Applicative f => (Item -> f Item) -> [SpecTree] -> f [SpecTree]
traverseSpec f = traverse (traverseTree f)

-- | Process the items in a depth-first walk, passing in the item counter value.
mapWithCounter :: (Int -> Item -> Item) -> [SpecTree] -> [SpecTree]
mapWithCounter f s = flip evalState 0 $ traverseSpec go s
    where
        go item = state $ \cnt -> (f cnt item, cnt+1)

countItems :: [SpecTree] -> Int
countItems s = flip execState 0 $ traverseSpec go s
    where
        go item = state $ \cnt -> (item, cnt+1)

data AbortSessionEx = AbortSessionEx
    deriving Typeable
instance E.Exception AbortSessionEx
instance Show AbortSessionEx where
    show AbortSessionEx = "Session Aborted"

data SessionTest a = SessionTest (IO () -> IO ()) (a -> IO a)
    deriving Typeable
instance Show (SessionTest a) where
    show _ = "Test must be contained within a session of matching state type"
instance Typeable a => E.Exception (SessionTest a)

-- | A session example, which contains an expectation and also transforms the state.
-- @SessionExample@s must be located as a child to a call to 'session' and the type @s@ must match between
-- the example and the call to 'session'.  Sessions cannot be nested, so if there is no parent call
-- to 'session' or the types @s@ do not match, the example will fail.
data SessionExample s = SessionExample (s -> IO s)
instance Typeable a => Example (SessionExample a) where
    evaluateExample (SessionExample f) _ act _ = E.throwIO $ SessionTest act f

data Session a = Session {
    sessionCount :: Int
  , sessionMVars :: [MVar (Either E.SomeException a)]
  , sessionCreate :: IO a
  , sessionClose :: a -> IO ()
}

sessionItem :: Typeable a => Session a -> Int -> Item -> Item
sessionItem sess i item =
        item { itemExample = \p a prog -> runTest $ itemExample item p a prog }
    where
        open | i == 0 = E.try $ sessionCreate sess
             | otherwise = takeMVar $ sessionMVars sess !! i

        close ma | i == sessionCount sess - 1 = either (const $ return ()) (sessionClose sess) ma
                 | otherwise = putMVar (sessionMVars sess !! (i+1)) ma

        runTest ex = do
            ma <- open
            mres <- E.try ex
            case mres of
                -- normal, non-session test.  Use the original state ma for the next test.
                Right res -> close ma >> return res

                Left (E.SomeException err) ->
                    case (ma, cast err) of
                        -- non-session test threw an error (since the cast to SessionTest
                        -- failed).  Use the original state ma for the next test and rethrow the
                        -- error.
                        (_, Nothing) -> close ma >> E.throwIO err

                        -- A session test, where in addition the open function succeeded.
                        (Right a, Just (SessionTest act f)) -> do
                            aborted <- newIORef False
                            act $ do
                                mstate <- E.try $ f a
                                case mstate of
                                    -- pass new state to next test
                                    Right st -> close (Right st)
                                    Left serr@(E.SomeException actErr) ->
                                        case cast actErr of
                                            -- pass abort to next test
                                            Just AbortSessionEx -> close (Left serr) >> writeIORef aborted True
                                            -- pass original state and rethrow the error
                                            Nothing -> close ma >> E.throwIO serr
                            abrt <- readIORef aborted
                            return $ if abrt then Pending (Just "Session Aborted") else Success
                            
                        -- A session test where the state ma is an error (which is the error
                        -- thrown by open).  Pass the error ma to the next test and throw the
                        -- error.
                        (Left err', _) -> close ma >> E.throwIO err'

-- | This function causes all child examples (sessions cannot be nested) to be executed serially in
-- depth-first order, tracking a state of type @s@ throughout the examples as they are processed in
-- order.  Examples which are 'SessionExample's (which are essentially functions @s -> IO s@) can view
-- and modify the state.
--
-- * If an example is not a 'SessionExample', the example is executed and there is no change in the
-- state.
--
-- * If an example is a 'SessionExample' but throws an exception, there is no change in the state.
--
-- * If an example is a 'SessionExample' and completes successfully, the state returned from the
-- example is used as the new state.
--
-- Just before the first example is run, the create action is executed to obtain the initial state.
-- If the create action throws an error, all of the child 'SessionExample' will report this error.
-- Once the final example is run, the cleanup action is executed with the current state (which is
-- the intial state passed through all successful examples).  An exception in the cleanup function
-- is ignored.
--
-- If you use 'parallel', the child examples in the session will still be executed serially in
-- depth-first order so that the state is processed through properly.  But multiple sessions will be
-- executed in parallel.
session :: Typeable s => IO s -- ^ create the state
                      -> (s -> IO ()) -- ^ cleanup the state
                      -> Spec -- ^ spec tree to process
                      -> Spec
session create close s = do
    (sess, trees) <- runIO $ do
        trees <- runSpecM s
        let cnt = countItems trees
        mvars <- replicateM cnt newEmptyMVar
        return (Session cnt mvars create close, trees)
    fromSpecList (mapWithCounter (sessionItem sess) trees)

-- | Create an example to pass to 'it' which accesses and modifies the state using the state monad.
-- For example,
--
-- >it "checks the state" $ runState $ do
-- >    s <- get
-- >    liftIO $ s `shouldBe` "Hello, World"
runState :: StateT s IO () -> SessionExample s
runState = SessionExample . execStateT

-- | Create an example to pass to 'it' which only reads the state.  This is useful for bracketing
-- tests with some resource.  For example,
--
-- >openDb :: IO DbConnection
-- >openDb = ...
-- >
-- >closeDb :: DbConnection -> IO ()
-- >closeDb = ...
-- >
-- >dbSessionWhich :: String -> Spec -> Spec
-- >dbSessionWhich msg = session openDb closeDb . describe msg
-- >
-- >spec :: Spec
-- >spec = 
-- >  ...
-- >  dbSessionWhich "checks users" $ do
-- >      it "adds a user" $ with $ \db -> do
-- >          ... something with db ...
-- >
-- >      it "loads the user" $ with $ \db -> do
-- >          ... something with db ...
with :: (s -> IO ()) -> SessionExample s
with f = SessionExample $ \a -> f a >> return a
