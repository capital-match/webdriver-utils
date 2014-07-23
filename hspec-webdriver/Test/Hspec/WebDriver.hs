{-# LANGUAGE CPP, OverloadedStrings, FlexibleInstances, DeriveDataTypeable, TypeFamilies, GeneralizedNewtypeDeriving #-}
-- | Write hspec tests that are webdriver tests, automatically managing the webdriver sessions.
--
-- This module re-exports functions from "Test.Hspec" and "Test.WebDriver.Commands" and it is
-- intended that you just import @Test.Hspec.WebDriver@.  If you need to import @Test.Hspec@ or
-- @Test.WebDriver@, you should do so using a qualified import.
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >module XKCD where
-- >
-- >import Test.Hspec.WebDriver
-- >
-- >main :: IO ()
-- >main = hspec $
-- >    describe "XKCD Tests" $ do
-- >
-- >        session "for 327" $ using Firefox $ do
-- >            it "opens the page" $ runWD $
-- >                openPage "http://www.xkcd.com/327/"
-- >            it "checks hover text" $ runWD $ do
-- >                e <- findElem $ ByCSS "div#comic > img"
-- >                e `shouldBeTag` "img"
-- >                e `shouldHaveAttr` ("title", "Her daughter is named Help I'm trapped in a driver's license factory.")
-- >
-- >        parallel $ session "for 303" $ using [Firefox, Chrome] $ do
-- >            it "opens the page" $ runWD $
-- >                openPage "http://www.xkcd.com/303/"
-- >            it "checks the title" $ runWD $ do
-- >                e <- findElem $ ById "ctitle"
-- >                e `shouldBeTag` "div"
-- >                e `shouldHaveText` "Compiling"
--
-- The above code assumes selenium-server-standalone is running on @127.0.0.1:4444@ at path
-- @\/wd\/hub@ (this is the default).
module Test.Hspec.WebDriver(
  -- * Webdriver
    BrowserDefaults(..)
  , session
  , sessionOn
  , runWD
  , WDExample
  , Using(..)
  , inspectSession

  -- * Multiple sessions at once
  , multiSession
  , multiSessionOn
  , runWDWith

  -- * Expectations
  , shouldBe
  , shouldBeTag
  , shouldHaveText
  , shouldHaveAttr
  , shouldReturn
  , shouldThrow

  -- * Custom Capabilities
  , TestCapabilities(..)

  -- * Re-exports from "Test.Hspec"
  , hspec
  , Spec
  , describe
  , it
  , context
  , parallel
  , pending
  , pendingWith

  -- * Re-exports from "Test.WebDriver"
  , WD
  , liftIO
  , module Test.WebDriver.Commands
) where

import Control.Exception.Lifted (try, Exception, onException, throwIO, catch)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.IORef
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Test.HUnit (assertEqual, assertFailure)
import qualified Data.Text as T

import Test.Hspec hiding (shouldReturn, shouldBe, shouldSatisfy, shouldThrow)
import Test.Hspec.Core (Result(..), Item(..), mapSpecItem)
import qualified Test.Hspec as H

import Test.WebDriver (WD)
import Test.WebDriver.Commands
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Classes as W

import qualified Test.Hspec.WebDriver.Internal as I

-- | Webdriver expectations consist of a set of browser 'W.Capabilities' to use and the actual test as
-- a 'WD' monad.  The browser capabilities are specified by an enumeration which is an instance of
-- 'TestCapabilities'.  The @BrowserDefaults@ enumeration provides items that represent the default set of
-- capabilities for each browser (see 'W.defaultCaps').
--
-- To obtain more control over the capabilities (e.g. to test multiple versions of IE or to test
-- Firefrox without javascript), you should @import Test.Hspec.WebDriver hiding (BrowserDefaults)@
-- and then create your own enumeration which is an instance of 'TestCapabilities' and 'Using'.
data BrowserDefaults = Firefox | Chrome | IE | Opera | IPhone | IPad | Android
    deriving (Eq, Show, Enum, Bounded)

-- | Provides information about the browser capabilities used for testing.  If you want more control
-- over capabilities, you should hide 'BrowserDefaults' and then make an enumeration of all the
-- webdriver capabilities you will be testing with.  For example,
--
-- >data TestCaps = Firefox
-- >              | FirefoxWithoutJavascript
-- >              | Chrome
-- >              | IE8
-- >              | IE9
-- >   deriving (Show, Eq, Bounded, Enum)
--
-- @TestCaps@ must then be made an instance of @TestCapabilities@.  Also, instances of @Using@
-- should be created.
class Show c => TestCapabilities c where
    -- | The capabilities to pass to 'createSession'.
    newCaps :: c -> WD W.Capabilities

instance TestCapabilities BrowserDefaults where
    newCaps Firefox = return $ W.defaultCaps { W.browser = W.firefox }
    newCaps Chrome = return $ W.defaultCaps { W.browser = W.chrome }
    newCaps IE = return $ W.defaultCaps { W.browser = W.ie }
    newCaps Opera = return $ W.defaultCaps { W.browser = W.opera }
    newCaps IPhone = return $ W.defaultCaps { W.browser = W.iPhone }
    newCaps IPad = return $ W.defaultCaps { W.browser = W.iPad }
    newCaps Android = return $ W.defaultCaps { W.browser = W.android }

-- | Combine the examples nested inside this call into a webdriver session.  For each capability in
-- the list, before the first example is executed, a new webdriver session is created using the
-- capabilities.  The examples are then executed in depth-first order using this webdriver session
-- (so later examples can rely on the browser state created by earlier examples).  Once the final
-- example has executed, the session is closed.  If some 'WDExample' fails (throws an exception),
-- all remaining examples in the session will become pending.
--
-- Note that when using 'parallel', the examples within a single session will still execute
-- serially.  Different sessions (including the multiple sessions created if more than one
-- capability is passed to 'session') will be executed in parallel.
--
-- This function uses the default webdriver host (127.0.0.1), port (4444), and
-- basepath (@\/wd\/hub@).
session :: TestCapabilities cap => String -> ([cap], Spec) -> Spec
session = hSessionWd create
    where
        create :: TestCapabilities cap => cap -> IO (WdState ())
        create = createSt W.defaultSession

-- | A variation of 'session' which allows you to specify the webdriver host, port, and basepath.
sessionOn :: TestCapabilities cap
          => String -- ^ host
          -> Word16 -- ^ port
          -> String -- ^ base path
          -> String -- ^ message
          -> ([cap], Spec)
          -> Spec
sessionOn host port bp = hSessionWd create
    where
        create :: TestCapabilities cap => cap -> IO (WdState ())
        create = createSt def { W.wdHost = host
                              , W.wdPort = port
                              , W.wdBasePath = bp
                              }

multiSession :: (TestCapabilities cap, Typeable a, Eq a)
             => a -- ^ Can be an undefined value of type a, this is used only to determine the type
             -> String -- ^ the message
             -> ([cap], Spec) -- ^ the list of capabilites and the spec
             -> Spec
multiSession val = hSessionWd $ create val
    where
        create :: (Typeable a, Eq a, TestCapabilities cap) => a -> cap -> IO (WdState a)
        create _ = createSt W.defaultSession

-- | A variation of 'session' which allows you to specify the webdriver host, port, and basepath.
multiSessionOn :: (TestCapabilities cap, Typeable a, Eq a)
               => String -- ^ host
               -> Word16 -- ^ port
               -> String -- ^ base path
               -> a -- ^ Can be an undefined value of type a, this is used only to determine the type
               -> String -- ^ the message
               -> ([cap], Spec) -- ^ the list of capabilites and the spec
               -> Spec
multiSessionOn host port bp val = hSessionWd $ create val
    where
        create :: (Typeable a, Eq a, TestCapabilities cap) => a -> cap -> IO (WdState a)
        create _ = createSt def { W.wdHost = host
                                , W.wdPort = port
                                , W.wdBasePath = bp
                                }

-- | A typeclass of things which can be converted to a list of capabilities.  It's primary purpose
-- is to allow the word @using@ to be used with 'session' so that the session description reads like
-- a sentance.
--
-- >session "for the home page" $ using Firefox $ do
-- >    it "loads the page" $ runWD $ do
-- >        ...
-- >    it "scrolls the carosel" $ runWD $ do
-- >        ...
-- >session "for the users page" $ using [Firefox, Chrome] $ do
-- >    ...
class Using a where
    type UsingList a
    using :: a -> Spec -> (UsingList a, Spec)

instance Using BrowserDefaults where
    type UsingList BrowserDefaults = [BrowserDefaults]
    using d s = ([d], s)

instance Using [BrowserDefaults] where
    type UsingList [BrowserDefaults] = [BrowserDefaults]
    using d s = (d, s)

-- | Abort the session without closing the session.
--
-- Normally, 'session' will automatically close the session either when the tests complete without
-- error or when any of the tests within the session throws an error.  When developing the test
-- suite, this can be annoying since closing the session causes the browser window to close.
-- Therefore, while developing the test suite, you can insert a call to 'inspectSession'.  This will
-- immedietly halt the session (all later tests will fail) but will not close the session so that
-- the browser window stays open.
inspectSession :: WD ()
inspectSession = throwIO I.AbortSessionEx

-- | 'H.shouldBe' lifted into the 'WD' monad.
shouldBe :: (Show a, Eq a) => a -> a -> WD ()
x `shouldBe` y = liftIO $ x `H.shouldBe` y

-- | Asserts that the given element matches the given tag.
shouldBeTag :: Element -> T.Text -> WD ()
e `shouldBeTag` name = do
    t <- tagName e
    liftIO $ assertEqual ("tag of " ++ show e) name t

-- | Asserts that the given element has the given text.
shouldHaveText :: Element -> T.Text -> WD ()
e `shouldHaveText` txt = do
    t <- getText e
    liftIO $ assertEqual ("text of " ++ show e) txt t

-- | Asserts that the given elemnt has the attribute given by @(attr name, value)@.
shouldHaveAttr :: Element -> (T.Text, T.Text) -> WD ()
e `shouldHaveAttr` (a, txt) = do
    t <- attr e a
    liftIO $ assertEqual ("attribute " ++ T.unpack a ++ " of " ++ show e) (Just txt) t

-- | Asserts that the action returns the expected result.
shouldReturn :: (Show a, Eq a) => WD a -> a -> WD ()
action `shouldReturn` expected = action >>= (\a -> liftIO $ a `H.shouldBe` expected)

-- | Asserts that the action throws an exception.
shouldThrow :: (Show e, Eq e, Exception e) => WD a -> e -> WD ()
shouldThrow w expected = do
    r <- try w
    case r of
        Left err -> err `shouldBe` expected
        Right _ -> liftIO $ assertFailure $ "did not get expected exception " ++ show expected


--------------------------------------------------------------------------------
-- Internal Test Runner
--------------------------------------------------------------------------------

-- | State passed between examples
data WdState a = WdState {
   stInitialSession :: W.WDSession -- ^ initial session used only for host, port, and basepath
 , stCaps :: WD W.Capabilities -- ^ the call to newCaps to create the capabilities for this session
 , stSessions :: [(a,W.WDSession)] -- ^ the webdriver sessions
 , stError :: IORef Bool    -- ^ has an error occured in an earlier example?  We rely on the serialization
                            --   of examples to ensure that at most one thread is reading/writing this
                            --   ioref.
} deriving Typeable

-- | Used to signal that a previous example had an error
data PrevHasError = PrevHasError
    deriving (Show, Typeable)
instance Exception PrevHasError

-- | The initial session is used only for its host, port, and basepath.  A new session is created.
createSt :: TestCapabilities cap => W.WDSession -> cap -> IO (WdState a)
createSt initial cap = do
    err <- newIORef False
    return $ WdState initial (newCaps cap) [] err 

closeSt :: WdState a -> IO ()
closeSt st = 
    forM_ (stSessions st) $ \(_, sess) ->
        W.runWD sess closeSession

-- | The 'WDSession' passed in is used for its host, port, and base path.
hSessionWd :: (Typeable a, TestCapabilities cap) => (cap -> IO (WdState a)) -> String -> ([cap], Spec) -> Spec
hSessionWd create msg (caps, spec) = spec'
    where
        spec' = case caps of
                    [] -> it msg $ pendingWith "No capabilities specified"
                    [c] -> describe (msg ++ " using " ++ show c) $ proc c spec
                    _ -> describe msg $ mapM_ (\c -> describe ("using " ++ show c) $ proc c spec) caps

        proc cap = mapSpecItem addCatchResult . I.session (create cap) closeSt

        addCatchResult item = item {
#if MIN_VERSION_hspec(1,10,0)
            itemExample = \p a prog -> itemExample item p a prog
#else
            itemExample = \p a -> itemExample item p a
#endif
                                    `catch` \PrevHasError -> return $ Pending $ Just "previous example had error" }

-- | An example that can be passed to 'it' containing a webdriver action.  It must be created with
-- 'runWD' or 'runWDWith'.
newtype WDExample multi = WdExample (I.SessionExample (WdState multi))
    deriving Example

-- | Create an example from a 'WD' action.  This /must/ be nested inside a call to 'session' or
-- 'sessionOn' and can only be used when only a single session is running.
runWD :: WD () -> WDExample ()
runWD = runWDWith ()

-- | Create an example from a 'WD' action, parameterized by which session to run.
-- This /must/ be nested inside a call to 'multiSession' or 'multiSessionOn' and can only be used
-- when multiple sessions are running.
runWDWith :: (Eq a, Typeable a) => a -> WD () -> WDExample a
runWDWith a w = WdExample $ I.SessionExample $ \state -> do
    err <- readIORef $ stError state
    when err $ throwIO PrevHasError

    sess <- case lookup a (stSessions state) of
                Just s -> return s
                Nothing -> W.runWD (stInitialSession state) $ stCaps state >>= createSession

    W.runWD sess $ do
        w `onException` liftIO (writeIORef (stError state) True)
        swd <- W.getSession
        return state { stSessions = (a,swd) : (filter ((/=a) . fst) $ stSessions state) }
