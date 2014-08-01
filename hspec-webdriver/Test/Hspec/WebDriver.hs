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
  , sessionWith
  , runWD
  , inspectSession
  , Using(..)

  -- * Multiple sessions at once
  , multiSession
  , multiSessionWith
  , runWDWith
  , WDExample

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
  , runIO

  -- * Re-exports from "Test.WebDriver"
  , WD
  , liftIO
  , module Test.WebDriver.Commands
) where

import Control.Exception.Lifted (try, Exception, onException, throwIO, catch)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Typeable (Typeable)
import Test.HUnit (assertEqual, assertFailure)
import qualified Data.Text as T

import Test.Hspec hiding (shouldReturn, shouldBe, shouldSatisfy, shouldThrow)
import Test.Hspec.Core (Result(..), Item(..), mapSpecItem)
import qualified Test.Hspec as H

import Test.WebDriver (WD)
import Test.WebDriver.Commands
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Session as W
import qualified Test.WebDriver.Config as W

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
    newCaps :: c -> IO W.Capabilities

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
        create = createSt W.defaultConfig

-- | A variation of 'session' which allows you to specify the webdriver configuration.  Note that
-- the capabilities in the 'W.WDConfig' will be ignored, instead the capabilities will come from the
-- list of 'TestCapabilities'.
sessionWith :: TestCapabilities cap
            => W.WDConfig -> String -> ([cap], Spec) -> Spec
sessionWith config = hSessionWd create
    where
        create :: TestCapabilities cap => cap -> IO (WdState ())
        create = createSt config

-- | Allows testing multiple browser sessions at once.
--
-- The way this works is you create a type @a@ to index the sessions, pass an undefined value to
-- 'multiSession', and then use values of type @a@ with 'runWDWith' to identify which session the
-- example should run with.  The first time 'runWDWith' sees a value, a new session is created.  Note
-- that the examples are still run serially in depth-first order.
--
-- Note that in hspec1, the requirement that every example inside 'multiSession' must use 'runWDWith'
-- with the same type @a@ is not checked by types.  In <http://hackage.haskell.org/package/hspec2 hspec2>
-- the types are expressive enough so that this can be checked by the type system (and also means
-- 'multiSession' does not need the undefined value of type @a@).
--
-- I use this for testing multiple users at once, with one user in each browser session.
--
-- >data TestUser = Gandolf | Bilbo | Legolas
-- >    deriving (Show, Eq, Enum, Bounded, Typeable)
-- >
-- >usersSession :: TestCapabilities cap => String -> ([cap],Spec) -> Spec
-- >usersSession = multiSession (undefined :: TestUser)
-- >
-- >runUser :: TestUser -> WD () -> WDExample TestUser
-- >runUser = runWDWith
-- >
-- >spec :: Spec
-- >spec = usersSession "tests some page" $ using Firefox $ do
-- >    it "does something with Gandolf" $ runUser Gandolf $ do
-- >        openPage ...
-- >    it "does something with Bilbo" $ runUser Bilbo $ do
-- >        openPage ...
-- >    it "goes back to the Gandolf session" $ runUser Gandolf $ do
-- >        e <- findElem ....
-- >        ...
--
-- In the above code, two sessions are created and the examples will go back and forth between the
-- two sessions.  Note that a session for Legolas will only be created the first time he shows up in
-- a call to @runUser@.  To share information between the sessions (e.g. some data that Gandolf
-- creates that Bilbo should expect), the best way I have found is to use 'runIO' to create an
-- IORef while constructing the spec.  Note this can be hidden inside the @usersSession@ function.
multiSession :: (TestCapabilities cap, Typeable a, Eq a)
             => a -- ^ Can be an undefined value of type a, this is used only to determine the type
             -> String -- ^ the message
             -> ([cap], Spec) -- ^ the list of capabilites and the spec
             -> Spec
multiSession val = hSessionWd $ create val
    where
        create :: (Typeable a, Eq a, TestCapabilities cap) => a -> cap -> IO (WdState a)
        create _ = createSt W.defaultConfig

-- | A variation of 'multiSession' which allows you to specify the webdriver configuration.  Note that
-- the capabilities in the 'W.WDConfig' will be ignored, instead the capabilities will come from the
-- list of 'TestCapabilities'.
multiSessionWith :: (TestCapabilities cap, Typeable a, Eq a)
               => W.WDConfig
               -> a -- ^ Can be an undefined value of type a, this is used only to determine the type
               -> String -- ^ the message
               -> ([cap], Spec) -- ^ the list of capabilites and the spec
               -> Spec
multiSessionWith config val = hSessionWd $ create val
    where
        create :: (Typeable a, Eq a, TestCapabilities cap) => a -> cap -> IO (WdState a)
        create _ = createSt config

-- | A typeclass of things which can be converted to a list of capabilities.  It has two uses.
-- First, it allows you to create a datatype of grouped capabilities in addition to your actual
-- capabilities.  These psudo-caps can be passed to @using@ to convert them to a list of your actual
-- capabilities.  Secondly, it allows the word @using@ to be used with 'session' so that the session
-- description reads like a sentance.
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
   stInitialConfig :: W.WDConfig -- ^ initial config
 , stSessions :: [(a,W.WDSession)] -- ^ the webdriver sessions
 , stError :: IORef Bool    -- ^ has an error occured in an earlier example?  We rely on the serialization
                            --   of examples to ensure that at most one thread is reading/writing this
                            --   ioref.
} deriving Typeable

-- | Used to signal that a previous example had an error
data PrevHasError = PrevHasError
    deriving (Show, Typeable)
instance Exception PrevHasError

-- | Create the internal WdState
createSt :: TestCapabilities cap => W.WDConfig -> cap -> IO (WdState a)
createSt cfg cap = do
    err <- newIORef False
    caps <- newCaps cap
    return $ WdState cfg { W.wdCapabilities = caps} [] err 

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
            itemExample = \p a prog -> itemExample item p a prog
                                    `catch` \PrevHasError -> return $ Pending $ Just "previous example had error" }

-- | An example that can be passed to 'it' containing a webdriver action.  It must be created with
-- 'runWD' or 'runWDWith'.
newtype WDExample multi = WdExample (I.SessionExample (WdState multi))
    deriving Example

-- | Create an example from a 'WD' action.  This /must/ be nested inside a call to 'session' or
-- 'sessionWith'.
runWD :: WD () -> WDExample ()
runWD = runWDWith ()

-- | Create an example from a 'WD' action, parameterized by which session to run.
-- This /must/ be nested inside a call to 'multiSession' or 'multiSessionWith' and can only be used
-- when multiple sessions are running.  Also, the type @a@ must match the type given to
-- 'multiSession'.
runWDWith :: (Eq a, Typeable a) => a -> WD () -> WDExample a
runWDWith a w = WdExample $ I.SessionExample $ \state -> do
    err <- readIORef $ stError state
    when err $ throwIO PrevHasError

    sess <- case lookup a (stSessions state) of
                Just s -> return s
                Nothing -> do
                    let cfg = stInitialConfig state
                    s <- W.mkSession cfg
                    W.runWD s $ W.createSession $ W.wdCapabilities cfg

    W.runWD sess $ do
        w `onException` liftIO (writeIORef (stError state) True)
        swd <- W.getSession
        return state { stSessions = (a,swd) : filter ((/=a) . fst) (stSessions state) }
