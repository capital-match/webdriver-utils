{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveDataTypeable, TypeFamilies #-}
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
  , WdExample(..)
  , runWD
  , pending
  , pendingWith
  , example
  , session
  , sessionWith
  , Using(..)
  , WdTestSession
  , inspectSession

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
  , SpecWith
  , describe
  , it
  , context
  , parallel
  , runIO

  -- * Re-exports from "Test.WebDriver"
  , WD
  , module Test.WebDriver.Commands
) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception (SomeException(..))
import Control.Exception.Lifted (try, Exception, onException, throwIO)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (state, evalState, execState)
import Data.Typeable (Typeable, cast)
import Data.IORef
import Data.Traversable (traverse)
import Test.HUnit (assertEqual, assertFailure)
import qualified Data.Text as T

import Test.Hspec hiding (shouldReturn, shouldBe, shouldSatisfy, shouldThrow, pending, pendingWith, example)
import Test.Hspec.Core (Result(..), Item(..), Example(..), SpecTree(..), fromSpecList, runSpecM)
import qualified Test.Hspec as H

import Test.WebDriver (WD)
import Test.WebDriver.Commands
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Session as W
import qualified Test.WebDriver.Config as W

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

-- | Internal state for a webdriver test session.
data WdTestSession = WdTestSession {
    wdTestOpen :: IO (Maybe W.WDSession, Bool)
        -- ^ action to obtain the session.  The boolean is True if a previous example has an error.
        -- Note that if a previous example has an error, the session is still passed along since it
        -- must be eventually closed.  A nothing for the session is only used if the actual session
        -- open procedure throws an error.
  , wdTestClose :: (Maybe W.WDSession, Bool) -> IO ()
        -- ^ once the test is done with the session, it should pass the new session on to the close
        -- method.  Again the boolean is True if either a previous or this example has an error.
}

-- | A webdriver example.
--
-- The webdriver action of type @'WD' ()@ should interact with the webpage using commands from
-- "Test.WebDriver.Commands" (which is re-exported from this module) and then use the <#g:2 expectations>
-- in this module.  It is possible to split up the spec of a single page into multiple
-- examples where later examples start with the web browser state from the end of the previous
-- example.  This is helpful to keep each individual example small and allows the entire spec to be
-- described at the beginning with pending examples.  
--
-- The way this works is that you combine examples into a session using 'session', 'sessionWith',
-- 'multiSession', and 'multiSessionWith'.  A webdriver session is then threaded through all
-- examples in a session so that a later example in the session can rely on the webbrowser state as
-- set up by the previous example.  The type system enforces that every webdriver example must be
-- located within a call to 'session' or 'sessionOn'.  Indeed, a 'WdExample' produces a @'SpecWith'
-- 'WdTestSession'@ which can only be converted to 'Spec' using 'session' or 'sessionOn'.  The
-- reason for the 'WdPending' constructor is so that a pending example can be specified with type
-- @'SpecWith' 'WdTestSession'@ so it can compose with the other webdriver examples.
data WdExample = WdExample (WD ())
               | WdPending (Maybe String)

-- | A shorthand for constructing a 'WdExample' from a webdriver action.
runWD :: WD () -> WdExample
runWD = WdExample

-- | A pending example.
pending :: WdExample
pending = WdPending Nothing

-- | A pending example with a message.
pendingWith :: String -> WdExample
pendingWith = WdPending . Just

-- | A version of 'H.example' which lifts an @IO ()@ to a webdriver example (so it can be composed
-- with other webdriver examples).
example :: Expectation -> WdExample
example = WdExample . liftIO

-- | Combine the examples nested inside this call into a webdriver session.  All the examples are
-- run once for each capability in the list. Before the first example is executed, a new webdriver
-- session is automatically created using the capabilities.  The examples are then executed in
-- depth-first order using this webdriver session (so later examples can rely on the browser state
-- created by earlier examples).  Once the final example has executed, the session is automatically
-- closed.  If some example fails (throws an exception), all remaining examples in the session will
-- become pending.
--
-- Note that when using 'parallel', the examples within a single session will still execute
-- serially.  Different sessions (including the multiple sessions created if more than one
-- capability is passed to 'session') will be executed in parallel.
--
-- This function uses the default webdriver host (127.0.0.1), port (4444), and basepath
-- (@\/wd\/hub@).
session :: TestCapabilities cap => String -> ([cap], SpecWith WdTestSession) -> Spec
session = procTestsWithCaps W.defaultConfig

-- | A variation of 'session' which allows you to specify the webdriver configuration.  Note that
-- the capabilities in the 'W.WDConfig' will be ignored, instead the capabilities will come from the
-- list of 'TestCapabilities'.
sessionWith :: TestCapabilities cap => W.WDConfig -> String -> ([cap], SpecWith WdTestSession) -> Spec
sessionWith = procTestsWithCaps

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
    using :: a -> SpecWith WdTestSession -> (UsingList a, SpecWith WdTestSession)

instance Using BrowserDefaults where
    type UsingList BrowserDefaults = [BrowserDefaults]
    using d s = ([d], s)

instance Using [BrowserDefaults] where
    type UsingList [BrowserDefaults] = [BrowserDefaults]
    using d s = (d, s)

data AbortSession = AbortSession
    deriving (Show, Typeable)
instance Exception AbortSession

-- | Abort the session without closing the session.
--
-- Normally, 'session' will automatically close the session either when the tests complete without
-- error or when any of the tests within the session throws an error.  When developing the test
-- suite, this can be annoying since closing the session causes the browser window to close.
-- Therefore, while developing the test suite, you can insert a call to 'inspectSession'.  This will
-- immedietly halt the session (all later tests will fail) but will not close the session so that
-- the browser window stays open.
inspectSession :: WD ()
inspectSession = throwIO AbortSession


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

-- | Create a WdTestSession.
createTestSession :: TestCapabilities cap
                  => W.WDConfig -> cap -> [MVar (Maybe W.WDSession,Bool)] -> Int -> WdTestSession
createTestSession cfg cap mvars n = WdTestSession open close
    where
        open | n == 0 = do c <- newCaps cap
                           s <- W.mkSession cfg {W.wdCapabilities = c}
                           s' <- W.runWD s $ createSession c
                           return (Just s', False)
             | otherwise = takeMVar (mvars !! n)

        close (sess,err) | length mvars - 1 == n = maybe (return ()) (`W.runWD` closeSession) sess
                         | otherwise = putMVar (mvars !! (n + 1)) (sess, err)

-- | Convert a single test item to a generic item by providing it with the WdTestSession.
procSpecItem :: TestCapabilities cap
             => W.WDConfig -> cap -> [MVar (Maybe W.WDSession, Bool)] -> Int -> Item WdTestSession -> Item ()
procSpecItem cfg mvars caps n item = item { itemExample = \p act progress -> itemExample item p (act . act') progress }
    where
        act' f () = f (createTestSession cfg mvars caps n)

-- | Convert a spec tree of test items to a spec tree of generic items by creating a single session for 
-- the entire tree.
procTestSession :: TestCapabilities cap
                => W.WDConfig -> cap -> SpecWith WdTestSession -> Spec
procTestSession cfg c s = fromSpecList [build]
    where
        build = BuildSpecs $ do
            trees <- forceSpec s
            let cnt = countItems trees
            mvars <- replicateM cnt newEmptyMVar
            return $ mapWithCounter (procSpecItem cfg c mvars) trees

-- | Convert a list of capabilites and a spec tree of test items to a generic spec tree by creating
-- one session per capability.
procTestsWithCaps :: TestCapabilities cap => W.WDConfig -> String -> ([cap], SpecWith WdTestSession) -> Spec
procTestsWithCaps cfg msg (caps, spec) = spec'
    where
        spec' = case caps of
                    [] -> it msg $ H.pendingWith "No capabilities specified"
                    [c] -> describe (msg ++ " using " ++ show c) $ procTestSession cfg c spec
                    _ -> describe msg $ mapM_ (\c -> describe ("using " ++ show c) $ procTestSession cfg c spec) caps


instance Example WdExample where
    type Arg WdExample = WdTestSession
    evaluateExample (WdPending msg) _ _ _ = return $ Pending msg
    evaluateExample (WdExample wd) _ act _ = do
        prevHadError <- newIORef False

        act $ \testsession -> do

            (msess, err) <- wdTestOpen testsession
                                    `onException` wdTestClose testsession (Nothing, True)

            case msess of
                (Just wdsession) | not err -> W.runWD wdsession $ do
                    -- run the example
                    macterr <- try wd
                    case macterr of
                        Right () -> do
                            -- pass current session on to the next test
                            wdsession' <- W.getSession
                            liftIO $ wdTestClose testsession (Just wdsession', False)
                        Left acterr@(SomeException actex) ->
                            case cast actex of
                                -- pass nothing on to the next test so the session is not closed
                                Just AbortSession -> liftIO (wdTestClose testsession (Nothing, True)) >> throwIO AbortSession
                                -- pass the original session on to the next session and rethrow the error
                                Nothing -> liftIO (wdTestClose testsession (Just wdsession, True)) >> throwIO acterr


                _ -> do
                    -- on error, just pass along the session and error
                    writeIORef prevHadError True
                    wdTestClose testsession (msess, err)

        merr <- readIORef prevHadError
        return $ if merr then Pending (Just "Previous example had an error") else Success


--------------------------------------------------------------------------------
--- Utils
--------------------------------------------------------------------------------

-- | Force a spec tree to not contain any BuildSpecs
forceSpecTree :: SpecTree a -> IO [SpecTree a]
forceSpecTree s@(SpecItem _ _) = return [s]
forceSpecTree (SpecGroup msg ss) = do
    ss' <- concat <$> mapM forceSpecTree ss
    return [SpecGroup msg ss']
forceSpecTree (BuildSpecs m) = do
    trees <- m
    concat <$> mapM forceSpecTree trees

-- | Force a spec to not contain any BuildSpecs
forceSpec :: SpecWith a -> IO [SpecTree a]
forceSpec s = do
    trees <- runSpecM s
    concat <$> mapM forceSpecTree trees

-- | Traverse a spec, but only if forceSpec has already been called
traverseTree :: Applicative f => (Item a -> f (Item b)) -> SpecTree a -> f (SpecTree b)
traverseTree f (SpecItem msg i) = SpecItem msg <$> f i
traverseTree f (SpecGroup msg ss) = SpecGroup msg <$> traverse (traverseTree f) ss
traverseTree _ (BuildSpecs _) = error "No BuildSpecs should be left"

-- | Traverse a list of specs, but only if forceSpecs has already been called
traverseSpec :: Applicative f => (Item a -> f (Item b)) -> [SpecTree a] -> f [SpecTree b]
traverseSpec f = traverse (traverseTree f)

-- | Process the items in a depth-first walk, passing in the item counter value.
mapWithCounter :: (Int -> Item a -> Item b) -> [SpecTree a] -> [SpecTree b]
mapWithCounter f s = flip evalState 0 $ traverseSpec go s
    where
        go item = state $ \cnt -> (f cnt item, cnt+1)

countItems :: [SpecTree a] -> Int
countItems s = flip execState 0 $ traverseSpec go s
    where
        go item = state $ \cnt -> (item, cnt+1)
