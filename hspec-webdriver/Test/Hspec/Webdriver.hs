{-# LANGUAGE OverloadedStrings #-}
-- | Write hspec tests that are webdriver tests, automatically managing the webdriver sessions.
--
-- This module re-exports functions from "Test.Hspec" and it is intended that you just import
-- @Test.Hspec.Webdriver@ and not @Test.Hspec@.  If you need to import @Test.Hspec@, you should do
-- so using a qualified import.
module Test.Hspec.Webdriver(
  -- * Webdriver
    WdSessions(..)
  , WithSession(..)
  , itRunsWithSessions

  -- * Expectations
  , shouldBe
  , shouldBeTag
  , shouldHaveText
  , shouldHaveAttr
  , shouldReturn
  , shouldThrow

  -- * Re-exports from hspec
  , hspec
  , Spec
  , Example
  , describe
  , context
  , it
  , pending
  , pendingWith
  , before
  , after
  , around
) where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Control.Exception.Lifted (try, Exception)
import Test.Hspec.Core (Example(..), Result(..), fromSpecList, SpecTree(..), Item(..))
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec hiding (shouldReturn, shouldBe, shouldSatisfy, shouldThrow)
import Test.WebDriver
import Test.WebDriver.Classes
import qualified Test.Hspec as H
import qualified Data.Text as T

-- | Provides information about your session enumeration.
--
-- To use this module, you must make an enumeration of all the webdriver sessions you will be
-- testing with.  For example,
--
-- >data TestSessions = Firefox
-- >                  | FirefoxWithoutJavascript
-- >                  | Chrome
-- >   deriving (Show, Eq, Bounded, Enum)
--
-- This enumeration must then be made an instance of this class.
class WdSessions s where

    -- | Check if the 'Capabilities' match your session.  Note that these capabilities will be the
    -- actual capabilities (with things like version information filled in) so you should not use
    -- @==@ to compare capabilities.  For example, to match Chrome you could use
    --
    -- > sessMatchCaps Chrome (Capabilities { browser = Chrome _ _ _ _}) = return True
    sessMatchesCaps :: s -> Capabilities -> WD Bool

    -- | The capabilities to pass to 'createSession' when no existing session is found.
    newCaps :: s -> WD Capabilities

-- | Use this to create a test for a single session.  It converts a session type and a 'WD' action
-- into a hspec 'Example' which can be passed to 'it'.
--
-- >myspecs :: Spec
-- >myspecs = describe "home page" $ do
-- >              it "loads the page" $ WithSession Firefox $ do
-- >                  elem <- findElem $ ByCSS "div#main"
-- >                  elem `shouldHaveText` "Hello, World!"
--
-- Before running the actual test, all existing sessions are loaded.  If any match the session type
-- (checked using 'sessMatchesCaps'), that session is used.  If no existing session matches, a new
-- session is created.  This is currently not thread-safe so the tests must be run serially.
data WithSession s = WithSession s (WD ())

instance WdSessions s => Example (WithSession s) where
    evaluateExample (WithSession stype w) _ action = do
        action (runWD defaultSession $ findSession stype w)
        return Success

-- | Find or create a session
findSession :: WdSessions s => s -> WD a -> WD a
findSession stype test = do
    ss <- sessions
    ss' <- filterM (\(_, caps) -> sessMatchesCaps stype caps) ss
    case ss' of
        ((s, _):_) -> putSession $ defaultSession { wdSessId = Just s }
        _ -> do caps <- newCaps stype
                s <- createSession caps
                putSession s
    test

-- | Use this to run a single test on multiple sessions.
--
-- >myspecs :: Spec
-- >myspecs = describe "home page" $ do
-- >              itRunsWithSessions [Firefox, Chrome] "loads the page" $ do
-- >                  elem <- findElem $ ByCSS "div#main"
-- >                  elem `shouldHaveText` "Hello, World!"
--
-- I also suggest you add a definition like the following
--
-- >itRunsWithAllSessionsAnd :: String -> WD () -> Spec
-- >itRunsWithAllSessionsAnd = itRunsWithSessions [(minBound::TestSessions)..maxBound]
itRunsWithSessions :: (WdSessions s, Show s) => [s] -> String -> WD () -> Spec
itRunsWithSessions ss msg test = describe msg $ fromSpecList $ map mkSpec ss
    where
        mkSpec stype = SpecItem $ Item
                        { itemIsParallelizable = False
                        , itemRequirement = "with " ++ show stype
                        , itemExample = \_ action -> do
                            action (runWD defaultSession $ findSession stype test)
                            return Success
                        }

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
