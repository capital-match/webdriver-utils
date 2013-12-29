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
-- >        it "checks hover text of 327" $ using Firefox $ do
-- >            openPage "http://www.xkcd.com/327/"
-- >            e <- findElem $ ByCSS "div#comic > img"
-- >            e `shouldBeTag` "img"
-- >            e `shouldHaveAttr` ("title", "Her daughter is named Help I'm trapped in a driver's license factory.")
-- >
-- >        parallel $ it "checks title of 303" $ using [Firefox, Chrome] $ do
-- >            openPage "http://www.xkcd.com/303/"
-- >            e <- findElem $ ById "ctitle"
-- >            e `shouldBeTag` "div"
-- >            e `shouldHaveText` "Compiling"
-- >
-- >        it "checks image of 1023" pending
--
-- The above code assumes selenium-server-standalone is running on @127.0.0.1:4444@ at path
-- @\/wd\/hub@ (this is the default).  You can configure this using `createSessionManager'`. 
module Test.Hspec.WebDriver(
  -- * Webdriver
    BrowserDefaults(..)
  , it
  , Using(..)
  , pending
  , pendingWith
  , WdExpectation(..)

  -- * Expectations
  , shouldBe
  , shouldBeTag
  , shouldHaveText
  , shouldHaveAttr
  , shouldReturn
  , shouldThrow

  -- * Session Manager
  , createSessionManager
  , createSessionManager'

  -- * Custom Capabilities
  , TestCapabilities(..)

  -- * Re-exports from hspec
  , hspec
  , Spec
  , describe
  , context
  , before
  , after
  , around
  , parallel

  -- * Re-exports from "Test.WebDriver"
  , WD
  , liftIO
  , module Test.WebDriver.Commands

  -- * Internal
  , withCaps
) where

import Control.Exception.Lifted (try, Exception)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (Typeable)
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec hiding (shouldReturn, shouldBe, shouldSatisfy, shouldThrow, it, pending, pendingWith)
import Test.Hspec.Core (Result(..), fromSpecList, SpecTree(..), Item(..), Params)
import Test.WebDriver hiding (Browser(..))
import Test.WebDriver.Commands
import qualified Test.WebDriver.Capabilities as W
import qualified Test.Hspec as H
import qualified Data.Text as T

import Test.Hspec.WebDriver.Internal

-- | Webdriver expectations consist of a set of browser 'Capabilities' to use and the actual test as
-- a 'WD' monad.  The browser capabilities are specified by an enumeration which is an instance of
-- 'TestCapabilities'.  The @BrowserDefaults@ enumeration provides items that represent the default set of
-- capabilities for each browser.  When creating new sessions, the 'defaultCaps' are used.  Also,
-- any existing session (which exists at program startup) which matches the browser is used, no
-- matter the actual capabilities.
--
-- To obtain more control over the capabilities (e.g. to test multiple versions of IE or to test
-- Firefrox without javascript), you should @import Test.Hspec.WebDriver hiding (BrowserDefaults)@
-- and then create your own enumeration which is an instance of 'TestCapabilities' and 'Using'.
data BrowserDefaults = Firefox | Chrome | IE | Opera | IPhone | IPad | Android | HTMLUnit
    deriving (Eq, Show, Enum, Bounded, Typeable)

instance TestCapabilities BrowserDefaults where
    matchesCaps Firefox (Capabilities { browser = W.Firefox _ _ _ }) = True
    matchesCaps Chrome (Capabilities { browser = W.Chrome _ _ _ _ }) = True
    matchesCaps IE (Capabilities { browser = W.IE _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ }) = True
    matchesCaps Opera (Capabilities { browser = W.Opera _ _ _ _ _ _ _ _ _ _ _ _ }) = True
    matchesCaps IPhone (Capabilities { browser = W.IPhone}) = True
    matchesCaps IPad (Capabilities { browser = W.IPad }) = True
    matchesCaps Android (Capabilities { browser = W.Android }) = True
    matchesCaps HTMLUnit (Capabilities { browser = W.HTMLUnit}) = True
    matchesCaps _ _ = False

    newCaps Firefox = return $ defaultCaps { browser = firefox }
    newCaps Chrome = return $ defaultCaps { browser = chrome }
    newCaps IE = return $ defaultCaps { browser = ie }
    newCaps Opera = return $ defaultCaps { browser = opera }
    newCaps IPhone = return $ defaultCaps { browser = iPhone }
    newCaps IPad = return $ defaultCaps { browser = iPad }
    newCaps Android = return $ defaultCaps { browser = android }
    newCaps HTMLUnit = return $ defaultCaps { browser = W.htmlUnit }

-- | A webdriver expectation is either an action and a list of capabilities (which should be an instance of
-- 'TestCapabilities') or a pending message.
data WdExpectation cap = WdTest [cap] (WD ())
                       | WdPending (Maybe String)

-- | Evaluate an expectation to a list of the function expected by hspec.
evaluateWd :: (Show cap, TestCapabilities cap) => WdExpectation cap -> [(String,Params -> (IO () -> IO ()) -> IO Result)]
evaluateWd (WdPending msg) = [("", \_ _ -> return $ Pending msg)]
evaluateWd (WdTest cs test) = map mkItem cs
    where
        mkItem c = ("using " ++ show c, eval c)

        eval :: TestCapabilities c => c -> params -> (IO () -> IO ()) -> IO Result
        eval c _ action = do action (runWD defaultSession $ withCaps c test)
                             return Success

-- | A typeclass of things which can be converted to a 'WdExpectation'.  This is the primary method
-- to create expectations to pass to 'it'.  Both a single 'BrowserDefaults' and a list
-- of 'BrowserDefaults' can be used.
--
-- >it "opens the home page" $ using Firefox $ do
-- >    ...
-- >it "opens the users page" $ using [Firefox, Chrome] $ do
-- >    ...
class Using a where
    type UsingCapabilities a :: *
    using :: a -> WD () -> WdExpectation (UsingCapabilities a)

instance Using BrowserDefaults where
    type UsingCapabilities BrowserDefaults = BrowserDefaults
    using d = WdTest [d]

instance Using [BrowserDefaults] where
    type UsingCapabilities [BrowserDefaults] = BrowserDefaults
    using = WdTest

-- | Create a spec from a webdriver expectation.
--
-- The webdriver expectation consists of a list of browser capabilities and the actual test.  The
-- test will be executed once for each set of capabilities in the list.  By default, the tests will be
-- executed serially.  You can use 'parallel' to execute the test on all capabilities in parallel.
--
-- To run the test, a webdriver session is allocated from a pool of sessions (in a thread-safe
-- manner).  The pools will be initialized automatically from existing sessions the first time a
-- test is run; you can explicitly create the pools using 'createSessionManager'.
it :: (Show cap, TestCapabilities cap) => String -> WdExpectation cap -> Spec
it msg a = spec
    where
        mkItem m f = SpecItem Item { itemIsParallelizable = False
                                   , itemRequirement = m
                                   , itemExample = f
                                   }
        spec = case evaluateWd a of
                [] -> fromSpecList []
                [("",f)] -> fromSpecList [mkItem (msg) f]
                [(m,f)] -> fromSpecList [mkItem (msg ++ " " ++ m) f]
                ss -> describe msg $ fromSpecList $ map (uncurry mkItem) ss

-- | A 'WdExpectation' that is pending.
pending :: WdExpectation ()
pending = WdPending Nothing

-- | A 'WdExpectation' that is pending, with a message.
pendingWith :: String -> WdExpectation ()
pendingWith = WdPending . Just

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
