{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveDataTypeable #-}
-- | This module exposes <https://hackage.haskell.org/package/webdriver webdriver> actions that can
-- be used to interact with a page which uses <http://angularjs.org/ AngularJs>.  This provides
-- similar functionality as <https://github.com/angular/protractor protractor> and in fact we share
-- some code with protractor.
module Test.WebDriver.Commands.Angular (
    -- * Loading
      waitForAngular

    -- * Searching for elements
    , NgException(..)
    , NgSelector(..)
    , findNg
    , findNgs
    , findNgFrom
    , findNgsFrom
    , NgRepeater(..)
    , findRepeater
    , findRepeaters
    , findRepeaterFrom
    , findRepeatersFrom

    -- * Misc
    , ngEvaluate
    , getLocationAbsUrl
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception (throwIO, Exception)
import Data.Typeable (Typeable)
import Test.WebDriver.Classes
import Test.WebDriver.Commands
import Test.WebDriver.JSON (fromJSON')
import Test.WebDriver.Commands.Internal (clientScripts)
import Language.Haskell.TH (runIO, litE, stringL)
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

-- | Map of the clientsidescripts for angular
cs :: M.HashMap T.Text T.Text
cs = either (\err -> error $ "Error parsing scripts " ++ err) id mhash
    where
        mhash = clientScripts j
        j = $(runIO (readFile "js/angular-clientsidescripts.js") >>= litE . stringL)

execCS :: (WebDriver wd, A.FromJSON a) => T.Text -> [JSArg] -> wd a
execCS script arg = executeJS arg body
    where
        body = maybe (error $ "Unable to find " ++ T.unpack script) id $ M.lookup script cs

-- | Variant of execCS that fails properly on Null
execElems :: WebDriver wd => T.Text -> [JSArg] -> wd [Element]
execElems script arg = do
    x <- execCS script arg
    case (x, A.fromJSON x) of
        (A.Null, _) -> return []
        (A.Array _, A.Success [A.Null]) -> return []
        _ -> fromJSON' x

asyncCS :: (WebDriver wd, A.FromJSON a) => T.Text -> [JSArg] -> wd (Maybe a)
asyncCS script arg = asyncJS arg body
    where
        body = maybe (error $ "Unable to find " ++ T.unpack script) id $ M.lookup script cs

-- | Wait until Angular has finished rendering before continuing.  @False@ indicates the timeout
-- was hit (see 'setScriptTimeout') and we stopped waiting and @True@ means that angular has
-- finished rendering.
waitForAngular :: WebDriver wd => T.Text -- ^ CSS selector to element which has ng-app
                               -> wd Bool
waitForAngular sel = do
    a <- asyncCS "waitForAngular" [JSArg sel] :: WebDriver wd => wd (Maybe ())
    return $ maybe False (const True) a

-- | Exceptions of this type will be thrown when an element is unable to be located.
data NgException = NgException String
  deriving (Show, Eq, Typeable)
instance Exception NgException

checkOne :: (Show s, MonadIO wd, WebDriver wd) => s -> [Element] -> wd Element
checkOne _ [e] = return e
checkOne sel es = liftIO $ throwIO err
    where
        err = NgException $ "Selector " ++ show sel ++ " returned " ++ show es

data NgSelector = 
    ByBinding T.Text -- ^ Argument is the binding, e.g. {{dog.name}}
  | ByModel T.Text   -- ^ Argument is the model name.  Searches for elements with the @ng-model=\"name\"@ attribute.
  | BySelectedOption T.Text -- ^ Argument is a model name. Searches for selected options within a select element
                            --   matching the modelname.  That is, the @\<option:checked\>@ elements within a 
                            --   @\<select ng-model=\"name\" ... \>@.
  deriving (Show,Eq)

data NgRepeater =
    ByRows T.Text    -- ^ All the rows matching the repeater (e.g. 'dog in dogs')
  | ByRow T.Text Int -- ^ A single row specified by the text of the repeater (e.g. 'dog in dogs') and the row index
  | ByColumn T.Text T.Text -- ^ A single column matching the text of the repeater (e.g. 'dog in dogs') and the
                         -- column binding (e.g '{{dog.name}}').
  | ByRowAndCol T.Text Int T.Text -- ^ A single row and column, given (repeater, row index, column binding).
  deriving (Show,Eq)

-- | Find a single element from the document matching the given Angular selector.  If zero or more
-- than one element is returned, an exception of type 'NgException' is thrown.
findNg :: (MonadIO wd, WebDriver wd) => NgSelector -> wd Element
findNg s = checkOne s =<< findNg' (JSArg A.Null) s

-- | Find elements from the document matching the given Angular selector.
findNgs :: WebDriver wd => NgSelector -> wd [Element]
findNgs = findNg' $ JSArg A.Null

-- | Find a single element from within the given element which matches the given Angular selector. If
-- zero or more than one element is returned, an exception of type 'NgException' is thrown.
findNgFrom :: (MonadIO wd, WebDriver wd) => Element -> NgSelector -> wd Element
findNgFrom e s = checkOne s =<< findNg' (JSArg e) s

-- | Find elements from within the given element which match the given Angular selector.
findNgsFrom :: WebDriver wd => Element -> NgSelector -> wd [Element]
findNgsFrom e = findNg' $ JSArg e

findNg' :: WebDriver wd => JSArg -> NgSelector -> wd [Element]
findNg' e (ByBinding name) = execElems "findBindings" [e, JSArg name]
findNg' e (ByModel name) = execElems "findByModel" [e, JSArg name]
findNg' e (BySelectedOption name) = execElems "findSelectedOptions" [e, JSArg name]

-- | Find an element from the document which matches the Angular repeater.  If zero or more than one
-- element are returned, an exception of type 'NgException' is thrown.
findRepeater :: (MonadIO wd, WebDriver wd) => NgRepeater -> wd Element
findRepeater r = checkOne r =<< findRepeater' (JSArg A.Null) r

-- | Find elements from the document which match the Angular repeater.
findRepeaters :: WebDriver wd => NgRepeater -> wd [Element]
findRepeaters = findRepeater' $ JSArg A.Null

-- | Find an element from the given element which matches the Angular repeater.  If zero or more than
-- one are returned, an exception of type 'NgException' is thrown.
findRepeaterFrom :: (MonadIO wd, WebDriver wd) => Element -> NgRepeater -> wd Element
findRepeaterFrom e r = checkOne r =<< findRepeater' (JSArg e) r

-- | Find elements from the given element which match the Angular repeater.
findRepeatersFrom :: WebDriver wd => Element -> NgRepeater -> wd [Element]
findRepeatersFrom e = findRepeater' $ JSArg e

findRepeater' :: WebDriver wd => JSArg -> NgRepeater -> wd [Element]
findRepeater' e (ByRows rep) = execElems "findAllRepeaterRows" [e, JSArg rep]
findRepeater' e (ByRow rep idx) = execElems "findRepeaterRows" [e, JSArg rep, JSArg idx]
findRepeater' e (ByColumn rep idx) = execElems "findRepeaterColumn" [e, JSArg rep, JSArg idx]
findRepeater' e (ByRowAndCol rep row col) = execElems "findRepeaterElement" [e, JSArg rep, JSArg row, JSArg col]

-- | Evaluate an angular expression, using the scope attached to the given element.
ngEvaluate :: (WebDriver wd, A.FromJSON a) 
           => Element -- ^ element in whose scope to evaluate
           -> T.Text  -- ^ expression to evaluate, e.g. \"dog.name | reverse\"
           -> wd a
ngEvaluate e expr = execCS "evaluate" [JSArg e, JSArg expr]

-- | Return the current absolute url according to Angular (using @$location.absUrl()@).
getLocationAbsUrl :: WebDriver wd => T.Text -- ^ CSS selector to element which has ng-app
                                  -> wd T.Text
getLocationAbsUrl sel = execCS "getLocationAbsUrl" [JSArg sel]
