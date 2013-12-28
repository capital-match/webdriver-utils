{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Caps where

import Data.Typeable (Typeable)
import Test.Hspec.WebDriver
import qualified Test.WebDriver.Capabilities as W

data TestCaps = Firefox
              | Chrome
    deriving (Show,Eq,Enum,Bounded,Typeable)

instance TestCapabilities TestCaps where
    matchesCaps Firefox (W.Capabilities { W.browser = W.Firefox _ _ _ }) = True
    matchesCaps Chrome (W.Capabilities { W.browser = W.Chrome _ _ _ _ }) = True
    matchesCaps _ _ = False

    newCaps Firefox = return W.defaultCaps -- defaultCaps uses Firefox.
    newCaps Chrome = return W.defaultCaps { W.browser = W.chrome }
