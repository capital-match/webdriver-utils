{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, GeneralizedNewtypeDeriving  #-}
-- |Internal functions
module Test.WebDriver.Commands.Internal
   ( clientScripts ) where

import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Language.JavaScript.Parser (JSNode(..), Node(..))
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Language.JavaScript.Parser as JS


-- | Parse top level javascript commands
parseClientTop :: JSNode -> [(T.Text,T.Text)]
parseClientTop (NN (JSSourceElementsTop es)) = catMaybes $ map parseClientDef es
parseClientTop _ = []

-- | Build a function body that can be passed to async.  The body must use
-- arguments array but the code from clientSideScripts.js uses function arguments.
buildFunction :: Node -> T.Text
buildFunction func = "return (" <> T.pack func' <> ").apply(null, arguments);"
    where
        func' = dropWhile (=='\n') $ JS.renderToString $ NN func

-- | Parse a call clientSideScripts.somefunction = function() {...}, returning the function name
-- and the body.
parseClientDef :: JSNode -> Maybe (T.Text, T.Text)
parseClientDef (NN (JSExpression
                        [NN (JSMemberDot
                            [NT (JSIdentifier "clientSideScripts") _ _]
                            _ -- literal .
                            (NT (JSIdentifier name) _ _))
                        , NN (JSOperator (NT (JSLiteral "=") _ _))
                        , NN (func@(JSFunctionExpression _ _ _ _ _ _))
                        ]))
    = Just (T.pack name, buildFunction func)
                        -- Use renderToString instead of renderJS so we don't need to depend on builder
parseClientDef _ = Nothing

-- | Parse a javascript file.  All toplevel commands which look like
--
-- >clientSideScripts.somefunction = function() {
-- > ...
-- >};
--
-- are loaded.  The return value is either an error message or
-- a map with keys the function names and value the body of the function.
clientScripts :: String -> Either String (M.HashMap T.Text T.Text)
clientScripts j = M.fromList . parseClientTop <$> JS.parse j "<client>"
