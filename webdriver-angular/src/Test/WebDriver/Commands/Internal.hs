{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, GeneralizedNewtypeDeriving  #-}
-- |Internal functions
module Test.WebDriver.Commands.Internal
   ( clientScripts ) where

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Language.JavaScript.Parser (JSAST(..), JSExpression(..), JSStatement(..))
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Language.JavaScript.Parser as JS


-- | Parse top level javascript commands
parseClientTop :: JSAST -> [(T.Text,T.Text)]
parseClientTop (JSAstProgram es _) = catMaybes $ map parseClientDef es
parseClientTop _ = []

-- | Build a function body that can be passed to async.  The body must use
-- arguments array but the code from clientSideScripts.js uses function arguments.
buildFunction :: JSExpression -> T.Text
buildFunction func = "return (" <> T.pack func' <> ").apply(null, arguments);"
    where
        func' = dropWhile (=='\n') $ JS.renderToString $ JSAstExpression func JS.JSNoAnnot

-- | Parse a call clientSideScripts.somefunction = function() {...}, returning the function name
-- and the body.
parseClientDef :: JSStatement -> Maybe (T.Text, T.Text)
parseClientDef (JSAssignStatement
                    (JSMemberDot
                        (JSIdentifier _ "clientSideScripts")
                        _ -- annotation
                        (JSIdentifier _ name))
                    _ -- assign operator
                    (func@(JSFunctionExpression{}))
                    _) -- semicolon
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
