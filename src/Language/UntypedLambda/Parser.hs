module Language.UntypedLambda.Parser
  ( runUlParser
  ) where

import           Language.UntypedLambda.Prelude (c, prelude)
import           Language.UntypedLambda.Types
import           Language.Utils.Parser

import           Control.Applicative
import qualified Data.Map                       as Map
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Text.Parser.Token.Highlight
import           Text.Trifecta

runUlParser :: String -> Either String UntypedLambdaTerm
runUlParser = runParserString exprP

exprP :: Parser UntypedLambdaTerm
exprP = lefty <$> factorP <*> termsP
  where
    lefty x xs = foldl1 TmApp (x:xs)
    termsP = many (space *> factorP)

factorP :: Parser UntypedLambdaTerm
factorP = (char '(' *> (exprP <* char ')')) <|> try numP <|> varP <|> lambdaP

lambdaP :: Parser UntypedLambdaTerm
lambdaP = TmLam <$  symbol "λ"
                <*> identP
                <*  dot
                <*> token exprP

numP :: Parser UntypedLambdaTerm
numP = c . read <$  char 'c'
                <*> some digit

varP :: Parser UntypedLambdaTerm
varP = toTerm <$> oneOf ['a'..'z'] <*> many alphaNum
  where
    toTerm x xs = lifty $ T.pack (x:xs)
    lifty var = Map.findWithDefault (TmVar var) var prelude

identP :: Parser Text
identP = ident defaultIdentStyle

defaultIdentStyle :: IdentifierStyle Parser
defaultIdentStyle = IdentifierStyle
  { _styleName              = "UntypedLambda"
  , _styleStart             = oneOf ['a'..'z']
  , _styleLetter            = alphaNum
  , _styleReserved          = mempty
  , _styleHighlight         = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }
