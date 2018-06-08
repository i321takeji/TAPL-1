module Language.UntypedLambda.Parser
  ( runUlParser
  ) where

import           Language.UntypedLambda.Types
import           Language.Utils.Parser

import           Control.Applicative
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.Parser.Token.Highlight
import           Text.Trifecta

runUlParser :: String -> Either String Term
runUlParser = runParserString exprP

exprP :: Parser Term
exprP = lefty <$> factorP <*> termsP
  where
    lefty x xs = foldl1 TmApp (x:xs)
    termsP = many (space *> factorP)

factorP :: Parser Term
factorP = (char '(' *> (exprP <* char ')')) <|> varP <|> lambdaP

lambdaP :: Parser Term
lambdaP = TmLam <$  symbol "λ"
                <*> identP
                <*  dot
                <*> token exprP

varP :: Parser Term
varP = lifty <$> oneOf ['a'..'z'] <*> many alphaNum
  where
    lifty x xs = TmVar $ T.pack (x:xs)

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
