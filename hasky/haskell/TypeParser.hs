module TypeParser where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskell, haskellDef)
import Text.Parsec.String

import HTypes (HType(..), typeparsers, htype)

data TypeDef = TypeDef {
    funcN :: String,
    funcT :: [HType]
    } deriving (Show, Eq)

parseTypeDef :: Parser TypeDef
parseTypeDef = do
  fname <- funcName
  types <- typeDef *> parseTypes
  return $ TypeDef fname types

parseTypes :: Parser [HType]
parseTypes = skip *> sepBy1 (parseType <* skip) (arrow <* skip)

parseType :: Parser HType
parseType = func <|> tuple <|> list <|> unit <|> htype
-- Parser
typeConstr = funcName *> barrow

skip = skipMany space

unit = parens skip >> return HUnit
func = try $ lookAhead isFunc *> parens (skip *> sepBy1 (htype <* skip) (arrow <* skip))  >>= return . HFunc
tuple = try $ lookAhead isTuple *> parens (commaSep (htype <* skip)) >>= return . HTuple
list = brackets (skip *> htype <* skip) >>= return . HList

isFunc = lookAhead $ parens (identifier *> arrow *> identifier *> many (arrow *> identifier))
isTuple = lookAhead $ parens (identifier *> comma *> identifier *> many (comma *> identifier))
isTypeDef = lookAhead $ identifier *> typeDef

-- Lexer:
lexer = P.makeTokenParser haskellDef

commaSep = P.commaSep lexer
parens = P.parens lexer
brackets = P.brackets lexer
barrow = (P.reservedOp lexer) "=>"
arrow = (P.reservedOp lexer) "->"
iomonad = (P.reservedOp lexer) "IO"
typeDef = (P.reservedOp lexer) "::"
comma = P.comma lexer
funcName = P.identifier lexer
identifier = P.identifier lexer
