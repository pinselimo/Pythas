module TypeParser (parseTypeDefs, TypeDef(funcN, funcT)) where

import Text.Parsec
import Text.Parsec.String (Parser)

import HTypes (HType(..), htype)
import ParseUtils

data TypeDef = TypeDef {
    funcN :: String,
    funcT :: [HType]
    } deriving (Show, Eq)

parseTypeDefs :: Parser [TypeDef]
parseTypeDefs = manyTill parseIfTypeDef eof

parseIfTypeDef :: Parser TypeDef
parseIfTypeDef = (manyTill skipLine isTypeDef) *> parseTypeDef

parseTypeDef :: Parser TypeDef
parseTypeDef = do
  fname <- funcName
  types <- typeDef *> parseTypes
  return $ TypeDef fname types

parseTypes :: Parser [HType]
parseTypes = skip *> sepBy1 (strip parseType) (strip arrow) <* skipLine

parseType :: Parser HType
parseType = func <|> tuple <|> list <|> io <|> unit <|> htype

typeConstr = funcName *> barrow

skipLine = manyTill anyToken (endOfLine <|> semi <|> (eof *> return '\n'))

io = try iomonad *> parseType >>= return . HIO
unit = parens skip >> return HUnit
func = isFunc *> parens (sepBy1 (strip parseType) (strip arrow))  >>= return . HFunc
tuple = isTuple *> parens (commaSep $ strip parseType) >>= return . HTuple
list = brackets (strip parseType) >>= return . HList

-- Checkers:
isFunc = try $ lookAhead $ parens (identifier *> many1 (strip $ arrow *> parseType))
isTuple = try $ lookAhead $ parens (parseType *> many1 (strip $ comma *> parseType))
isTypeDef = try $ lookAhead $ parseTypeDef
