module HaskyFFI.ParseTypes (parseTypeDefs, TypeDef(funcN, funcT)) where

import Text.Parsec
import Text.Parsec.String (Parser)

import HaskyFFI.HTypes (HType(..), htype)
import HaskyFFI.ParseUtils

data TypeDef = TypeDef {
    funcN :: String,
    funcT :: [HType]
    } deriving (Show, Eq)

parseTypeDefs :: Parser [TypeDef]
parseTypeDefs = manyTill parseIfTypeDef eof

parseIfTypeDef :: Parser TypeDef
parseIfTypeDef = manyTill skipLine isTypeDef *> parseTypeDef

parseTypeDef :: Parser TypeDef
parseTypeDef = do
  fname <- funcName
  types <- typeDef *> parseTypes
  manyTill (skipLine <|> [] <$ whiteSpace) (() <$ isTypeDef <|> eof)
  return $ TypeDef fname types

parseTypes :: Parser [HType]
parseTypes = skip *> sepBy1 (strip parseType) (strip arrow) <* skipLine

parseType :: Parser HType
parseType = func <|> tuple <|> list <|> io <|> unit <|> htype

typeConstr = funcName *> barrow

skipLine = manyTill anyToken (endOfLine <|> semi <|> ('\n' <$ eof))

io    = HIO    <$> (try iomonad *> parseType)
unit  = HUnit  <$  (parens skip)
func  = HFunc  <$> (isFunc *> parens (sepBy1 (strip parseType) (strip arrow)))
tuple = HTuple <$> (isTuple *> parens (commaSep $ strip parseType))
list  = HList  <$> (brackets (strip parseType))

-- Checkers:
isFunc = try $ lookAhead $ parens (identifier *> many1 (strip $ arrow *> parseType))
isTuple = try $ lookAhead $ parens (parseType *> many1 (strip $ comma *> parseType))
isTypeDef = try $ lookAhead parseTypeDef

