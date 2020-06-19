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
parseTypeDefs = many parseIfTypeDef

parseIfTypeDef :: Parser TypeDef
parseIfTypeDef = let getIfTypeDef = try $ lookAhead isTypeDef *> parseTypeDef
                  in getIfTypeDef <|> skipLine *> parseIfTypeDef

parseTypeDef :: Parser TypeDef
parseTypeDef = do
  fname <- funcName
  types <- typeDef *> parseTypes
  return $ TypeDef fname types

parseTypes :: Parser [HType]
parseTypes = skip *> sepBy1 (strip parseType) (strip arrow)

parseType :: Parser HType
parseType = func <|> tuple <|> list <|> io <|> unit <|> htype

typeConstr = funcName *> barrow

skipLine = manyTill anyToken (newline <|> semi)

io = try iomonad *> parseType >>= return . HIO
unit = parens skip >> return HUnit
func = try $ lookAhead isFunc *> parens (sepBy1 (strip parseType) (strip arrow))  >>= return . HFunc
tuple = try $ lookAhead isTuple *> parens (commaSep $ strip parseType) >>= return . HTuple
list = brackets (strip parseType) >>= return . HList

-- Checkers:
isFunc = lookAhead $ parens (identifier *> many1 (strip $ arrow *> parseType))
isTuple = lookAhead $ parens (parseType *> many1 (strip $ comma *> parseType))
isTypeDef = lookAhead $ identifier *> typeDef
