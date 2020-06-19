module TypeParser where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskell, haskellDef)
import Text.Parsec.String

import HTypes (HType(..), htype)

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

skip = whiteSpace
strip x = skip *> x <* skip
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

-- Lexer:
lexer = P.makeTokenParser haskellDef

commaSep = P.commaSep lexer
parens = P.parens lexer
brackets = P.brackets lexer
barrow = (P.reservedOp lexer) "=>"
arrow = (P.reservedOp lexer) "->"
iomonad = (P.reservedOp lexer) "IO"
typeDef = (P.reservedOp lexer) "::"
semi = P.semi lexer *> return ';'
comma = P.comma lexer
funcName = P.identifier lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer
