module ParseUtils where

import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.Functor (($>))

skip = whiteSpace
strip x = skip *> x <* skip

-- Lexer:
lexer = P.makeTokenParser haskellDef

mod = P.reserved lexer "module"
whe = P.reserved lexer "where"
commaSep = P.commaSep lexer
parens = P.parens lexer
brackets = P.brackets lexer
barrow = P.reservedOp lexer "=>"
arrow = P.reservedOp lexer "->"
iomonad = P.reservedOp lexer "IO"
typeDef = P.reservedOp lexer "::"
semi = P.semi lexer $> ';'
comma = P.comma lexer
funcName = P.identifier lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer
