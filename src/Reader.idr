module Reader

import Control.Monad.Either
import Control.Monad.State
import Data.SortedSet
import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Syntax

tokenEnd : SortedSet Char
tokenEnd = fromList [' ', '\n', '(', ')', '[', ']']

specialChar : SortedSet Char
specialChar = union tokenEnd $ fromList ['#', '^']

str : List Char -> String
str []        = ""
str (s :: ss) = strCons s $ str ss

applyApp : Vect 2 Expr -> Expr
applyApp [x, e] = App x e

token : Parser (List Char)
token = (many $ satisfy $ not . (\c => contains c tokenEnd))

symbol' : Parser (List Char)
symbol' = map (::) (satisfy $ not . (\c => contains c specialChar)) <*> token

symbol : Parser Symbol
symbol = map (MkSymbol . str) symbol' <?> "Symbol"

sym : Parser Expr
sym = map Sym symbol

fn : Parser String
fn = spaces *> string "fn" <* spaces

specialsym : String -> Parser Expr
specialsym s = spaces *> map (Sym . MkSymbol) (string s) <* spaces

lbind : Parser Symbol
lbind = char '[' >! spaces *!> symbol <* spaces >! char ']'

sexp : Parser a -> Parser a
sexp = between (char '(') (char ')')

vexp : Parser a -> Parser a
vexp e = spaces *> between (char '[') (char ']') e <* spaces

letcons : Parser (Symbol, Expr) -> Parser Expr -> Parser Expr
letcons binding expr =
  do (sym, e1) <- binding
     e2        <- expr
     pure $ Let sym e1 e2

litInt : Parser Expr
litInt = map (Lit . LInt . fromInteger . fromDigits) $ some digit
  where fromDigits : List (Fin 10) -> Integer
        fromDigits = foldl (\a, b => 10 * a + cast b) 0

mutual
  app : Parser Expr
  app = sexp $ map applyApp (ntimes 2 expr)

  lambda : Parser Expr
  lambda = sexp $ fn *> map Lam lbind <*> expr

  letbind : Parser (Symbol, Expr)
  letbind = vexp $ liftA2 MkPair symbol expr

  letexpr : Parser Expr
  letexpr = sexp (specialsym "let" *> letcons letbind expr)

  ifexpr : Parser Expr
  ifexpr = sexp $ specialsym "if" *> map If expr <*> expr <*> expr

  expr' : Parser Expr
  expr' = sym <|> litInt <|>| lambda <|>| app <|>| letexpr <|>| ifexpr

  expr : Parser Expr
  expr = spaces *> expr' <* spaces
