module Reader

import Control.Monad.Either
import Control.Monad.State
import Data.SortedSet
import Syntax

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings


import Debug.Trace

PushbackBuffer : Type
PushbackBuffer = (List Char, List Char)

record ReaderState where
  constructor MkReaderState
  delims : List Char
  stackc : Nat
  buffer : PushbackBuffer

data Error = ReaderError
           | UnmatchedDelimeter
           | NestingTooDeep (Nat, Nat)
           | EOFError
           | CannotUnreadError

Reader : Type -> Type
Reader a = EitherT Error (State ReaderState) a

runReader : Reader a -> String -> (Either Error a, ReaderState)
runReader m s =
  runState (runEitherT m) $ MkReaderState [] 0 (unpack s, [])

readChar : Reader (Maybe Char)
readChar =
  do MkReaderState d sc (s, buf) <- get
     case s of
       []        => pure Nothing
       (c :: s') => do _ <- put $ MkReaderState d sc (s', [c])
                       pure $ Just c

unreadChar : Reader ()
unreadChar =
  do MkReaderState d sc (s, buf) <- get
     case buf of
       []       => throwErr CannotUnreadError
       (c :: _) => do _ <- put $ MkReaderState d sc (c :: s, []); pure ()

readWhile : (Char -> Bool) -> Reader String
readWhile p =
  do c <- readChar
     case c of
       Just c  => if p c
                  then do s <- readWhile p; pure $ strCons c s
                  else do _ <- unreadChar; pure ""
       Nothing => pure ""

readUntil : (Char -> Bool) -> Reader String
readUntil p = readWhile $ not . p

tokenEnd : SortedSet Char
tokenEnd = fromList [' ', '\n', '(', ')']

readToken : Char -> Reader String
readToken c = map (strCons c) $ readUntil $ \c => contains c tokenEnd

readSymbol : Char -> Reader Expr
readSymbol c = map (Sym . MkSymbol) $ readToken c

data RExpr = FIN | EOF | E Expr | L (List Expr)
implementation Show RExpr where
  show FIN = "FIN"
  show EOF = "EOF"
  -- show E e = debug e
  show _ = "oops"
  -- show L e = show e

whiteChars : SortedSet Char
whiteChars = fromList [' ', '\n', '\t', '\f', '\r']

whitespace : Char -> Bool
whitespace c = contains c whiteChars

delimStackCheck : Reader ()
delimStackCheck =
  do MkReaderState ds sc _ <- get
     if length ds > 2 --|| sc > 5
     then throwErr $ NestingTooDeep $ (length ds, sc)
     else pure ()

pushDelim : Char -> Reader ()
pushDelim c =
  do _ <- delimStackCheck
     modify (\s => record { delims $= (::) c } s)

popDelim : Reader ()
popDelim =
  do MkReaderState ds _ _ <- get
     case ds of
       []        => throwErr UnmatchedDelimeter
       (_ :: xs) => modify (\s => record { delims = xs } s)

getDelim : Reader (Maybe Char)
getDelim =
  do MkReaderState ds _ _ <- get
     case ds of
       []       => pure Nothing
       (c :: _) => pure $ Just c

(>>) : Monad m => m a -> m b -> m b
(>>) ma mb = ma >>= \_ => mb

mutual
  macro : Char -> Maybe (Char -> Reader RExpr)
  macro '(' = Just $ \c => do _ <- pushDelim ')'; readList c
  macro ')' = Just $ \_ => throwErr UnmatchedDelimeter
  macro _   = Nothing

  readExprC : Char -> Reader RExpr
  readExprC c =
    if whitespace c
    then readExpr
    else case (macro c) of
      Just r  => r c
      Nothing => map E $ readSymbol c

  readExpr : Reader RExpr
  readExpr =
    do c <- readChar
       d <- getDelim
       case (c, d) of
         (Just c', Just d') => if c' == d' then pure FIN else readExprC c'
         (Just c', _)       => readExprC c'
         _                  => throwErr EOFError

  readList : Char -> Reader RExpr
  readList c =
    do expr <- readExpr
       case expr of
         FIN => do _ <- popDelim; pure $ L []
         EOF => throwErr EOFError
         E e => do (L l) <- readList c; pure $ L $ e :: l
         L l => pure $ L l
         _   => throwErr ReaderError

-- -- read =
-- --   do c <- readChar
-- --      case c of

-- main : IO ()
-- main = let (a, s) = runReader readExpr "(())"
--        in case a of
--          Left e => putStrLn "Err"
--          Right a => print a
mutual
  seq : Parser (List RExpr)
  seq = char '(' *!> (expr `sepBy` (char ' ')) <* char ')'

  expr' : Parser RExpr
  expr' =  (map JsonString jsonString)
            <|> (map JsonNumber jsonNumber)
            <|> (map JsonBool   jsonBool)
            <|>| map seq  seq

  expr : Parser RExpr
  expr = spaces *> expr' <* spaces
