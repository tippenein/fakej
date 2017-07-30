{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Lib where

import GHC.Show (Show(..))
import Lib.Prelude
import Text.Megaparsec hiding (token)
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Char as C

type Parser = Parsec () Text

data Expr
  = Num Integer
  | Add SeqExpr SeqExpr
  | Seq SeqExpr
  | Fold SeqExpr
  deriving (Show, Eq)

data SeqExpr
  = SeqNum Integer SeqExpr
  | End
  deriving (Show, Eq)

data Value
  = NumValue Integer
  | SeqValue SeqExpr
  deriving (Eq)

instance Show Value where
  show (NumValue i) = show i
  show (SeqValue is) = show is

runSeqExpr :: SeqExpr -> [Integer]
runSeqExpr (SeqNum i rest) = i : runSeqExpr rest
runSeqExpr End = []

toSeqExpr :: [Integer] -> SeqExpr
toSeqExpr (x:xs) = SeqNum x $ toSeqExpr xs
toSeqExpr [] = End

eval :: Expr -> Value
eval (Num n) = NumValue n
eval (Seq s) = SeqValue s
eval (Add e1 e2) = SeqValue $ toSeqExpr $ map (\(a,b) -> a + b) $ zip (runSeqExpr e1) (runSeqExpr e2)
eval (Fold e1) = NumValue $ sum $ runSeqExpr e1

run :: Text -> Expr
run t = case runParser expr "" t of
  Left _e -> error "dsaf"
  Right a -> a

num :: Parser Expr
num = do
  n <- L.integer
  return $ Num n

seqExpr :: Parser SeqExpr
seqExpr = do
  toSeqExpr <$> many L.integer

-- addOp = L.symbol "+"

-- foldOp = L.symbol "+/"

-- prefixOp = do
--   addOp <|> foldOp
-- plus = L.symbol '+'
expr :: Parser Expr
expr = do
  addExpr <|> foldExpr -- <|> num
  where
    addExpr = do
      a <- seqExpr
      _ <- reserved "+"
      b <- seqExpr
      return $ Add a b

    foldExpr = do
      _ <- reserved "+/"
      a <- seqExpr
      return $ Fold a
  -- addop seqExpr seqExpr

  -- b <- seqExpr
  -- return $ op <$> a <*> b
-- infixOp :: Text -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (SeqExpr -> SeqExpr -> Expr)
addop = reserved "+" >> return Add

token :: Parser a -> Parser a
token p = do
  a <- p
  _ <- many C.spaceChar
  return a

spaces :: Parser ()
spaces = many C.spaceChar >> pure ()


lexeme :: Parser a -> Parser a
lexeme  = L.lexeme spaces

integer = lexeme L.integer

text = C.string

-- reserved :: Text -> Parser Text
reserved s = token (text s)
