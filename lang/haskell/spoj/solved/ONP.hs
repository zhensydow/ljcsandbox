--------------------------------------------------------------------------------
import Control.Monad( replicateM_ )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

--------------------------------------------------------------------------------
data Expr = Operand !Char
          | Expr !Char Expr Expr
          deriving( Show )

--------------------------------------------------------------------------------
expr    :: Parser Expr
expr    = buildExpressionParser table factor
          <?> "expression"

table = [
  [op "^" (\a b -> Expr '^' a b) AssocLeft],
  [op "/" (\a b -> Expr '/' a b) AssocLeft],
  [op "*" (\a b -> Expr '*' a b) AssocLeft],
  [op "-" (\a b -> Expr '-' a b) AssocLeft],
  [op "+" (\a b -> Expr '+' a b) AssocLeft]
        ]
  where
    op s f assoc = Infix (do{ string s; return f}) assoc

factor  = do{ char '('
            ; x <- expr
            ; char ')'
            ; return x 
            }
          <|> operand
          <?> "simple expression"

operand :: Parser Expr
operand = do{ o <- letter
            ; return $ Operand o
            }
        <?> "operand"

--------------------------------------------------------------------------------
rpnForm :: Expr -> String
rpnForm (Operand c) = [c]
rpnForm (Expr o a b) = concat [rpnForm a,rpnForm b,[o]]

--------------------------------------------------------------------------------
testCase = do
  n <- getLine
  case (parse expr "" n) of
    Left err -> error $ show err
    Right x ->  putStrLn . rpnForm $ x
  
--------------------------------------------------------------------------------
main = do
  num <- getLine
  replicateM_ (read num) testCase

--------------------------------------------------------------------------------
