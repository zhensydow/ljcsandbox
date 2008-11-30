Parser
\begin{code}
module Chapter08 where
\end{code}

\begin{code}
import Data.Char
\end{code}

\begin{code}
type Parser a = String ->Maybe (a,String)
\end{code}

\begin{code}
returnp :: a -> Parser a
returnp v = \inp -> Just (v,inp)
\end{code}

\begin{code}
failure :: Parser a
failure = \inp -> Nothing
\end{code}

\begin{code}
item :: Parser Char
item = \imp -> case imp of
                 [] -> Nothing
                 (x:xs) -> Just (x,xs)
\end{code}

\begin{code}
parse :: Parser a -> String -> Maybe (a,String)
parse p inp = p inp
\end{code}

\begin{code}
(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>>= f = \inp ->case parse p inp of
                   Nothing -> Nothing
                   Just (v,out) -> parse (f v) out
\end{code}

\begin{code}
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \imp -> case parse p imp of
                    Nothing -> parse q imp
                    ret -> ret
\end{code}

\begin{code}
sat p = item >>>= \x -> if p x then returnp x else failure
\end{code}

\begin{code}
digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphanum = sat isAlphaNum
char x = sat (==x)
\end{code}

\begin{code}
string :: String -> Parser String
string [] = returnp []
string (x:xs) = (char x) >>>= \_-> (string xs) >>>= \_-> returnp (x:xs)
\end{code}

\begin{code}
many :: Parser a -> Parser [a]
many p = many1 p +++ returnp []
many1 :: Parser a -> Parser [a]
many1 p = p >>>= \v-> many p >>>= \vs-> returnp (v:vs)
\end{code}

\begin{code}
ident = lower >>>= \x-> (many alphanum) >>>= \xs-> returnp (x:xs)
\end{code}

\begin{code}
nat :: Parser Int
nat = (many1 digit) >>>= \xs-> returnp (read xs)
\end{code}

\begin{code}
space = (many $ sat isSpace) >>>= \_-> returnp ()
\end{code}

\begin{code}
token p = space >>>= \_-> p >>>= \v-> space >>>= \_-> returnp v
\end{code}

\begin{code}
identifier = token ident
natural = token nat
symbol xs = token (string xs)
\end{code}

\begin{code}
numbers :: Parser [Int]
numbers = begin >>>= \_-> natural >>>= 
          \n-> (many rest) >>>= 
          \ns-> end >>>= \_-> returnp (n:ns)
    where begin = symbol "["
          end = symbol "]"
          rest = (symbol ",") >>>= \_-> natural
\end{code}

\begin{code}
expr = term >>>= \t-> 
       ( (symbol "+" >>>= \_-> 
          expr >>>= \e-> 
          returnp (t+e)) 
         +++ 
         (symbol "-" >>>= \_->
          expr >>>= \e->
          returnp (t-e))
         +++ returnp t)
\end{code}

\begin{code}
term = expo >>>= \e-> 
       ( (symbol "*" >>>= \_-> 
          term >>>= \t->
          returnp (e*t)) 
         +++ 
         (symbol "/" >>>= \_->
          term >>>= \t->
          returnp (e `div` t))
         +++ returnp e)
\end{code}

\begin{code}
expo = factor >>>= \f->
       ( (symbol "^" >>>= \_->
          factor >>>= \f2->
          returnp (f^f2))
         +++ returnp f)
\end{code}

\begin{code}
factor = (symbol "(" >>>= \_-> 
          expr >>>= \e->
          symbol ")" >>>= \_->
          returnp e)
         +++ natural
\end{code}

\begin{code}
eval xs = case parse expr xs of
            Just (n,[]) -> n
            Just (_,out) -> error $ "unused input " ++ out
            Nothing -> error "invalid input"
\end{code}

Exercises

\begin{code}
int = (symbol "-" >>>= \_->
       natural >>>= \n->
       returnp (-n))
      +++ natural
\end{code}

\begin{code}
comment = symbol "--" >>>= \_->
          many (sat (/='\n')) >>>= \_->
          string "\n" >>>= \_->
          returnp ""
\end{code}

\begin{code}
test = (symbol "a") +++ (symbol "b") +++ (symbol "c")
\end{code}
