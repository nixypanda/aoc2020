module Day18 (d18p1, d18p2) where

import Text.Parsec
    ( char
    , digit
    , oneOf
    , string
    , between
    , chainl1
    , chainr1
    , many1
    , (<|>)
    , many
    , parse
    , ParseError
    )
import Text.Parsec.String ( Parser )

-- Translation to a CFG
-- Expression ::= Expression [+*] Factor | Factor
-- Factor     ::= Number | '(' Expression ')'

type BOp = Int -> Int -> Int

data Expr = B BOp Expr Expr | N Int

instance Show Expr where
    show (N int) = show int
    show (B _ e1 e2) = "(" ++ show e1 ++ ") op (" ++ show e2 ++ ")"

symbol :: String -> Parser String
symbol s = string s <* ws

toBOp :: Char -> Expr -> Expr -> Expr
toBOp '+' = B (+)
toBOp '*' = B (*)

ws :: Parser String
ws = many (char ' ')

num :: Parser Expr
num = N . read <$> many1 digit <* ws

addMul :: Parser (Expr -> Expr -> Expr)
addMul = toBOp <$> oneOf "+*" <* ws

parens :: Parser Expr
parens = between (symbol "(") (symbol ")") expression

factor :: Parser Expr
factor = num <|> parens

expression :: Parser Expr
expression = factor `chainl1` addMul

-- >>> fmap eval $ parse expression "" "1 + 2 * 3 + 4 * 5 + 6"
-- Right 71
-- >>> fmap eval $ parse expression "" "1 + (2 * 3) + (4 * (5 + 6))"
-- Right 51
-- >>> fmap eval $ parse expression "" "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
-- Right 13632
eval :: Expr -> Int
eval (N i) = i
eval (B b e1 e2) = eval e1 `b` eval e2

-- >>> parse expression "" "1 + 2 * 3 + 4 * 5 + 6"
-- Right (((((1) op (2)) op (3)) op (4)) op (5)) op (6)

parseWith :: Parser b -> String -> Either ParseError [b]
parseWith parser = mapM (parse parser "") . lines

d18p1 :: IO ()
d18p1 = getContents >>= print . fmap (sum . fmap eval) . parseWith expression


-- Part 2

-- Translation to a CFG
-- Expression ::= Term '*' Expression | Term
-- Term       ::= Factor '+' Term | Factor
-- Factor     ::= Number | '(' Expression ')'

add :: Parser (Expr -> Expr -> Expr)
add = toBOp <$> char '+' <* ws

mul :: Parser (Expr -> Expr -> Expr)
mul = toBOp <$> char '*' <* ws

parens' :: Parser Expr
parens' = between (symbol "(") (symbol ")") expression'

factor' :: Parser Expr
factor' = num <|> parens'

term :: Parser Expr
term = factor' `chainr1` add 

expression' :: Parser Expr
expression' = term `chainr1` mul

-- >>> parse expression' "" "1 + 2 * 3 + 4 * 5 + 6"
-- Right ((1) op (2)) op (((3) op (4)) op ((5) op (6)))
--
-- >>> fmap eval $ parse expression' "" "1 + 2 * 3 + 4 * 5 + 6"
-- Right 231
-- >>> fmap eval $ parse expression' "" "1 + (2 * 3) + (4 * (5 + 6))"
-- Right 51
-- >>> fmap eval $ parse expression' "" "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
-- Right 23340

d18p2 :: IO ()
d18p2 = getContents >>= print . fmap (sum . fmap eval) . parseWith expression'
