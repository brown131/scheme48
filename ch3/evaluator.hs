{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Data.Complex
import Data.Ratio
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Number

data LispVal = Atom String
             | List [LispVal]
             | Vector [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Ratio Rational
             | Complex (Complex Double)
             | String String
             | Bool Bool
           deriving (Read, Show)

--instance Show LispVal where show = showVal
         
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ Atom atom
                         
{-| Parse Literals -}

parseBool :: Parser LispVal
parseBool = do
              prefix <- oneOf "tf"
              return $ case prefix of 
                         't' -> Bool True
                         'f' -> Bool False

parseRadix :: Parser LispVal
parseRadix = do
               prefix <- oneOf "bdox"
               x <- many1 (case prefix of
                           'b' -> binDigit
                           'd' -> digit
                           'o' -> octDigit
                           'x' -> hexDigit)
               let ((n, _):_) = (case prefix of
                                  'b' -> readBin 
                                  'd' -> readDec 
                                  'o' -> readOct 
                                  'x' -> readHex) x
               return $ Number n

parseCharLiteral :: Parser LispVal
parseCharLiteral = do
                     char '\\'
                     x <- try (string "space" >> return ' ')
                          <|> try (string "newline" >> return '\n')
                          <|> anyChar
                     return $ String [x]

parseVector :: Parser LispVal
parseVector = liftM Vector $ sepBy parseExpr spaces

parseLiteral :: Parser LispVal
parseLiteral = do
                 char '#'
                 x <- (parseBool <|> parseRadix <|> parseCharLiteral <|> parseVector)
                 return x 

{-| Parse Numbers -}

readBin' :: (Eq a, Num a) => ReadS a
readBin' ""     = [(0, "")]
readBin' (x:[]) = [(if x == '1' then 1 else 0, "")]
readBin' (x:xs) = [((if x == '1' then 1 else 0) + (2 * (fst $ head $ readBin' xs)), "")]

readBin :: (Eq a, Num a) => ReadS a
readBin s = readBin' $ reverse s

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n
toDouble(Ratio n) = fromRational n

parseInteger :: Parser LispVal                
parseInteger = do
                  x <- int; -- many1 digit <|> (:) <$> char '-' <*> many1 digit
                  notFollowedBy $ char '.' 
                  return $ Number $ read x

parseFloat :: Parser LispVal
parseFloat = do
               n <- many1 digit
               char '.'
               e <- many1 digit
               let ((f, _):_) = readFloat (n ++ "." ++ e)
               return $ Float f

parseNegFloat :: Parser LispVal
parseNegFloat = do
                  char '-'
                  n <- many1 digit
                  char '.'
                  e <- many1 digit
                  let ((f, _):_) = readFloat(n ++ "." ++ e)
                  return $ Float $ f * (-1)

parseRatio :: Parser LispVal
parseRatio = do
               n <- many1 digit
               char '/'
               d <- many1 digit
               return $ Ratio ((read n) % (read d))

parseComplex :: Parser LispVal
parseComplex = do
                 x <- (try parseFloat <|> parseInteger)
                 char '+'
                 y <- (try parseFloat <|> parseInteger)
                 char 'i' 
                 return $ Complex (toDouble x :+ toDouble y)
                   
parseNumber :: Parser LispVal                
parseNumber = try parseRatio <|> try parseComplex <|> try parseInteger <|> try parseFloat <|> try parseNegFloat

{-| Parse Data Structures -}

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many1 $ noneOf "\""
                     <|> try (string "\\\"" >> return '"')
                     <|> try (string "\\n" >> return '\n')
                     <|> try (string "\\r" >> return '\r')
                     <|> try (string "\\t" >> return '\t')
                     <|> try (string "\\\\" >> return '\\')
                char '"'
                return $ String x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
                    head <- endBy parseExpr spaces
                    tail <- char '.' >> spaces >> parseExpr
                    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]
    
parseBackquoted :: Parser LispVal
parseBackquoted = do
                    char '`'
                    x <- parseExpr
                    return $ List [Atom "quasiquote", x]
    
parseUnquoted :: Parser LispVal
parseUnquoted = do
                    char ','
                    x <- parseExpr
                    return $ List [Atom "unquote", x]
    
parseExpr :: Parser LispVal
parseExpr = parseLiteral
            <|> parseNumber
            <|> parseString
            <|> parseQuoted
            <|> parseBackquoted
            <|> parseUnquoted
            <|> parseAtom
            <|> do char '('
                   x <- try parseList <|> try parseDottedList
                   char ')'
                   return x

{-| Evaluation -}

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives
             
testValType :: String -> LispVal -> LispVal
testValType "boolean?" (Bool _) = Bool True
testValType "char?" (String [_]) = Bool True
testValType "complex?" (Complex _) = Bool True
testValType "float?" (Float _) = Bool True
testValType "list?" (List _) = Bool True
testValType "number?" (Number _) = Bool True
testValType "pair?" (DottedList _ _) = Bool True
testValType "rational?" (Ratio _) = Bool True
testValType "symbol?" (Atom _) = Bool True
testValType "string?" (String _) = Bool True
testValType "vector?" (Vector _) = Bool True
testValType _ _ = Bool False

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Float _) = val
eval val@(Number _) = val
eval val@(Ratio _) = val
eval val@(Complex _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List [Atom "symbol->string", Atom val]) = String val
eval (List [Atom "string->symbol", String val]) = Atom val
eval (List [Atom "boolean?", val]) = testValType "boolean?" val
eval (List [Atom "char?", val]) = testValType "char?" val
eval (List [Atom "complex?", val]) = testValType "complex?" val
eval (List [Atom "float?", val]) = testValType "float?" val
eval (List [Atom "list?", val]) = testValType "list?" val
eval (List [Atom "number?", val]) = testValType "number?" val
eval (List [Atom "pair?", val]) = testValType "pair?" val
eval (List [Atom "rational?", val]) = testValType "rational?" val
eval (List [Atom "string?", val]) = testValType "string?" val
eval (List [Atom "symbol?", val]) = testValType "symbol?" val
eval (List [Atom "vector?", val]) = testValType "vector?" val
eval (List (Atom func : args)) = apply func $ map eval args

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Float contents) = show contents
showVal (Number contents) = show contents
showVal (Ratio contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Atom name) = name
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector contents) = "#(" ++ unwordsList contents ++ ")"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

evaluator :: String -> IO ()
evaluator x = (print . eval . readExpr) x

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
