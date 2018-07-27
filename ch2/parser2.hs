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
                         
{-| Literals -}

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

{-| Numbers -}

readBin' :: (Eq a, Num a) => ReadS a
readBin' ""     = [(0, "")]
readBin' (x:[]) = [(if x == '1' then 1 else 0, "")]
readBin' (x:xs) = [((if x == '1' then 1 else 0) + (2 * (fst $ head $ readBin' xs)), "")]

readBin :: (Eq a, Num a) => ReadS a
readBin s = readBin' $ reverse s

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseInteger :: Parser LispVal                
parseInteger = do
                  x <- many1 digit <|> (:) <$> char '-' <*> many1 digit
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

{-| Data Structures -}

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
parseExpr = parseAtom
            <|> parseLiteral
            <|> parseNumber
            <|> parseString
            <|> parseQuoted
            <|> parseBackquoted
            <|> parseUnquoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x
         
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right x -> "Found value: " ++ show x
    
main :: IO ()
main = do
          (expr:_) <- getArgs
          putStrLn (readExpr expr)
