{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
         
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

binDigit :: Parser Char
binDigit = oneOf "01"

readBin' :: (Eq a, Num a) => ReadS a
readBin' ""     = [(0, "")]
readBin' (x:[]) = [(if x == '1' then 1 else 0, "")]
readBin' (x:xs) = [((if x == '1' then 1 else 0) + (2 * (fst $ head $ readBin' xs)), "")]

readBin :: (Eq a, Num a) => ReadS a
readBin s = readBin' $ reverse s

parseAtom :: Parser LispVal
parseAtom = do
              val <- oneOf "tf"
              return $ case val of 
                         't' -> Bool True
                         'f' -> Bool False

parseRadix :: Parser LispVal
parseRadix = do
               prefix <- oneOf "dox"
               x <- many (case prefix of
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

parseFloat :: Parser LispVal
parseFloat = do
               n <- many digit
               char '.'
               e <- many digit
               let ((f, _):_) = readFloat (n ++ "." ++ e)
               return $ Float f

parseInteger :: Parser LispVal                
parseInteger = many1 digit >>= (return . Number . read)

parseNumber :: Parser LispVal                
parseNumber = try parseFloat <|> try parseInteger

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ noneOf "\""
                     <|> try (string "\\\"" >> return '"')
                     <|> try (string "\\n" >> return '\n')
                     <|> try (string "\\r" >> return '\r')
                     <|> try (string "\\t" >> return '\t')
                     <|> try (string "\\\\" >> return '\\')
                char '"'
                return $ String x

parseLiteral :: Parser LispVal
parseLiteral = do
                 char '#'
                 x <- (parseAtom <|> parseRadix <|> parseCharLiteral)
                 return x 
                 
parseExpr :: Parser LispVal
parseExpr = parseLiteral
         <|> parseNumber
         <|> parseString
         
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right x -> "Found value"
    
main :: IO ()
main = do
          (expr:_) <- getArgs
          putStrLn (readExpr expr)
                    
