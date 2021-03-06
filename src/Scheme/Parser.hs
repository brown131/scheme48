module Scheme.Parser where
  
import Control.Monad.Except
import Data.Complex
import Data.Ratio
import Numeric
import Scheme.Data
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Numeric

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow $ endBy parseExpr spaces

{-| Parse Empty -}

parseEmpty :: Parser LispVal
parseEmpty = do
               skipMany1 space
               try parseExpr

parseComment :: Parser LispVal
parseComment = do
                 char ';'
                 manyTill anyChar (try (oneOf "\n\r"))
                 try parseExpr
               
{-| Parse Symbols -}

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

readBin :: (Eq a, Num a) => ReadS a
readBin s = readBin' $ reverse s
  where readBin' ""     = [(0, "")]
        readBin' (x:[]) = [(if x == '1' then 1 else 0, "")]
        readBin' (x:xs) = [((if x == '1' then 1 else 0) + (2 * (fst $ head $ readBin' xs)), "")]

parseInteger :: Parser LispVal                
parseInteger = do
                  x <- many1 digit <|> (:) <$> char '-' <*> many1 digit
                  notFollowedBy $ char '.' 
                  return $ Number $ fromIntegral $ read x

parseFloating :: Parser LispVal
parseFloating = do
                  f <- sign <*> (floating3 True)
                  return $ Number f

parseRatio :: Parser LispVal
parseRatio = do
               n <- many1 digit
               char '/'
               d <- many1 digit
               return $ Ratio ((read n) % (read d))

parseComplex :: Parser LispVal
parseComplex = do
                 x <- (try parseFloating <|> parseInteger)
                 char '+'
                 y <- (try parseFloating <|> parseInteger)
                 char 'i' 
                 return $ Complex (toDouble x :+ toDouble y)
                 where toDouble(Number n) = realToFrac n
                   
parseNumber :: Parser LispVal                
parseNumber = try parseRatio <|> try parseComplex <|> try parseInteger <|> try parseFloating

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

parseSExpr :: Parser LispVal
parseSExpr = do
               char '('
               x <- try parseList <|> try parseDottedList
               char ')'
               return x
                
parseExpr :: Parser LispVal
parseExpr = parseEmpty
            <|> parseComment
            <|> parseLiteral
            <|> parseNumber
            <|> parseString
            <|> parseQuoted
            <|> parseBackquoted
            <|> parseUnquoted
            <|> parseAtom
            <|> parseSExpr
            <|> parseEmpty
