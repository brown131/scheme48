{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Except
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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

type ThrowsError = Either LispError

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

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)
             
testValType :: String -> LispVal -> ThrowsError LispVal
testValType "boolean?" (Bool _) = return $ Bool True
testValType "char?" (String [_]) = return $ Bool True
testValType "complex?" (Complex _) = return $ Bool True
testValType "float?" (Float _) = return $ Bool True
testValType "list?" (List _) = return $ Bool True
testValType "number?" (Number _) = return $ Bool True
testValType "pair?" (DottedList _ _) = return $ Bool True
testValType "rational?" (Ratio _) = return $ Bool True
testValType "symbol?" (Atom _) = return $ Bool True
testValType "string?" (String _) = return $ Bool True
testValType "vector?" (Vector _) = return $ Bool True
testValType _ _ = return $ Bool False

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Float _) = return val
eval val@(Number _) = return val
eval val@(Ratio _) = return val
eval val@(Complex _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "symbol->string", Atom val]) = return $ String val
eval (List [Atom "string->symbol", String val]) = return $ Atom val
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
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

scheme :: String -> IO ()
scheme x = do
             evaled <- return $ liftM show $ readExpr x >>= eval
             putStrLn $ extractValue $ trapError evaled

main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled
