{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Except
import Data.Complex
import Data.Ratio
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Numeric

data LispVal = Atom String
             | List [LispVal]
             | Vector [LispVal]
             | DottedList [LispVal] LispVal
             | Number Float
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

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

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

unpackNum :: LispVal -> ThrowsError Float
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

numericBinop :: (Float -> Float -> Float) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

integralOp :: Integral a => (a -> a -> a) -> Float -> Float -> Float
integralOp op a b = fromIntegral $ op (floor a) (floor b)

integralBinop :: Integral a => (a -> a -> a) -> [LispVal] -> ThrowsError LispVal
integralBinop op           []  = throwError $ NumArgs 2 []
integralBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
integralBinop op params        = mapM unpackNum params >>= return . Number . foldl1 (integralOp op)

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                   (all equalPair $ zip arg1 arg2)
     where equalPair (x1, x2) = case equal [x1, x2] of
                                  Left err -> False
                                  Right (Bool val) -> val
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop (/)),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("mod", integralBinop mod),
              ("quotient", integralBinop quot),
              ("remainder", integralBinop rem),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)
             
testValType :: String -> LispVal -> ThrowsError LispVal
testValType "boolean?" (Bool _) = return $ Bool True
testValType "char?" (String [_]) = return $ Bool True
testValType "complex?" (Complex _) = return $ Bool True
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
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
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
     
