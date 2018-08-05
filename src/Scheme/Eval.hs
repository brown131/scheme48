{-# LANGUAGE LambdaCase #-}

module Scheme.Eval where
  
import Control.Monad
import Control.Monad.Except
import Data.Char
import Data.List
import Scheme.Data
import Scheme.Parser
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

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

unpackList :: LispVal -> ThrowsError [LispVal]
unpackList (List x) = return x
unpackList notList  = throwError $ TypeMismatch "list" notList

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
integralOp op a b = fromIntegral $ op (round a) (round b)

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
testValType "integer?" (Number x) = return $ Bool $ x == fromInteger (round x)
testValType "list?" (List _) = return $ Bool True
testValType "pair?" (DottedList _ _) = return $ Bool True
testValType "rational?" (Ratio _) = return $ Bool True
testValType "real?" (Number _) = return $ Bool True
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

{-| Type functions -}
eval (List [Atom "boolean?", val]) = testValType "boolean?" val
eval (List [Atom "char?", val]) = testValType "char?" val
eval (List [Atom "complex?", val]) = testValType "complex?" val
eval (List [Atom "integer?", val]) = testValType "integer?" val
eval (List [Atom "list?", val]) = testValType "list?" val
eval (List [Atom "pair?", val]) = testValType "pair?" val
eval (List [Atom "rational?", val]) = testValType "rational?" val
eval (List [Atom "real?", val]) = testValType "real?" val
eval (List [Atom "string?", val]) = testValType "string?" val
eval (List [Atom "symbol?", val]) = testValType "symbol?" val
eval (List [Atom "vector?", val]) = testValType "vector?" val

{-| String functions -}
eval (List [Atom "symbol->string", Atom val]) = return $ String val
eval (List [Atom "string->symbol", String val]) = return $ Atom val
eval (List [Atom "make-string", Number n, String c]) = return $ String $ take (round n) [(head c), (head c)..]
eval (List [Atom "string-length", String val]) = return $ Number $ fromIntegral $ length val
eval (List [Atom "string-ref", String val, Number k]) = return $ String $ [val !! (round k)]
eval (List [Atom "string-set!", String val, Number k, String c]) =
  return $ String $ take (round k) val ++ c ++ drop ((round k) + 1) val
eval (List [Atom "string-<?", String val1, String val2]) = return $ Bool $ val1 < val2
eval (List [Atom "string->?", String val1, String val2]) = return $ Bool $ val1 > val2
eval (List [Atom "string-<=?", String val1, String val2]) = return $ Bool $ val1 <= val2
eval (List [Atom "string->=?", String val1, String val2]) = return $ Bool $ val1 >= val2
eval (List [Atom "string-ci<?", String val1, String val2]) =
  return $ Bool $ (map toLower val1) < (map toLower val2)
eval (List [Atom "string-ci>?", String val1, String val2]) =
  return $ Bool $ (map toLower val1) > (map toLower val2)
eval (List [Atom "string-ci<=?", String val1, String val2]) = 
  return $ Bool $ (map toLower val1) <= (map toLower val2)
eval (List [Atom "string-ci>=?", String val1, String val2]) =
  return $ Bool $ (map toLower val1) >= (map toLower val2)
eval (List [Atom "substring", String val, Number s, Number e]) =
  return $ String $ take ((round e) - (round s) + 1) (drop (round s) val)
eval (List [Atom "string-append", String val1, String val2]) = return $ String $ val1 ++ val2
eval (List [Atom "string->list", String val]) = return $ List $ map (\c -> String [c]) val
eval (List [Atom "string-copy", String val]) = return $ String $ val
eval (List [Atom "string-fill!", String val, String c]) = return $ String $ take (length val) (repeat (head c))
-- TODO Doesn't take quoted list.
eval (List [Atom "list->string", List val]) =
  return $ String $ concat $ map (\case 
                                    String c -> c
                                    x -> show (eval x)) val

eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq

eval (List (Atom "cond" : clauses)) =
     do
        case find testClause clauses of
          Just x  -> return $ case unpackList x of
                                Left err -> Bool False
                                Right (x : xs) -> xs !! 0
          Nothing -> return $ Bool False
        where testClause (List (x : xs)) =
                case eval x of
                  Right (Bool True) -> True
                  otherwise -> False                              
               
eval (List (Atom "case" : keyVal : cases)) =
     do
        case find testCase cases of
          Just x  -> return $ case unpackList x of
                                Left err -> Bool False
                                Right (x : xs) -> xs !! 0
          Nothing -> return $ Bool False
        where key = case eval keyVal of
                      Right x -> x
                      Left err -> Bool False
                      
              testTest t = case eqv [key, t] of
                 Right (Bool True) -> True
                 otherwise -> False

              testCase (List ((Atom "else") : xs)) = True
              testCase (List ((List x) : xs)) =
                case find testTest x of
                  Just _  -> True
                  Nothing -> False                              
  
eval (List (Atom func : args)) = mapM eval args >>= apply func

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
