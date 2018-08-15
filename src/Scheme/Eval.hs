{-# LANGUAGE LambdaCase #-}

module Scheme.Eval where
  
import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra
import Data.Char
import Data.List
import Data.IORef
import Scheme.Data
import Scheme.Parser
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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
unpackStr (Number s) = return $ if s == fromInteger i then show i else show s
                       where i = round s
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

ciStringOp :: (String -> String -> Bool) -> String -> String -> Bool
ciStringOp op a b = op (map toLower a) (map toLower b)

ciStrBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
ciStrBoolBinop op args = if length args /= 2 
                         then throwError $ NumArgs 2 args
                         else do left <- unpackStr $ args !! 0
                                 right <- unpackStr $ args !! 1
                                 return $ Bool $ ciStringOp op left right

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
              ("string-ci=?", ciStrBoolBinop (==)),
              ("string-ci<?", ciStrBoolBinop (<)),
              ("string-ci>?", ciStrBoolBinop (>)),
              ("string-ci<=?", ciStrBoolBinop (<=)),
              ("string-ci>=?", ciStrBoolBinop (>=)),
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

testValType :: Env -> String -> LispVal -> IOThrowsError LispVal
testValType env "boolean?" val = do
  result <- eval env val
  case result of
     Bool _    -> return $ Bool True
     otherwise -> return $ Bool False 
testValType env "char?" val = do
  result <- eval env val
  case result of
     String [_]  -> return $ Bool True
     otherwise -> return $ Bool False 
testValType env "complex?" val = do
  result <- eval env val
  case result of
     Complex _  -> return $ Bool True
     otherwise -> return $ Bool False 
testValType env "integer?" val = do
  result <- eval env val
  case result of
     Number x  -> return $ Bool $ x == fromInteger (round x)
     otherwise -> return $ Bool False 
testValType env "list?" val = do
  result <- eval env val
  case result of
     List _  -> return $ Bool True
     otherwise -> return $ Bool False 
testValType env "pair?" val = do
  result <- eval env val
  case result of
     DottedList _ _ -> return $ Bool True
     otherwise -> return $ Bool False 
testValType env "rational?" val = do
  result <- eval env val
  case result of
     Ratio _  -> return $ Bool True
     otherwise -> return $ Bool False 
testValType env "real?" val = do
  result <- eval env val
  case result of
     Number _  -> return $ Bool True
     otherwise -> return $ Bool False 
testValType env "symbol?" val = do
  result <- eval env val
  case result of
     Atom _  -> return $ Bool True
     otherwise -> return $ Bool False 
testValType env "string?" val = do
  result <- eval env val
  case result of
     String _  -> return $ Bool True
     otherwise -> return $ Bool False 
testValType env "vector?" val = do
  result <- eval env val
  case result of
     Vector _  -> return $ Bool True
     otherwise -> return $ Bool False 
testValType env _ _ = return $ Bool False

listToString :: LispVal -> ThrowsError LispVal
listToString (List [Atom "quote", val]) = listToString val
listToString (List val) = do
                            strs <- mapM unpackStr val
                            return $ String $ concat $ strs

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Ratio _) = return val
eval env val@(Complex _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val

{-| Type functions -}
eval env (List [Atom "boolean?", val]) = testValType env "boolean?" val
eval env (List [Atom "char?", val]) = testValType env "char?" val
eval env (List [Atom "complex?", val]) = testValType env "complex?" val
eval env (List [Atom "integer?", val]) = testValType env "integer?" val
eval env (List [Atom "list?", val]) = testValType env "list?" val
eval env (List [Atom "pair?", val]) = testValType env "pair?" val
eval env (List [Atom "rational?", val]) = testValType env "rational?" val
eval env (List [Atom "real?", val]) = testValType env "real?" val
eval env (List [Atom "string?", val]) = testValType env "string?" val
eval env (List [Atom "symbol?", val]) = testValType env "symbol?" val
eval env (List [Atom "vector?", val]) = testValType env "vector?" val

{-| String functions -}
eval env (List [Atom "symbol->string", val]) = do
  result <- eval env val
  case result of
    Atom x -> return $ String x
    badForm  -> throwError $ BadSpecialForm "Argument is not a symbol" badForm
    
eval env (List [Atom "string->symbol", val]) = do
  result <- eval env val
  case result of
    String x -> return $ Atom x
    badForm  -> throwError $ BadSpecialForm "Argument is not a string" badForm
    
eval env (List [Atom "make-string", val1, val2]) = do
  num <- eval env val1
  chr <- eval env val2
  case num of
    Number n -> case chr of
                   String c -> return $ String $ take (round n) [(head c), (head c)..]
                   badForm  -> throwError $ BadSpecialForm "Second argument is not a string" badForm
    badForm  -> throwError $ BadSpecialForm "First argument is not a number" badForm
    
eval env (List [Atom "string-length", val]) = do
  result <- eval env val
  case result of
    String x -> return $ Number $ fromIntegral $ length x
    badForm  -> throwError $ BadSpecialForm "Argument is not a string" badForm
    
eval env (List [Atom "string-ref", val, k]) =do
  str <- eval env val
  num <- eval env k
  case str of
    String s -> case num of
                   Number n -> return $ String $ [s !! (round n)]
                   badForm  -> throwError $ BadSpecialForm "Second argument is not a number" badForm
    badForm  -> throwError $ BadSpecialForm "First argument is not a string" badForm

eval env (List [Atom "string-set!", val, k, c]) = do
  str1 <- eval env val
  num  <- eval env k
  str2 <- eval env c
  case str1 of
    String s1 -> case num of
                   Number n -> case str2 of
                                 String s2 -> return $ String $ take (round n) s1 ++ s2 ++ drop ((round n) + 1) s1
                                 badForm  -> throwError $ BadSpecialForm "Third argument is not a string" badForm
                   badForm  -> throwError $ BadSpecialForm "Second argument is not a number" badForm
    badForm  -> throwError $ BadSpecialForm "First argument is not a string" badForm
   
eval env (List [Atom "substring", val, s, e]) = do
  str  <- eval env val
  num1 <- eval env s
  num2 <- eval env e
  case str of
    String s -> case num1 of
                  Number n1 -> case num2 of
                                Number n2 -> return $ String $ take ((round n2) - (round n1) + 1)
                                                                    (drop (round n1) s)
                                badForm  -> throwError $ BadSpecialForm "Third argument is not a number" badForm
                  badForm   -> throwError $ BadSpecialForm "Second argument is not a number" badForm
    badForm  -> throwError $ BadSpecialForm "First argument is not a string" badForm
  
eval env (List [Atom "string-append", val1, val2]) = do
  str1 <- eval env val1
  str2 <- eval env val2
  case str1 of
    String s1 -> case str2 of
                   String s2 -> return $ String $ s1 ++ s2
                   badForm  -> throwError $ BadSpecialForm "Second argument is not a string" badForm
    badForm  -> throwError $ BadSpecialForm "First argument is not a string" badForm
    
eval env (List [Atom "string->list", val]) = do
  result <- eval env val
  case result of
    String x -> return $ List $ map (\c -> String [c]) x
    badForm  -> throwError $ BadSpecialForm "Argument is not a symbol" badForm
    
eval env (List [Atom "string-copy", val]) =  do
  result <- eval env val
  case result of
    String x -> return $ String x
    badForm  -> throwError $ BadSpecialForm "Argument is not a symbol" badForm
    
eval env (List [Atom "string-fill!", val, c]) =  do
  str1 <- eval env val
  str2 <- eval env c
  case str1 of
    String s1 -> case str2 of
                   String s2 -> return $ String $ take (length s1) (repeat (head s2))
                   badForm  -> throwError $ BadSpecialForm "Second argument is not a string" badForm
    badForm  -> throwError $ BadSpecialForm "First argument is not a string" badForm
    
eval env (List [Atom "list->string", val]) = liftThrows $ listToString val

eval env (List [Atom "if", pred, conseq, alt]) = do
     result <- eval env pred
     case result of
       Bool False -> eval env alt
       otherwise  -> eval env conseq

eval env (List [Atom "only-if", pred, conseq, alt]) = do
     result <- eval env pred
     case result of
       Bool True -> eval env conseq
       otherwise -> eval env alt

eval env (List (Atom "cond" : clauses)) = do
  result <- findM testClause clauses
  case result of
    Just x  -> return $ case unpackList x of
                          Left err -> Bool False
                          Right (x : xs) -> xs !! 0
    Nothing -> return $ Bool False
   where testClause (List (x : xs)) = do
           result <- eval env x
           case result of
             Bool True -> return $ True
             otherwise -> return $ False                              
               
eval env (List (Atom "case" : key : cases)) = do
  case find testCase cases of
    Just x  -> return $ case unpackList x of
                          Left err -> Bool False
                          Right (x : xs) -> xs !! 0
    Nothing -> return $ Bool False
  where testTest t = case eqv [key, t] of
                       Right (Bool True) -> True
                       otherwise -> False
        testCase (List ((Atom "else") : xs)) = True
        testCase (List ((List x) : xs)) =
           case find testTest x of
              Just _  -> True
              Nothing -> False                              

eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
     
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var

eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
