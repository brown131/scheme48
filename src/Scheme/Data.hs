module Scheme.Data where

import Control.Monad.Except
import Data.Complex
import Data.IORef
import System.IO
import Text.ParserCombinators.Parsec

type Env = IORef [(String, IORef LispVal)]

data LispVal = Atom String
             | List [LispVal]
             | Vector [LispVal]
             | DottedList [LispVal] LispVal
             | Number Float
             | Ratio Rational
             | Complex (Complex Double)
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
               
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val
