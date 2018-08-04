module Scheme.Data where

import Control.Monad.Except
import Data.Complex
import Text.ParserCombinators.Parsec

data LispVal = Atom String
             | List [LispVal]
             | Vector [LispVal]
             | DottedList [LispVal] LispVal
             | Number Float
             | Ratio Rational
             | Complex (Complex Double)
             | String String
             | Bool Bool
             deriving (Show)

--instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               deriving (Show)

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

type ThrowsError = Either LispError
