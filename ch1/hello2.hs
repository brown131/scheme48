module Main where
import System.Environment

calc :: String -> Int -> Int -> Int
calc "+" n1 n2 = n1 + n2
calc "-" n1 n2 = n1 - n2
calc "*" n1 n2 = n1 * n2
calc "/" n1 n2 = quot n1 n2
  
main :: IO ()
main = do
    args <- getArgs
    let num1 = read (args !! 0) :: Int
    let op   = read ("\"" ++ (args !! 1) ++ "\"") :: String
    let num2 = read (args !! 2) :: Int
    let ans  = calc op num1 num2
    putStrLn ((show num1) ++ " " ++ op ++ " " ++ (show num2) ++ " = " ++ (show ans))
    
   
    
