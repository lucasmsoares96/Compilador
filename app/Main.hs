module Main where
import System.Environment

import Lexico (lexico)
import Sintatico (sintatico, programa)
import Semantico (semantico)

main :: IO ()
main = do
  args <- getArgs
  codigo <- readFile (head args)
  let tokens = lexico codigo
  let parse = sintatico tokens
  let semantic = semantico tokens
  -- codigo <- readFile "1/Código_01.c"
  -- mapM_ putStrLn args
  -- putStrLn codigo
  -- mapM_ print tokens
  -- mapM_ print (programa tokens)

  print ("Analise Sintatica: " ++ show parse)
  if parse
    then
      print ("Analise Semantica: " ++ show semantic)
    else
      print ""
      