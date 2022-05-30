module Main where

import Lexico (lexico)
import Sintatico (sintatico)
import Semantico (semantico)

main :: IO ()
main = do
  codigo <- readFile "codigo.c"
  let tokens = lexico codigo
  let parse = sintatico tokens
  let semantic = semantico tokens
  
  -- mapM_ print tokens
  -- mapM_ print (sintatico tokens)

  print ("Analise Sintatica: " ++ show parse)
  if parse
    then
      print ("Analise Semantica: " ++ show semantic)
    else
      print ""