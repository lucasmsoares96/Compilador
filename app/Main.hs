module Main where

import Lexico (lexico)
import Semantico (semantico)
import Sintatico (sintatico)
import Tradutor (tradutor)
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  let nomeC = head args
  codigo <- readFile nomeC
  -- mapM_ putStrLn args
  -- codigo <- readFile "./Código_01.c"
  -- let nomeC = "Código_01.c"
  -- putStrLn codigo
  let tokens = lexico codigo
  -- mapM_ print tokens
  -- mapM_ print (programa tokens)
  let parse = sintatico tokens
  let semantic = semantico tokens
  let nomePy = takeWhile (/= '.') nomeC ++ ".py"
  let python = tradutor tokens

  print ("Analise Sintatica: " ++ show parse)
  if parse
    then print ("Analise Semantica: " ++ show semantic)
    else print ""

  writeFile nomePy python