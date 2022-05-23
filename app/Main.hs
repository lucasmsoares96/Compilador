module Main where
import Lexico ( lexico )
import Sintatico (sintatico)

main :: IO ()
main = do
  codigo <- readFile "codigo.c"
  let tokens = lexico codigo
  print $ sintatico tokens