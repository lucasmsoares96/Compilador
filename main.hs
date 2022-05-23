#!/usr/bin/env runhaskell
import Lexico ( lexico )

main :: IO ()
main = do
  codigo <- readFile "codigo.c"
  -- mapM_ print tokens
  mapM_ print $ lexico codigo