module Sintatico (sintatico) where

sintatico :: [(String, String)] -> Bool
sintatico xs = and (programa xs)

programa :: [(String, String)] -> [Bool]
programa xs = macroResult ++ inicio (drop (length macroResult) xs)
  where
    macroResult = macro xs

macro :: [(String, String)] -> [Bool]
macro xs
  | a = [a, b, c] ++ macro (drop 3 xs)
  | otherwise = []
  where
    a = snd (head xs) == "#"
    b = snd (xs !! 1) == "include"
    c = fst (xs !! 2) == "Biblioteca"

inicio :: [(String, String)] -> [Bool]
inicio xs = concat [a, b, c, d, e, f, g, h]
  where
    a = [snd (head xs) == "int"]
    b = [snd (xs !! 1) == "main"]
    c = [snd (xs !! 2) == "("]
    d = parametros (drop 3 xs)
    e = [snd (xs !! (tamanhoParametros + 3)) == ")"]
    f = [snd (xs !! (tamanhoParametros + 4)) == "{"]
    g = corpo (drop (5 + tamanhoParametros) xs)
    h = [snd (xs !! (tamanhoParametros + tamanhoCorpo + 5)) == "}"]
    tamanhoParametros = length d
    tamanhoCorpo = length g

parametros :: [(String, String)] -> [Bool]
parametros (x : y : z : zs)
  | snd x == ")" = []
  | snd y == ")" = [False]
  | snd z == ")" = [a, b]
  | snd z == "," = [a, b, c] ++ parametros zs
  | otherwise = [False]
  where
    a = fst x == "Tipo"
    b = fst y == "Variavel"
    c = snd z == ","
parametros _ = []

corpo :: [(String, String)] -> [Bool]
corpo (x : xs)
  | a = declaracaoResult ++ corpo (drop (length declaracaoResult) (x : xs))
  | b = atribuicaoResult ++ corpo (drop (length atribuicaoResult) (x : xs))
  | c = retornoResult ++ corpo (drop (length retornoResult) (x : xs))
  | otherwise = []
  where
    a = fst x == "Tipo"
    b = fst x == "Variavel"
    c = snd x == "return"
    declaracaoResult = declaracao (x : xs)
    atribuicaoResult = atribuicao (x : xs)
    retornoResult = retorno (x : xs)
corpo _ = []

declaracao :: [(String, String)] -> [Bool]
declaracao (x : y : z : zs)
  | c = [a, b, c]
  | a = a : atribuicao (y : z : zs)
  | otherwise = []
  where
    a = fst x == "Tipo"
    b = fst y == "Variavel"
    c = snd z == ";"
declaracao _ = []

atribuicao :: [(String, String)] -> [Bool]
atribuicao (x : y : z : ys)
  | not (head a) = []
  | otherwise = concat [a, b, c, d, e]
  where
    a = [fst x == "Variavel"]
    b = [snd y == "="]
    c = [numero]
    d = aritmetica ys
    e = [snd (ys !! tamanhoAritmetica) == ";"]
    tamanhoAritmetica = length d
    numero =
      fst z == "int"
        || fst z == "float"
        || fst z == "Variavel"
        || fst z == "char"
atribuicao _ = []

aritmetica :: [(String, String)] -> [Bool]
aritmetica (x : y : xs)
  | not a = []
  | otherwise = [a, b] ++ aritmetica xs
  where
    a = fst x == "Aritmetico"
    b = fst y == "int" || fst y == "float" || fst y == "Variavel"
aritmetica _ = []

retorno :: [(String, String)] -> [Bool]
retorno (x : y : z : xs)
  | not a = []
  | otherwise = [a, b, c]
  where
    a = snd x == "return"
    b = fst y == "int"
    c = snd z == ";"
retorno _ = []
