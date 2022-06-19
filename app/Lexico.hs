module Lexico (lexico) where

import Data.Char (isDigit)

lexico :: String -> [(String, String)]
lexico c = catalogar $ filter (`notElem` espacos) $ combinar $ concatMap separar $ palavras c

catalogar :: [String] -> [(String, String)]
catalogar [] = []
catalogar (x : xs)
  | head x == '\"' && last x == '\"' = ("char", x) : catalogar xs
  | head x == '<' && last x == '>' = ("biblioteca", x) : catalogar xs
  | x `elem` delimitador = ("delimitador", x) : catalogar xs
  | x `elem` atribuicao = ("atribuicao", x) : catalogar xs
  | x `elem` aritmetico = ("aritmetico", x) : catalogar xs
  | x `elem` pontuacao = ("pontuacao", x) : catalogar xs
  | x `elem` comando = ("comando", x) : catalogar xs
  | x `elem` comentarios = catalogar (dropWhile (/= "\n") xs)
  | x `elem` logico = ("logico", x) : catalogar xs
  | all isDigit x = ("int", x) : catalogar xs
  | x `elem` tipos = ("tipo", x) : catalogar xs
  | isfloat x = ("float", x) : catalogar xs
  | otherwise = ("variavel", x) : catalogar xs

combinar :: [String] -> [String]
combinar [] = []
combinar (x : xs)
  | x == "\"" = concat (x : primeiraAspas) : combinar (drop tamanhoAspas xs)
  | x == "/" && head xs == "/" = combinar (dropWhile (/= "\n")  xs)
  | x == "<" = concat (x : primeiroMenor) : combinar (drop tamanhoMenor xs)
  | x == ">" && head xs == "=" = (x ++ head xs): combinar (drop 1 xs)
  | x == "=" && head xs == "=" = (x ++ head xs): combinar (drop 1 xs)
  | x == "!" && head xs == "=" = (x ++ head xs): combinar (drop 1 xs)
  | all isDigit x = concat (x : primeirofloat) : combinar (drop tamanhofloat xs)
  | otherwise = x : combinar xs
  where
    primeirofloat = juntarfloat xs
    tamanhofloat = length primeirofloat
    primeiraAspas = juntarAspas xs
    tamanhoAspas = length primeiraAspas
    primeiroMenor = juntarMenor xs
    tamanhoMenor = length primeiroMenor

separar :: String -> [String]
separar "" = []
separar s
  | length s == 1 = [s]
  | otherwise = init s : [[last s]]

palavra :: String -> String
palavra "" = ""
palavra (x : xs)
  | x `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] = x : palavra xs
  | otherwise = [x]

palavras :: String -> [String]
palavras "" = []
palavras x = primeira : palavras (drop tamanho x)
  where
    primeira = palavra x
    tamanho = length primeira

juntarfloat :: [String] -> [String]
juntarfloat [] = []
juntarfloat (x : xs)
  | x == "." && all isDigit (head xs) = x : [head xs]
  | otherwise = []

juntarAspas :: [String] -> [String]
juntarAspas [] = []
juntarAspas (x : xs)
  | x /= "\"" = x : juntarAspas xs
  | otherwise = [x]

juntarMenor :: [String] -> [String]
juntarMenor [] = []
juntarMenor (x : xs)
  | x == "=" = [x]
  | x /= ">" = x : juntarMenor xs
  | otherwise = [x]

aritmetico :: [String]
aritmetico = ["+", "-", "*", "/", "%"]

logico :: [String]
logico = ["!=", "<=", ">=", "<", ">", "=="]

delimitador :: [String]
delimitador = ["(", ")", "{", "}", "[", "]"]

atribuicao :: [String]
atribuicao = ["="]

pontuacao :: [String]
pontuacao = [",",".", ";"]

tipos :: [String]
tipos = ["char", "float", "int"]

comando :: [String]
comando = ["#", "include", "while", "if", "do", "return"]

comentarios :: [String]
comentarios = ["//"]

espacos :: [String]
espacos = [" ", "\t", "\n", "\r"]

isfloat :: String -> Bool
isfloat xs
  | head fim == '.' && all isDigit (tail fim) = True
  | otherwise = False
  where
    fim = dropWhile isDigit xs