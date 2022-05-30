module Lexico (lexico) where
import Data.Char (isDigit)

lexico :: [Char] -> [(String, String)]
lexico c = catalogar $ filter (`notElem` [" ", "\t", "\n"]) $ combinar $ concatMap separar $ palavras c

catalogar :: [String] -> [(String, String)]
catalogar [] = []
catalogar (x : xs)
  | head x == '\'' && last x == '\'' = ("char", x) : catalogar xs
  | head x == '<' && last x == '>' = ("Biblioteca", x) : catalogar xs
  | x `elem` delimitador = ("Delimitador", x) : catalogar xs
  | x `elem` atribuicao = ("Atribuicao", x) : catalogar xs
  | x `elem` aritmetico = ("Aritmetico", x) : catalogar xs
  | x `elem` pontuacao = ("Pontuacao", x) : catalogar xs
  | x `elem` comandos = ("Comandos", x) : catalogar xs
  | x `elem` logico = ("Logico", x) : catalogar xs
  | all isDigit x = ("int", x) : catalogar xs
  | x `elem` tipos = ("Tipo", x) : catalogar xs
  | isfloat x = ("float", x) : catalogar xs
  | otherwise = ("Variavel", x) : catalogar xs

combinar :: [String] -> [String]
combinar [] = []
combinar (x : xs)
  | x == "\'" = concat (x : primeiraAspas) : combinar (drop tamanhoAspas xs)
  | x == "<" = concat (x : primeiroMenor) : combinar (drop tamanhoMenor xs)
  | x == ">" = concat (x : primeiroMaior) : combinar (drop tamanhoMaior xs)
  | x == "=" = concat (x : primeiroIgual) : combinar (drop tamanhoIgual xs)
  | x == "!" = concat (x : primeiroExclama) : combinar (drop tamanhoExclama xs)
  | all isDigit x = concat (x : primeirofloat) : combinar (drop tamanhofloat xs)
  | otherwise = x : combinar xs
  where
    primeirofloat = juntarfloat xs
    tamanhofloat = length primeirofloat
    primeiroExclama = juntarExclama xs
    tamanhoExclama = length primeiroExclama
    primeiroIgual = juntarIgual xs
    tamanhoIgual = length primeiroIgual
    primeiroMaior = juntarMaior xs
    tamanhoMaior = length primeiroMaior
    primeiroMenor = juntarMenor xs
    tamanhoMenor = length primeiroMenor
    primeiraAspas = juntarAspas xs
    tamanhoAspas = length primeiraAspas

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

palavras :: [Char] -> [String]
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

juntarExclama :: [String] -> [String]
juntarExclama [] = []
juntarExclama (x : xs)
  | x == "=" = [x]
  | otherwise = []

juntarIgual :: [String] -> [String]
juntarIgual [] = []
juntarIgual (x : xs)
  | x == "=" = [x]
  | otherwise = []

juntarAspas :: [String] -> [String]
juntarAspas [] = []
juntarAspas (x : xs)
  | x /= "\'" = x : juntarAspas xs
  | otherwise = [x]

juntarMaior :: [String] -> [String]
juntarMaior [] = []
juntarMaior (x : xs)
  | x == "=" = [x]
  | otherwise = []

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
pontuacao = [".", ";"]

tipos :: [String]
tipos = ["char", "float", "int"]

comandos :: [String]
comandos = ["#", "include", "while", "if", "do", "return"]

isfloat :: String -> Bool
isfloat xs
  | head fim == '.' && all isDigit (tail fim) = True
  | otherwise = False
  where
     fim = dropWhile isDigit xs