module Sintatico (sintatico) where

import Tipos

sintatico :: [Token] -> Bool
sintatico xs = and (programa xs)

programa :: [Token] -> [Bool]
programa xs = macro tokensMacro ++ inicio tokensInicio
  where
    tokensMacro = takeWhile (\x -> fst x /= "tipo") xs
    tokensInicio = drop (length tokensMacro) xs

macro :: [Token] -> [Bool]
macro [] = []
macro [_] = [False]
macro [_, _] = [False, False]
macro (x : y : z : xs) = [a, b, c] ++ macro (drop 3 xs)
  where
    a = snd x == "#"
    b = snd y == "include"
    c = fst z == "biblioteca"

inicio :: [Token] -> [Bool]
inicio xs = concat [a, b, c, d, e, f, g, h]
  where
    a = [snd (head xs) == "int"]
    b = [snd (xs !! 1) == "main"]
    c = [snd (xs !! 2) == "("]
    d = parametros (drop 3 xs)
    e = [snd (xs !! (tamanhoParametros + 3)) == ")"]
    f = [snd (xs !! (tamanhoParametros + 4)) == "{"]
    g = corpo (takeWhile (\x -> snd x /= "}") (drop (5 + tamanhoParametros) xs))
    h = [snd (xs !! (tamanhoParametros + tamanhoCorpo + 5)) == "}"]
    tamanhoParametros = length d
    tamanhoCorpo = length g

parametros :: [Token] -> [Bool]
parametros (x : y : z : zs)
  | snd x == ")" = []
  | snd y == ")" = [False]
  | snd z == ")" = [a, b]
  | snd z == "," = [a, b, c] ++ parametros zs
  | otherwise = [False]
  where
    a = fst x == "tipo"
    b = fst y == "variavel"
    c = snd z == ","
parametros _ = []

-- refatorar tudo pra cima

corpo :: [Token] -> [Bool]
corpo [] = []
corpo z
  | tipo = declaracao line ++ [ptoVirgula] ++ corpo (tail rest)
  | variavel = atribuicao line ++ [ptoVirgula] ++ corpo (tail rest)
  | retonro = retorno line ++ [ptoVirgula]
  -- verificar se existe algo depois do retorno
  | otherwise = [False]
  where
    line = takeWhile (\x -> snd x /= ";") z
    rest = drop (length line) z
    ptoVirgula = snd (head rest) == ";"
    -- dá pra tirar ?
    a = head z
    tipo = fst a == "tipo"
    variavel = fst a == "variavel"
    retonro = snd a == "return"

declaracao :: [Token] -> [Bool]
declaracao [] = []
declaracao (x : xs)
  | a = a : tipoDeclaracao xs
  | otherwise = [False]
  where
    a = fst x == "tipo"

--  <tipoDeclaracao> ::= <variável> = <operacao> "," <tipoDeclaracao>
--                     | <variável> = <operacao>
--                     | <variável> = <char>
--                     | <variável> "," <tipoDeclaracao>
--                     | <variável>

tipoDeclaracao :: [Token] -> [Bool]
tipoDeclaracao [] = [False]
tipoDeclaracao [a] = [fst a == "variavel"]
tipoDeclaracao (a : z)
  | virgulaVar = [variavel, virgulaVar] ++ tipoDeclaracao (tail z)
  | virgulaOP = [variavel, recebe] ++ operacao ateVirgula ++ [virgulaOP] ++ tipoDeclaracao depoisVirgula
  | recebe = [variavel, recebe] ++ operacao ateVirgula
  | otherwise = [False]
  where
    variavel = fst a == "variavel"
    virgulaVar = snd (head z) == ","
    recebe = snd (head z) == "="
    ateVirgula = takeWhile (\x -> snd x /= ",") (tail z)
    apartirVirgula = drop (length ateVirgula) (tail z)
    virgulaOP = not (null apartirVirgula)
    depoisVirgula = tail apartirVirgula

atribuicao :: [Token] -> [Bool]
atribuicao [] = [False]
atribuicao (x : xs)
  | variavel = [variavel, recebe] ++ operacao (tail xs)
  | otherwise = [False]
  where
    variavel = fst x == "variavel"
    recebe = snd (head xs) == "="

-- // operacao
-- <operacao> ::= <operandos>
--              | <operandos> <operador> <operacao>
--              | ( <operacao> )
--              | ( <operacao> ) <operador> <operacao>

operacao :: [Token] -> [Bool]
operacao [] = [False]
operacao (x : xs)
  | parentese1 && aritmetico2 =
    [parentese1]
      ++ operacao entreParenteses
      ++ [parentese2]
      ++ [aritmetico2]
      ++ operacao depoisAritmetico2
  | operando && aritmetico1 = [operando] ++ [aritmetico1] ++ operacao depoisAritmetico1
  | operando = [operando]
  | parentese1 = [parentese1] ++ operacao entreParenteses ++ [parentese2]
  | otherwise = [False]
  where
    operando = fst x `elem` operandos
    aritmetico1 = not (null xs) && fst (head xs) == "aritmetico"
    depoisAritmetico1 = drop 1 xs
    parentese1 = snd x == "("
    entreParenteses = take (correpondente 1 xs -1) xs
    apartirParenteses2 = drop (length entreParenteses) xs
    parentese2 = snd (head apartirParenteses2) == ")"
    depoisParentese2 = drop 1 apartirParenteses2
    aritmetico2 = not (null depoisParentese2) && fst (head depoisParentese2) == "aritmetico"
    depoisAritmetico2 = drop 1 depoisParentese2

correpondente :: Int -> [Token] -> Int
correpondente 0 _ = 0
correpondente _ [] = 0
correpondente contParenteses (atual : tokens)
  | abreParentese = 1 + correpondente (contParenteses + 1) tokens
  | fechaParentese = 1 + correpondente (contParenteses - 1) tokens
  | otherwise = 1 + correpondente contParenteses tokens
  where
    abreParentese = snd atual == "("
    fechaParentese = snd atual == ")"

operandos :: [String]
operandos = ["variavel", "int", "float", "char"]

retorno :: [Token] -> [Bool]
retorno [] = [False]
retorno [_] = [False]
-- verificar se existe algo depois do retorno
retorno (x : y ) = [a, b]
  where
    a = snd x == "return"
    b = fst (head y) == "int"
