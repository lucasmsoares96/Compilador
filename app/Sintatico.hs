module Sintatico (sintatico, programa) where

sintatico :: [(String, String)] -> Bool
sintatico xs = and (programa xs)

programa :: [(String, String)] -> [Bool]
programa xs = macro tokensMacro ++ inicio tokensInicio
  where
    tokensMacro = takeWhile (\x -> fst x /= "tipo") xs
    tokensInicio = drop (length tokensMacro) xs


macro :: [(String, String)] -> [Bool]
macro [] = []
macro [_] = [False]
macro [_,_] = [False,False]
macro (x : y : z : xs) = [a, b, c] ++ macro (drop 3 xs)
  where
    a = snd x == "#"
    b = snd y == "include"
    c = fst z == "biblioteca"

inicio :: [(String, String)] -> [Bool]
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

parametros :: [(String, String)] -> [Bool]
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

corpo :: [(String, String)] -> [Bool]
corpo [] = []
corpo xs
  | tipo = declaracao line ++ [ptoVirgula] ++ corpo (tail rest)
  | variavel = atribuicao line ++ [ptoVirgula] ++ corpo (tail rest)
  | retonro = retorno line ++ [ptoVirgula]
  -- verificar o fim
  | otherwise = [False]
  where
    line = takeWhile (\x -> snd x /= ";") xs
    rest = drop (length line) xs
    ptoVirgula = snd (head rest) == ";"
    -- dá pra tirar ?
    x = head xs
    tipo = fst x == "tipo"
    variavel = fst x == "variavel"
    retonro = snd x == "return"

declaracao :: [(String, String)] -> [Bool]
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

tipoDeclaracao :: [(String, String)] -> [Bool]
tipoDeclaracao [] = [False]
tipoDeclaracao [x] = [fst x == "variavel"]
tipoDeclaracao (x : xs)
  | virgulaVar = [variavel, virgulaVar] ++ tipoDeclaracao (tail xs)
  | virgulaOP = [variavel, recebe] ++ operacao ateVirgula ++ [virgulaOP] ++ tipoDeclaracao depoisVirgula
  | recebe = [variavel, recebe] ++ operacao ateVirgula
  | otherwise = [False]
  where
    variavel = fst x == "variavel"
    virgulaVar = snd (head xs) == ","
    recebe = snd (head xs) == "="
    ateVirgula = takeWhile (\x -> snd x /= ",") (tail xs)
    apartirVirgula = drop (length ateVirgula) (tail xs)
    virgulaOP = not (null apartirVirgula)
    depoisVirgula = tail apartirVirgula

atribuicao :: [(String, String)] -> [Bool]
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

operacao :: [(String, String)] -> [Bool]
operacao [] = [False]
operacao (x : xs)
  | parentese1 && aritmetico2 = [parentese1] ++ operacao entreParenteses ++ [parentese2] ++ [aritmetico2] ++ operacao depoisAritmetico2
  | operando && aritmetico1 = [operando] ++ [aritmetico1] ++ operacao depoisAritmetico1
  | operando = [operando]
  | parentese1 = [parentese1] ++ operacao entreParenteses ++ [parentese2]
  | otherwise = [False]
  where
    operando = fst x `elem` operandos
    aritmetico1 = not (null xs) && fst (head xs) == "aritmetico"
    depoisAritmetico1 = drop 1 xs
    parentese1 = snd x == "("
    -- entreParenteses = takeWhile (\x -> snd x /= ")") xs
    entreParenteses = take (correpondente 1 xs -1) xs
    apartirParenteses2 = drop (length entreParenteses) xs
    parentese2 = snd (head apartirParenteses2) == ")"
    depoisParentese2 = drop 1 apartirParenteses2
    aritmetico2 = not (null depoisParentese2) && fst (head depoisParentese2) == "aritmetico"
    depoisAritmetico2 = drop 1 depoisParentese2

correpondente :: Int -> [(String, String)] -> Int
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

retorno :: [(String, String)] -> [Bool]
retorno [] = [False]
retorno [_] = [False]
retorno (x : y : xs) = [a, b]
  where
    a = snd x == "return"
    b = fst y == "int"
