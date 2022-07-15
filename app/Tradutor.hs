module Tradutor
  ( tradutor,
  )
where

import Tipos

tradutor :: [Token] -> String
tradutor xs = concat $ inicio tokensInicio ++ ["\nmain()\n"]
  where
    tokensInicio = dropWhile (\x -> fst x /= "tipo") xs

inicio :: [Token] -> [String]
inicio xs = [a, b, c, d, e, f, g]
  where
    a = "def "
    b = "main "
    c = "("
    d = concat $ parametros tokenParametros
    e = ")"
    f = ":\n"
    g = concat $ corpo tokenCorpo
    tokenParametros = takeWhile (\x -> snd x /= ")") (drop 3 xs)
    tokenCorpo =
      takeWhile (\x -> snd x /= "}") (drop (5 + length tokenParametros) xs)

parametros :: [Token] -> [String]
parametros [_, b] = [snd b]
parametros (_ : b : c) = [snd b, snd (head c)] ++ parametros (tail c)
parametros _ = []

corpo :: [Token] -> [String]
corpo [] = []
corpo (a : z)
  | fst a == "tipo" = tipoDeclaracao (drop 1 line) ++ corpo rest
  | fst a == "variavel" = atribuicao line ++ corpo rest
  | snd a == "return" = ["\treturn 0\n"]
  | otherwise = []
  where
    line = takeWhile (\x -> snd x /= ";") (a : z)
    rest = drop (length line) z

--  <tipoDeclaracao> ::= <variável> = <operacao> "," <tipoDeclaracao>
--                     | <variável> = <operacao>
--                     | <variável> = <char>
--                     | <variável> "," <tipoDeclaracao>
--                     | <variável>
tipoDeclaracao :: [Token] -> [String]
tipoDeclaracao (a : z)
  | virgulaVar = tipoDeclaracao (tail z)
  | virgulaOP = ["\t", variavel, recebe] ++ operacao ateVirgula ++ ["\n"] ++ tipoDeclaracao depoisVirgula
  | recebe == "=" = ["\t", variavel, recebe] ++ operacao ateVirgula ++ ["\n"]
  where
    variavel = snd a
    virgulaVar = snd (head z) == ","
    recebe = snd (head z)
    ateVirgula = takeWhile (\x -> snd x /= ",") (tail z)
    apartirVirgula = drop (length ateVirgula) (tail z)
    virgulaOP = not (null apartirVirgula)
    depoisVirgula = tail apartirVirgula
tipoDeclaracao _ = []

-- // operacao
-- <operacao> ::= <operandos>
--              | <operandos> <operador> <operacao>
--              | ( <operacao> )
--              | ( <operacao> ) <operador> <operacao>

operacao :: [Token] -> [String]
operacao (x : xs)
  | parentese1 == "(" && aritmetico2 =
    [parentese1]
      ++ operacao entreParenteses
      ++ [parentese2]
      ++ [snd (head depoisParentese2)]
      ++ operacao depoisAritmetico2
  | operando && aritmetico1 = [snd x] ++ [snd (head xs)] ++ operacao depoisAritmetico1
  | operando = [snd x]
  | parentese1 == "(" = [parentese1] ++ operacao entreParenteses ++ [parentese2]
  where
    operando = fst x `elem` operandos
    aritmetico1 = not (null xs) && fst (head xs) == "aritmetico"
    depoisAritmetico1 = drop 1 xs
    parentese1 = snd x
    entreParenteses = take (correpondente 1 xs - 1) xs
    apartirParenteses2 = drop (length entreParenteses) xs
    parentese2 = snd (head apartirParenteses2)
    depoisParentese2 = drop 1 apartirParenteses2
    aritmetico2 = not (null depoisParentese2) && fst (head depoisParentese2) == "aritmetico"
    depoisAritmetico2 = drop 1 depoisParentese2
operacao _ = []

atribuicao :: [Token] -> [String]
atribuicao (x : xs) = ["\t", variavel, recebe] ++ operacao (tail xs) ++ ["\n"]
  where
    variavel = snd x
    recebe = snd (head xs)
atribuicao _ = []

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
