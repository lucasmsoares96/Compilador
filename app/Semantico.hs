module Semantico (semantico) where

import qualified Data.Maybe

semantico :: [(String, String)] -> Bool
semantico tokens =
  unica lista
    && and (atribuicao novosTokens)
  where
    lista = declaracao tokens
    novosTokens = variavelPtipo tokens lista

variavelPtipo :: [(String, String)] -> [(String, String)] -> [(String, String)]
variavelPtipo [] _ = []
variavelPtipo (token : tokens) variaveis
  -- quando forma uma variavel e não uma funcao
  | fst token == "variavel"
      && snd (head tokens) /= "(" =
    a : variavelPtipo tokens variaveis
  | otherwise = token : variavelPtipo tokens variaveis
  where
    a = findValue (snd token) variaveis

atribuicao :: [(String, String)] -> [Bool]
atribuicao (x : y : xs)
  | snd y == "=" =
    mesmoTipo :
    atribuicao (drop (length sentenca + 1) (x : y : xs))
  | otherwise = atribuicao (y : xs)
  where
    tipo = fst x
    sentenca = takeWhile (\x -> snd x /= "," && snd x /= ";") (x : y : xs)
    mesmoTipo =
      foldl
        -- (\acc x -> if fst x /= tipo then False else acc)
        (\acc x -> fst x == tipo && acc)
        True
        operandos
    operandos =
      filter
        ( \x ->
            fst x /= "aritmetico"
              && fst x /= "atribuicao"
              && fst x /= "delimitador"
        )
        sentenca
atribuicao [_] = []
atribuicao [] = []

-- [(tipo,nome)]
declaracao :: [(String, String)] -> [(String, String)]
declaracao [] = []
declaracao [_] = []
declaracao [_, _] = []
declaracao (x : y : z : xs)
  | variavel = variaveisLinha tipo linha ++ declaracao resto
  | otherwise = declaracao (y : z : xs)
  where
    -- não é uma função
    variavel = fst x == "tipo" && snd z /= "("
    linha = takeWhile (\x -> snd x /= ";") (y : z : xs)
    resto = drop (length linha + 1) (y : z : xs)
    tipo = snd x

variaveisLinha :: String -> [(String, String)] -> [(String, String)]
variaveisLinha _ [] = []
variaveisLinha tipo (x : xs)
  | fst x == "variavel" = (tipo, nome) : variaveisLinha tipo xs
  | otherwise = variaveisLinha tipo xs
  where
    nome = snd x

unica :: [(String, String)] -> Bool
unica [] = True
unica (x : xs)
  | findValue (snd x) xs == ("", "") = unica xs
  | otherwise = False

findValue :: Foldable t => String -> t (String, String) -> (String, String)
findValue value =
  Data.Maybe.fromMaybe ("", "")
    . foldr
      ( \(k, v) acc ->
          if value == v
            then Just (k, v)
            else acc
      )
      Nothing