module Semantico (semantico) where

import qualified Data.Maybe

semantico :: [(String, String)] -> Bool
semantico tokens = unica lista && and(atribuicao (variavelPtipo tokens lista))
  where
    lista = variaveis tokens

variavelPtipo :: [(String, String)] -> [(String, String)] -> [(String, String)]
variavelPtipo [] _ = []
variavelPtipo (x : xs) ys
  | fst x == "Variavel" && snd (head xs) /= "(" =
    a : variavelPtipo xs ys
  | otherwise = x : variavelPtipo xs ys
  where
    a = findValue (snd x) ys

atribuicao :: [(String, String)] -> [Bool]
atribuicao (x : y : xs)
  | snd y == "=" =
    foldl
      -- (\acc x -> if fst x /= tipo then False else acc)
      (\acc x -> fst x == tipo && acc)
      True
      operandos :
    atribuicao (drop (length sentenca) (x : y : xs))
  | otherwise = atribuicao (y : xs)
  where
    tipo = fst x
    sentenca = takeWhile (\x -> snd x /= ";") (x:y:xs)
    operandos =
      filter
        (\x -> fst x /= "Aritmetico" && fst x /= "Atribuicao")
        sentenca
atribuicao [_] = []
atribuicao [] = []

variaveis :: [(String, String)] -> [(String, String)]
variaveis [] = []
variaveis tokens
  | x == "Tipo" && c /= "(" = (a, b) : variaveis (drop 2 tokens)
  | otherwise = variaveis (drop 1 tokens)
  where
    x = fst (head tokens)
    a = snd (head tokens)
    b = snd (tokens !! 1)
    c = snd (tokens !! 2)

unica :: [(String, String)] -> Bool
unica [] = True
unica (x : xs)
  | findValue (snd x) xs == ("", "") = unica xs
  | otherwise = False

findValue :: Foldable t => [Char] -> t ([Char], [Char]) -> ([Char], [Char])
findValue value =
  Data.Maybe.fromMaybe ("", "")
    . foldr
      ( \(k, v) acc ->
          if value == v
            then Just (k, v)
            else acc
      )
      Nothing

-- lista (int, a)
--      (\x acc -> (fst x == tipo) || acc)