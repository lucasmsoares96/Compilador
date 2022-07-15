module Semantico (semantico) where

import qualified Data.Maybe
import Tipos

semantico :: [Token] -> Bool
semantico tokens =
  unica listaVariaveis
    && and (atribuicao novosTokens)
  where
    listaVariaveis = declaracao tokens
    novosTokens = variavelPtipo tokens listaVariaveis

-- adapta a lista de tokens para associar os tipos às variáveis
variavelPtipo :: [Token] -> [Variavel] -> [Token]
variavelPtipo [] _ = []
variavelPtipo (token : tokens) variaveis
  -- quando forma uma variavel e não uma funcao
  | fst token == "variavel"
      && snd (head tokens) /= "(" =
    a : variavelPtipo tokens variaveis
  | otherwise = token : variavelPtipo tokens variaveis
  where
    a = findValue (snd token) variaveis

-- localiza as linhas que  possuem atribuição
atribuicao :: [Token] -> [Bool]
atribuicao (a : b : z)
  | snd b == "=" =
    mesmoTipo :
    atribuicao (drop (length sentenca + 1) (a : b : z))
  | otherwise = atribuicao (b : z)
  where
    tipo = fst a
    sentenca = takeWhile (\x -> snd x /= "," && snd x /= ";") (a : b : z)
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
declaracao :: [Token] -> [Variavel]
declaracao [] = []
declaracao [_] = []
declaracao [_, _] = []
declaracao (a : b : c : z)
  | variavel = varTipoNome tipo linha ++ declaracao resto
  | otherwise = declaracao (b : c : z)
  where
    -- não é uma função
    variavel = fst a == "tipo" && snd c /= "("
    linha = takeWhile (\x -> snd x /= ";") (b : c : z)
    resto = drop (length linha + 1) (b : c : z)
    tipo = snd a

varTipoNome :: Tipo -> [Token] -> [Variavel]
varTipoNome _ [] = []
varTipoNome tipo (x : xs)
  | fst x == "variavel" = (tipo, nome) : varTipoNome tipo xs
  | otherwise = varTipoNome tipo xs
  where
    nome = snd x

unica :: [Variavel] -> Bool
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