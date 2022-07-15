module Tipos (Grupo, Lexema, Token, Tipo, Nome, Variavel) where

type Grupo = String

type Lexema = String

type Token = (Grupo, Lexema)

type Tipo = String

type Nome = String

type Variavel = (Tipo, Nome)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)