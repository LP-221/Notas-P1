module Parens where

import Test.HUnit

-- Nuestro tipo de datos principal, el lenguaje de los paréntesis.
data Lexer = Epsilon | M2 (Tokens) Lexer (Tokens) | M3 Lexer Lexer

-- Representamos los tokens que son '(' y ')' 
data Tokens = LP | RP deriving (Eq)

-- Representamos un stack de tokens
type Stack = [Tokens]


-- Instancia de Show para los tokens (paréntesis)
instance Show Tokens where
  show LP = "("
  show RP = ")"

-- Instancia de Show para las cadenas que podemos crear.
instance Show Lexer where
  show Epsilon    = ""
  show (M2 a b c) = show a ++ show b ++ show c
  show (M3 a b)   = show a ++ show b

-- Función que calcula el número de paréntesis izquierdos "a propósito".
nLP :: Lexer -> Int
nLP Epsilon = 0
nLP  x      = error "D:"

accept :: Lexer -> Bool
accept word = aux (show word) []

accept1 word = aux (getTokens word) []

aux :: String -> Stack -> Bool
aux "" [] = True
aux ('(':xs) stack = aux xs ([LP] ++ stack)
aux (')':xs) stack = if length stack > 0
                     then aux xs (pop stack)
                     else False
aux "" stack = False

                       -- ((
                       -- p = []
                       -- p = [(]
                       -- p = [(()]

                       -- p = []
                       -- s = (s'

pop :: Stack -> Stack
pop []     = []
pop (x:xs) = xs

string1a :: Lexer
string1a = M2 LP (M2 LP (M2 LP (Epsilon) RP) RP) RP
-- ((()))

{-
s1 = ((()))
p = []

s1 = (()))
p = [(]

s1 = ()))
p = [((]
-}

string1u :: Lexer
string1u = M2 LP (M2 LP (M2 RP (Epsilon) RP) RP) RP
-- (())))

string2a :: Lexer
string2a = M3 (M2 LP Epsilon RP) (M2 LP Epsilon RP)
-- ()()
