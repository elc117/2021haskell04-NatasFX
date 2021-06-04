-- Prática 04 de Haskell
-- Nome: Natã Schmitt

import Text.Printf

faixaIdoso :: Int -> String
faixaIdoso age = if age >= 60 then "IDO" ++ show (([64,69..80] ++ [80])!!if div (age-60) 5 > 4 then 4 else div (age-60) 5) else "ND"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos l = [ (name, age,f) | f <- [faixaIdoso (snd x) | x <- l], name <- [fst x | x <- l], age <- [snd x | x <- l]]

classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' = map (\x-> (fst x, snd x, faixaIdoso (snd x)))

strColor :: (Int,Int,Int) -> String
strColor t = printf "rgb%s" (show t)

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n t d = [(cx,snd t,d) | cx <- [fst t,fst t+2*d..fst t + 2*n*d]]

genReds :: Int -> [(Int,Int,Int)]
genReds n = [(mod x 256, 0, 0) | x <- [n,n+1..n+n]]

