import Data.Char

pegar :: Int -> [Int] -> [Int]
pegar 0 _ = []
pegar n (x:xs) = x: pegar (n-1) xs

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

esimo :: Int -> [Int] -> Int
esimo 0 _ = 0
esimo n (x) = head (drop n (x))

converte :: String -> String
converte [] = []
converte (x:xs) = toUpper x: converte xs

elimina :: [Int] -> [Int]
elimina [] = []
elimina (x:xs) = if (x > 10) then x: elimina xs else elimina xs

tirarM :: String -> String
tirarM [] = []
tirarM (x:xs) = if(x > 'Z') then x: tirarM xs else tirarM xs

nums :: Int -> Int -> [Int]
nums n m  = if (n < m) then [n+1] ++ nums (n+1) m else []

par :: [Int] -> [Int]
par [] = []
par (x:xs) = if mod x 2 == 0 then x: par xs else par xs

filtro :: Int -> Int -> Int
filtro x y = prodelem(par(nums x y))

soma :: Float -> Float -> Float -> Float
soma x y z = (x + y + z) / 3

soma1 :: Float -> Float -> Float
soma1 x z = x + z

soma2 :: [Float] -> Float
soma2 [] = 0
soma2 (x:xs) = x + soma2 xs

mediaentre :: Float -> Float -> Float -> Float
mediaentre x y z
  | x > soma x y z && y > soma x y z = 2
  | x > soma x y z && z > soma x y z = 2
  | y > soma x y z && z > soma x y z = 2
  | z > soma x y z = 1
  | y > soma x y z = 1
  | x > soma x y x = 1
  | otherwise      = 0

raiz1 :: Float -> Float-> Float -> Float
raiz1 a b c = -b+(sqrt(b^2-4*a*c))

raiz2 :: Float -> Float -> Float -> Float
raiz2 a b c = -b-(sqrt(b^2-4*a*c))

maior :: Float -> Float -> Float -> Float
maior a b c 
  |((b^2-4*a*c))==0 = (-b)/(2*a)
  |((b^2-4*a*c))>0 && ((raiz1 a b c) > (raiz2 a b c)) = (raiz1 a b c)
  |((b^2-4*a*c))>0 && ((raiz1 a b c) < (raiz2 a b c)) = (raiz2 a b c)
  |otherwise = 1

fatorial :: Int -> Int
fatorial 0 = 1
fatorial 1 = 1
fatorial x = x * (fatorial (x-1))

fatorialN :: Int -> Int
fatorialN 0 = 1
fatorialN n = fatorial n + fatorialN (n-1)

elevado :: Int -> Int
elevado x = 2^x

elev :: [Int] -> [Int]
elev [] = []
elev (x:xs) = x^2: elev xs

pot :: Int -> Int -> Int
pot m n = m^n

nesimo :: Int -> Int
nesimo 0 = 1
nesimo n = if n >= 0 then 2^n + nesimo (n-1) else n

prodelem :: [Int] -> Int
prodelem [] = 1
prodelem (x:xs) = x * prodelem xs

filtrar :: [Int] -> [Int]
filtrar [] = []
filtrar (x:xs) = if mod x 2 == 0 then x: filtrar xs else filtrar xs

eliminap :: Int -> [Int] -> [Int]
eliminap 1 (x:xs) = [x]
eliminap n [] = []
eliminap n (x:xs) = if x /= n then x: eliminap (n) xs else xs

eliminaLL :: Int -> [Int] -> [Int]
eliminaLL 1 (x:xs) = [x]
eliminaLL n [] = []
eliminaLL n (x:xs) = if x /= n then x: eliminaLL (n) xs else eliminaLL (n)xs

inverte :: String -> String
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

divisors :: Int -> [Int]
divisors x = [y | y <-[1..x], x `mod` y == 0]

--divparte1 :: Int->Int->[Int]->[Int]
--divparte1 x a xs = 
  -- (a > x) = []
  -- (a <= x) = xs: divparte1 x (a + 1) xs

--divisao :: Int-> [Int]
--divisao x = divparte1 x 2 [1]

ehprimo :: Int -> Bool
ehprimo x = if(tamanho (divisors x) > 2) then False else True