-- Ej 1

-- a)
f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16

-- b)
g :: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1

-- c)
h :: Integer -> Integer
h x = f (g x)
--h 8 = f (g 8)
--h 16 = f (g 16)
--h 131 = f (g 131)

k :: Integer -> Integer
k x = g (f x)
--k 1 = g (f 1)
--k 4 = g (f 4)
--k 16 = g (f 16)



-- Ej 2

-- a)
absoluto :: Int -> Int
absoluto x  | x >= 0 = x
            | otherwise = -x

-- b)
maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y  | absoluto x > absoluto y = absoluto x
                    | otherwise = absoluto y

-- c)
maximo3 :: Int -> Int -> Int -> Int
maximo3 num1 num2 num3  | (num1 >= num2) && (num1 >= num3) = num1 
                        | (num2 >= num1) && (num2 >= num3) = num2
                        | otherwise = num3

-- d)
algunoEs0 :: Float -> Float -> Bool
algunoEs0 num1 num2 = (num1 == 0) || (num2 == 0)

algunoEsCero :: Float -> Float -> Bool
algunoEsCero 0 _ = True
algunoEsCero _ 0 = True
algunoEsCero _ _ = False

-- e)
ambosSon0 :: Float -> Float -> Bool
ambosSon0 num1 num2 = (num1 == 0) && (num2 == 0)

ambosSonCero :: Float -> Float -> Bool
ambosSonCero 0 0 = True
ambosSonCero _ _ = False

-- f)
mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo num1 num2    | (num1 <= 3) && (num2 <= 3) = True
                            | (num1 > 3 && num1 <= 7) && (num2 > 3 && num2 <= 7) = True
                            | (num1 > 7) && (num2 > 7) = True
                            | otherwise = False
                
-- g)
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos num1 num2 num3    | (num1 == num2) && (num1 == num3) = num1
                                | (num1 == num2) && (num1 /= num3) = num1 + num3
                                | (num1 /= num2) && (num2 == num3) = num1 + num2
                                | (num1 == num3) && (num1 /= num2) = num1 + num2
                                | otherwise = num1 + num2 + num3

-- h)
esMultiploDe :: Int -> Int -> Bool
esMultiploDe num1 num2 = (mod num1 num2) == 0

-- i)
digitoUnidades :: Int -> Int
digitoUnidades num = mod num 10
                                
