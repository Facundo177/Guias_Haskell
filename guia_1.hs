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
digitoUnidades num = mod (absoluto num) 10
                                
-- j)
digitoDecenas :: Int -> Int
digitoDecenas num = div (mod (absoluto num) 100) 10



-- Ej 3

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados num1 num2 = (mod num1 num2) == 0



-- Ej 4

type DuplaReales = (Float, Float)
type TernaEnteros = (Int, Int, Int)

-- a)
prodInt :: DuplaReales -> DuplaReales -> Float
prodInt (a0, a1) (b0, b1) = a0 * b0 + a1 * b1

-- b)
todoMenorFeo :: DuplaReales -> DuplaReales -> Bool
todoMenorFeo a b = (fst a < fst b) && (snd a < snd b)

todoMenor :: DuplaReales -> DuplaReales -> Bool
todoMenor (a0, a1) (b0, b1) = (a0 < b0) && (a1 < b1) 

-- c)
distanciaPuntos :: DuplaReales -> DuplaReales -> Float
distanciaPuntos (a0, a1) (b0, b1) = sqrt ( (b1 - a1)**2 + (b0 - a0)**2 )

-- d)
sumaTerna :: TernaEnteros -> Int
sumaTerna (a, b, c) = a + b + c

-- e)
sumarSoloMultiplos :: TernaEnteros -> Int -> Int
sumarSoloMultiplos (a, b ,c) d  | aEsMultiplo && bEsMultiplo && cEsMultiplo = a + b + c
                                | aEsMultiplo && bEsMultiplo && not cEsMultiplo = a + b
                                | aEsMultiplo && not bEsMultiplo && cEsMultiplo = a + c
                                | not aEsMultiplo && bEsMultiplo && cEsMultiplo = b + c
                                | aEsMultiplo = a
                                | bEsMultiplo = b
                                | cEsMultiplo = c
                                | otherwise = 0
                                where   aEsMultiplo = (mod a d) == 0
                                        bEsMultiplo = (mod b d) == 0
                                        cEsMultiplo = (mod c d) == 0


sumarSoloMultiplosCorto :: TernaEnteros -> Int -> Int
sumarSoloMultiplosCorto (a, b ,c) d  | aEsMultiplo && bEsMultiplo && cEsMultiplo = a + b + c
                                | aEsMultiplo && bEsMultiplo = a + b
                                | aEsMultiplo && cEsMultiplo = a + c
                                | bEsMultiplo && cEsMultiplo = b + c
                                | aEsMultiplo = a
                                | bEsMultiplo = b
                                | cEsMultiplo = c
                                | otherwise = 0
                                where   aEsMultiplo = (mod a d) == 0
                                        bEsMultiplo = (mod b d) == 0
                                        cEsMultiplo = (mod c d) == 0



sumarSoloMultiplosAvanzado :: TernaEnteros -> Int -> Int
sumarSoloMultiplosAvanzado (a, b ,c) d = (siEsMultiplo a d) + (siEsMultiplo b d) + (siEsMultiplo c d)
                                        where siEsMultiplo x b  | (mod x b) == 0 = x
                                                                | otherwise = 0


-- f)
posPrimerPar :: TernaEnteros -> Int
posPrimerPar (a, b, c)  | (mod a 2) == 0 = 1
                        | (mod b 2) == 0 = 2
                        | (mod c 2) == 0 = 3
                        | otherwise = 4
                        
-- g)
crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

-- h)
invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

-- i)
-- Ya está hecho con DuplaReales



-- Ej 5

{- 
fFunc :: Int -> Int
fFunc n | n <= 7 = n*n 
        | otherwise = 2*n - 1

gFunc :: Int -> Int
gFunc n | (mod n 2) == 0 = div n 2 
        | otherwise = 3*n + 1
-}

todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (t0, t1, t2) = (f t0 > g t0) && (f t1 > g t1) && (f t2 > g t2)
                    where   f n | n <= 7 = n*n 
                                | otherwise = 2*n - 1
                            g n | (mod n 2) == 0 = div n 2 
                                | otherwise = 3*n + 1
                        


-- Ej 6
type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto anio   | not (mod anio 4 == 0) || ((mod anio 100 == 0) && not (mod anio 400 == 0)) = False
                | otherwise = True
