-- GUIA 4 (Recursion) 

-- Ej 1
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Ej 2
parteEntera :: Float -> Integer
parteEntera n   | n < 1 = 0
                | otherwise = 1 + parteEntera (n-1)

-- Ej 3
esDivisible :: Integer -> Integer -> Bool
esDivisible num1 num2   | num1 == 0 = True
                        | num1 < num2 = False
                        | otherwise = esDivisible (num1 - num2) num2

-- Ej 4
sumaImpares :: Integer -> Integer
sumaImpares n   | n == 1 = 1
                | otherwise = (n * 2 - 1) + sumaImpares (n - 1)

-- Ej 5
medioFact :: Integer -> Integer
medioFact n | n == 0 = 1
            | n == 1 = 1
            | otherwise = medioFact (n-2) * n

-- Ej 6
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n   | n < 10 = True
                        | otherwise = compararUltimosDosDigitos n && todosDigitosIguales (div n 10)
                        where compararUltimosDosDigitos n = mod n 10 == div (mod n 100) 10

-- Ej 7
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n (10 ^ (cantDigitos n - i))) 10
                    where cantDigitos n | n < 10 = 1 
                                        | otherwise = cantDigitos (div n 10) + 1 


iesimoDigitoRecursivo :: Integer -> Integer -> Integer
iesimoDigitoRecursivo n i   | i == 1 = div n (10^(cantDigitos n - 1))
                            | otherwise = iesimoDigitoRecursivo (mod n (10^(cantDigitos n - 1))) (i-1)
                            where cantDigitos n | n < 10 = 1 
                                                | otherwise = cantDigitos (div n 10) + 1 

                                        
-- Ej 8
sumaDigitos :: Integer -> Integer
sumaDigitos n   | n < 10 = n
                | otherwise = (mod n 10) + sumaDigitos (div n 10)

-- Ej 9
esCapicua :: Integer -> Bool
esCapicua n | n < 10 = True
            | otherwise = ultimoDigito n == primerDigito n && esCapicua (sacarPrimerYUltimoDigito n)
            where   cantDigitos n   | n < 10 = 1 
                                    | otherwise = cantDigitos (div n 10) + 1
                    ultimoDigito n = mod n 10
                    primerDigito n = div n (10 ^ (cantDigitos n - 1))
                    sacarPrimerYUltimoDigito n = div (mod n (10 ^ (cantDigitos n - 1))) 10
                
-- Ej 10
f1 :: Int -> Int
f1 0 = 1
f1 n = 2^n + f1 (n-1) 

f2 :: Int -> Float -> Float
f2 1 q = q
f2 n q = q^n + f2 (n-1) q 

f3 :: Int -> Float -> Float
f3 1 q = q
f3 n q = q^m + f2 (m-1) q 
    where m = 2*n

f4 :: Int -> Float -> Float
f4 n q = (f3 n q) - (f2 n q) + q^n


-- Ej 11
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- a)
eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = 1 / fromIntegral (factorial n) + eAprox (n-1)

-- b)
e :: Float
e = eAprox 10


-- Ej 12
raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = (sucesionAn n) - 1
                where sucesionAn n  | n == 1 = 2
                                    | otherwise = 2 + (1 / (sucesionAn (n-1)))


-- Ej 13
f :: Integer -> Integer -> Integer 
f n m   | n == 0 = 1
        | otherwise = f (n-1) m + auxiliar n m
        where auxiliar n m  | m == 1 = n
                            | otherwise = n^m + auxiliar n (m-1)


-- Ej 14
sumaPotencias :: Integer -> Integer -> Integer -> Integer 
sumaPotencias q n m | n == 1 = auxiliarB q 1 m   -- Es mejor hacer n == 0 = 0  -> así se hace esta llamada recursiva sin especificarla
                    | otherwise = sumaPotencias q (n-1) m + auxiliarB q n m
                    where auxiliarB q n m   | m == 1 = q^(n+1)
                                            | otherwise = q^(n+m) + auxiliarB q n (m-1)

-- Ej 15
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n m  | n == 1 = auxiliar 1 (fromInteger m)
                    | otherwise = sumaRacionales (n - 1) m + auxiliar (fromInteger n) (fromInteger m)
                    where auxiliar n m  | m == 1 = n
                                        | otherwise = (n / m) + auxiliar n (m-1)


-- Ej 16

-- a)
menorDivisor :: Integer -> Integer
menorDivisor n = divisionRecursiva n 2
            where divisionRecursiva n m | m >= n = n
                                        | mod n m == 0 = m   
                                        | otherwise = divisionRecursiva n (m+1) 

-- b)
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

-- c)
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m | n == 1 = True
                | compararDivisores n m = False
                | otherwise = sonCoprimos (div n (menorDivisor n)) m
                where   compararDivisores n m   | m == 1 = False
                                                | menorDivisor n == menorDivisor m = True
                                                | otherwise = compararDivisores n (div m (menorDivisor m))

-- d)
nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n   | n == 1 = 2
                | otherwise = siguientePrimo (nEsimoPrimo (n-1))
                where siguientePrimo i  | esPrimo (i+1) = i + 1
                                        | otherwise = siguientePrimo (i+1)


-- Ej 17
esFibonacci :: Integer -> Bool
esFibonacci 0 = True
esFibonacci 1 = True
esFibonacci n = compararFibonacci n 1
            where   compararFibonacci n m   | n == fibonacci m = True
                                            | n < fibonacci m = False 
                                            | otherwise = compararFibonacci n (m+1)



-- Ej 18
mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n = siEsPar (unDigito n)
                where   siEsPar n   | mod n 2 == 0 = n
                                    | otherwise = -1
                        quedarmeConElMayor n m  | n > m = n
                                                | otherwise = m
                        unDigito n  | n < 10 = n 
                                    | otherwise = quedarmeConElMayor (siEsPar (mod n 10)) (unDigito (div n 10))
                            


-- Ej 19
sumaPrimerosmPrimos :: Integer -> Integer
sumaPrimerosmPrimos n   | n == 1 = 2
                        | otherwise = soloSiEsPrimo n + sumaPrimerosmPrimos (n-1)
                        where soloSiEsPrimo n   | esPrimo n = n
                                                | otherwise = 0

esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n | n == 1 = False
                        | n == 2 = True
                        | otherwise = auxiliar n 1
                        where   auxiliar n i    | sumaPrimerosmPrimos i > n = False
                                                | sumaPrimerosmPrimos i == n = True
                                                | otherwise = auxiliar n (i+1)
                                


-- Ej 20
sumaDivisores :: Integer -> Integer
sumaDivisores n | n == 1 = 1
                | otherwise = sumatoriaCondicionada n n
                where   soloSiEsDivisor n m | mod n m == 0 = m
                                            | otherwise = 0
                        sumatoriaCondicionada n_base m  | m == 1 = 1 
                                                        | otherwise = soloSiEsDivisor n_base m + sumatoriaCondicionada n_base (m-1)

tomaValorMax :: Integer -> Integer -> Integer
tomaValorMax n m    | n > m = undefined
                    | n == m = n
                    | otherwise = quedarmeConElMayor m (tomaValorMax n (m-1))
                    where   quedarmeConElMayor n m  | sumaDivisores n > sumaDivisores m = n
                                                    | otherwise = m
                            
                            
-- Ej 21
--pitagoras :: Integer -> Integer -> Integer -> Integer     




