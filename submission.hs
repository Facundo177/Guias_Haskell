module SolucionT2 where

{-
ENUNCIADO:
Resolver los siguientes ejercicios cuyas especificaciones en lenguaje semiformal figuran a continuación. Deben ser implementadas en Haskell utilizando los tipos requeridos y solamente las funciones que se ven en la materia Introducción a la Programación / Algoritmos y Estructuras de Datos I (FCEyN-UBA).

Ejercicio 1 (2 puntos)
problema mediaMovilN (lista: seq⟨Z⟩, n: Z) : Float {
  requiere: {|lista| > 0}
  requiere: {n > 0 ∧ n ≤ |lista|}
  asegura: {res es el promedio de los últimos n elementos de lista}
}

Ejercicio 2 (2 puntos)
problema esAtractivo (n: Z) : Bool {
  requiere: {n > 0}
  asegura: {res = true <=> la cantidad de factores primos de n (distintos o no) es también un número primo.}
}
Aclaración: los factores primos de 30 son [5,3,2]. Los factores primos de 9 son [3,3].

Ejercicio 3 (2 puntos)
problema palabraOrdenada (palabra: seq⟨Char⟩) : Bool {
  requiere: {True}
  asegura: {res = true <=> cada uno de los elementos no blancos de palabra es mayor o igual al anterior caracter no blanco, si existe alguno.}
}
Aclaración: 'a' < 'b' es True.

Ejercicio 4 (3 puntos)
problema similAnagrama (palabra1: seq⟨Char⟩, palabra2: seq⟨Char⟩) : Bool⟩{
  requiere: {True}
  asegura: {res = true <=> (para todo caracter no blanco, la cantidad de apariciones de ese caracter en palabra1 es igual a la cantidad de apariciones en palabra2, y además existe al menos un caracter en palabra1 que tiene una posición distinta en palabra2)}
}

Ejercicio 5 (1 punto)
Conteste marcando la opción correcta. ¿Cuándo se dice que una especificación está sub-especificada?:
● Cuando se da una precondición más restrictiva de lo realmente necesario, o bien una postcondición más débil de la que se necesita.
  Cuando se da una precondición más débil de lo realmente necesario, o bien una postcondición más restrictiva de la que se necesita.
  Cuando no hay precondiciones (o la precondición es True).
-}



-- Ejercicio 1

-- Auxiliares
largoLista :: [t] -> Integer
largoLista [] = 0
largoLista (x:xs) = 1 + largoLista xs

sumaTodo :: Num a => [a] -> a
sumaTodo [] = 0
sumaTodo (x:xs) = x + sumaTodo xs

promedio :: (Fractional a1, Integral a2) => [a2] -> a1
promedio [] = 0
promedio lista = fromIntegral (sumaTodo lista) / fromIntegral (largoLista lista)


-- Función principal
mediaMovilN :: [Integer] -> Integer -> Float
mediaMovilN lista n = promedio (primerosNElementos (reverso lista) n)
                        where   reverso [] = []
                                reverso (x:xs) = reverso xs ++ [x]
                                primerosNElementos [] _ = []
                                primerosNElementos _ 0 = []
                                primerosNElementos (x:xs) n = x : primerosNElementos xs (n-1)
                                


-- Ejercicio 2

-- Auxiliares
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo 2 = True
esPrimo n = menorDivisor n == n

menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = recursionDeDivisores n 2 
                where   recursionDeDivisores n i    | i >= n = n
                                                    | mod n i == 0 = i
                                                    | otherwise = recursionDeDivisores n (i+1)
          
listaDivisoresPrimos :: Integer -> [Integer]
listaDivisoresPrimos 1 = []
listaDivisoresPrimos n = (menorDivisor n) : (listaDivisoresPrimos (div n (menorDivisor n)))


-- Función principal
esAtractivo :: Integer -> Bool
esAtractivo n = esPrimo (largoLista (listaDivisoresPrimos n))



-- Ejercicio 3

-- Auxiliares
eliminarBlancos :: [Char] -> [Char]
eliminarBlancos [] = []
eliminarBlancos (x:xs)  | x == ' ' = eliminarBlancos xs
                        | otherwise = x : eliminarBlancos xs


-- Función principal
palabraOrdenada :: String -> Bool
palabraOrdenada [] = True 
palabraOrdenada palabra = compararLetras (eliminarBlancos (palabra))
                        where   compararLetras [] = True
                                compararLetras [x] = True
                                compararLetras (x:y:xs) = x <= y && compararLetras (y:xs)



-- Ejercicio 4

-- Auxiliares
sonIguales :: String -> String -> Bool
sonIguales [] [] = True
sonIguales [] _ = False
sonIguales _ [] = False
sonIguales [x] [y] = x == y
sonIguales (x:xs) (y:ys) = (x == y) && sonIguales xs ys

aparicionesDeCadaCaracter :: String -> [(Char, Integer)]
aparicionesDeCadaCaracter [] = []
aparicionesDeCadaCaracter palabra = crearListaTuplas (eliminarBlancos palabra) []
                                    where   crearListaTuplas [] listaTuplas = listaTuplas
                                            crearListaTuplas (x:xs) [] = crearListaTuplas xs (actualizarListaTuplas x [])
                                            crearListaTuplas (x:xs) listaTuplas = crearListaTuplas xs (actualizarListaTuplas x listaTuplas)
                                            actualizarListaTuplas caracter [] = [(caracter, 1)]
                                            actualizarListaTuplas caracter ((char, apariciones):xs) | caracter == char = (char, apariciones+1) : xs 
                                                                                                    | otherwise = (char, apariciones) : actualizarListaTuplas caracter xs

encontrarApariciones :: [(Char, Integer)] -> Char -> Integer
encontrarApariciones [] _ = 0
encontrarApariciones ((char, apariciones):xs) caracter  | char == caracter = apariciones
                                                        | otherwise = encontrarApariciones xs caracter                                   

compararApariciones :: [(Char, Integer)] -> [(Char, Integer)] -> Bool
compararApariciones [] [] = True
compararApariciones [] _ = True
compararApariciones _ [] = False
compararApariciones ((caracter, apariciones):xs) segundaLista = (apariciones == (encontrarApariciones segundaLista caracter)) && compararApariciones xs segundaLista


-- Función principal
similAnagrama :: String -> String -> Bool
similAnagrama x y   | sonIguales (eliminarBlancos x) (eliminarBlancos y) = False
                    | largoLista (aparicionesDeCadaCaracter x) /= largoLista (aparicionesDeCadaCaracter y) = False
                    | otherwise = compararApariciones (aparicionesDeCadaCaracter x) (aparicionesDeCadaCaracter y)

