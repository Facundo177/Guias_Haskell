-- GUIA 5

-- Ej 1
--1
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)

--2
ultimo :: [t] -> t
ultimo [] = undefined
ultimo [x] = x
ultimo (x:xs) = ultimo xs

--3
principio :: [t] -> [t]
principio [] = undefined
principio [x] = []
principio (x:xs) = x:principio (xs)

--4
reverso :: [t] -> [t] 
reverso [] = []
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]



-- Ej 2
--1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece a [x] = a == x
pertenece a (x:xs) = a == x || pertenece a xs

--2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) = x == y && todosIguales (y:xs)

--3
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs)   | comparoConTodos x xs = False
                        | otherwise = todosDistintos xs
                        where   comparoConTodos n [x] = n == x
                                comparoConTodos n (x:xs)    | n == x = True
                                                            | otherwise = comparoConTodos n xs

--4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x:xs) | comparoConTodos x xs = True
                    | otherwise = hayRepetidos xs
                    where   comparoConTodos n [x] = n == x
                            comparoConTodos n (x:xs)    | n == x = True
                                                        | otherwise = comparoConTodos n xs
------
-- o
------
hayRepetidosSimple :: (Eq t) => [t] -> Bool
hayRepetidosSimple a = not (todosDistintos a)


--5
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar n [x]    | n == x = []
                | otherwise = [x]
quitar n (x:xs) | n == x = xs
                | otherwise = x:quitar n xs
                
--6 
quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos n [x]   | n == x = []
                    | otherwise = [x]
quitarTodos n (x:xs)    | n == x = quitarTodos n xs
                        | otherwise = x:quitarTodos n xs

--7
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs)    | comparoConTodos x xs = eliminarRepetidos xs
                            | otherwise = x:eliminarRepetidos xs
                            where   comparoConTodos n [x] = n == x
                                    comparoConTodos n (x:xs)    | n == x = True
                                                                | otherwise = comparoConTodos n xs

--8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos _ [] = False
mismosElementos [] _ = False
mismosElementos l1 l2 = compararElementos l1 l2 && compararElementos l2 l1
                    where   compararElementos [x] y = comparoConTodos x y
                            compararElementos (x:xs) y = comparoConTodos x y && compararElementos xs y
                            comparoConTodos n [x] = n == x
                            comparoConTodos n (x:xs)    | n == x = True
                                                        | otherwise = comparoConTodos n xs

--9
capicua :: (Eq t) => [t] -> Bool
capicua [] = True
capicua [x] = True
capicua l = compararUnoAUno l (reverso l)
        where   compararUnoAUno [x] [y] = x == y
                compararUnoAUno (x:xs) (y:ys) = x == y && compararUnoAUno xs ys



-- Ej 3
--1
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria [x] = x
sumatoria (x:xs) = x + sumatoria xs

--2
productoria :: [Integer] -> Integer
productoria [] = 1
productoria [x] = x
productoria (x:xs) = x * productoria xs

--3
maximo :: [Integer] -> Integer
maximo [] = undefined
maximo [x] = x
maximo (x:y:xs) | x > y = maximo (x:xs) 
                | otherwise = maximo (y:xs)

--4
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n [x] = [x+n]
sumarN n (x:xs) = [x+n] ++ sumarN n xs

--5
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [] = undefined
sumarElPrimero (x:xs) = sumarN x (x:xs)

--6
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [] = []
sumarElUltimo [x] = [x+x]
sumarElUltimo l = sumarN (primerElemento (reverso l)) l
                where   primerElemento [] = 0
                        primerElemento (x:xs) = x

--7
siEsMultiploALista :: Integer -> Integer -> [Integer]
siEsMultiploALista x n  | mod x n == 0 = [x]
                        | otherwise = []

pares :: [Integer] -> [Integer]
pares [] = []
pares [x] = siEsMultiploALista x 2
pares (x:xs) = siEsMultiploALista x 2 ++ pares xs  

--8
multiplosDeN :: Integer -> [Integer] -> [Integer] 
multiplosDeN _ [] = []
multiplosDeN n [x] = siEsMultiploALista x n
multiplosDeN n (x:xs) = siEsMultiploALista x n ++ multiplosDeN n xs

--9
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar l = ordenar (quitar (maximo l) l) ++ [maximo l]



-- Ej 4
type Texto = [Char]

-- a
sacarBlancosRepetidos :: Texto -> Texto
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs)  | x == ' ' && x == y = sacarBlancosRepetidos (y:xs) 
                                | otherwise = x : sacarBlancosRepetidos (y:xs)

                              
-- b
contarEspacios :: Texto -> Integer
contarEspacios [] = 0
contarEspacios (x:xs)   | x == ' ' = 1 + contarEspacios xs
                        | otherwise = contarEspacios xs

sacarEspacioAlPrincipio :: Texto -> Texto
sacarEspacioAlPrincipio [] = []
sacarEspacioAlPrincipio (x:xs)  | x == ' ' = xs
                                | otherwise = (x:xs)

sacarEspacioAlFinal :: Texto -> Texto
sacarEspacioAlFinal [] = []
sacarEspacioAlFinal [x] | x == ' ' = []
                        | otherwise = [x]
sacarEspacioAlFinal (x:xs) = x : sacarEspacioAlFinal xs


contarPalabras :: Texto -> Integer
contarPalabras [] = 0
contarPalabras texto = 1 + contarEspacios (sacarEspacioAlFinal (sacarEspacioAlPrincipio (sacarBlancosRepetidos texto)))


-- c
palabras :: Texto -> [Texto]                        
palabras [] = []
palabras texto = armarPalabra textoLimpio : palabras (eliminarPalabra textoLimpio)
                where   textoLimpio = sacarEspacioAlFinal (sacarEspacioAlPrincipio (sacarBlancosRepetidos texto))
                        armarPalabra [] = []
                        armarPalabra (x:xs)     | x == ' ' = []
                                                | otherwise = x : armarPalabra xs
                        eliminarPalabra [] = []
                        eliminarPalabra (x:xs)  | x == ' ' = xs
                                                | otherwise = eliminarPalabra xs
                        

-- d
palabraMasLarga :: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga texto = masLarga (palabras texto)
                        where   largoDeUnaPalabra [] = 0
                                largoDeUnaPalabra (x:xs) = 1 + largoDeUnaPalabra xs  
                                masLarga [] = []
                                masLarga (x:xs) = laMasLargaDeLasDos x (masLarga xs)
                                laMasLargaDeLasDos a b  | largoDeUnaPalabra a > largoDeUnaPalabra b = a
                                                        | otherwise = b

-- e
aplanar :: [[Char]] -> [Char]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

-- f
aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos [] = []
aplanarConBlancos (x:xs)        | xs == [] = x
                                | otherwise = x ++ [' '] ++ aplanarConBlancos xs

-- g
aplanarConNBlancos :: [[Char]] -> Integer -> [Char]
aplanarConNBlancos [] _ = []
aplanarConNBlancos (x:xs) n     | xs == [] = x
                                | otherwise = x ++ nBlancos n ++ aplanarConNBlancos xs n
                                where   nBlancos 0 = [] 
                                        nBlancos n = ' ' : nBlancos (n-1)



-- Ej 5

--1
sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada [x] = [x]
sumaAcumulada (x:y:xs) = x : sumaAcumulada (x+y:xs)

--2
descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x:xs) = divisoresPrimosDeUnNumero x : descomponerEnPrimos xs
                        where   divisionRecursiva :: Integer -> Integer -> Integer
                                divisionRecursiva n m   | m >= n = n
                                                        | mod n m == 0 = m   
                                                        | otherwise = divisionRecursiva n (m+1)
                                menorDivisor :: Integer -> Integer
                                menorDivisor n = divisionRecursiva n 2
                                divisoresPrimosDeUnNumero :: Integer -> [Integer]
                                divisoresPrimosDeUnNumero 1 = []
                                divisoresPrimosDeUnNumero n = (menorDivisor n) : divisoresPrimosDeUnNumero (div n (menorDivisor n))
                                         


-- Ej 6
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

-- a
elNombre :: Contacto -> Nombre
elNombre (nombre, telefono) = nombre

elTelefono :: Contacto -> Telefono
elTelefono (nombre, telefono) = telefono

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos _ [] = False
enLosContactos nombre [x] = nombre == elNombre x
enLosContactos nombre (x:xs) = enLosContactos nombre [x] || enLosContactos nombre xs

-- b
agregarContacto :: Contacto -> ContactosTel -> ContactosTel
agregarContacto contacto [] = [contacto]
agregarContacto (nombre, telefono) (x:xs)       | nombre == elNombre x = (nombre, telefono) : xs
                                                | otherwise = x : agregarContacto (nombre, telefono) xs

-- c
eliminarContacto :: Nombre -> ContactosTel -> ContactosTel
eliminarContacto nombre [] = []
eliminarContacto nombre (x:xs)  | nombre == elNombre x = xs
                                | otherwise = x : eliminarContacto nombre xs



-- Ej 7
type Identificacion = Integer
type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]
type Disponibilidad = Bool

--1
existeElLocker :: Identificacion -> MapaDeLockers -> Bool
existeElLocker _ [] = False
existeElLocker locker ((identificacion, estado):xs)     | locker == identificacion = True
                                                        | otherwise = existeElLocker locker xs

--2
ubicacionDelLocker :: Identificacion -> MapaDeLockers -> Ubicacion
ubicacionDelLocker _ [] = undefined
ubicacionDelLocker locker ((identificacion, (disponibilidad, ubicacion)):xs)    | locker == identificacion = ubicacion
                                                                                | otherwise = ubicacionDelLocker locker xs

--3
estaDisponibleElLocker :: Identificacion -> MapaDeLockers -> Bool
estaDisponibleElLocker _ [] = undefined
estaDisponibleElLocker locker ((identificacion, (disponibilidad, ubicacion)):xs)        | locker == identificacion = disponibilidad
                                                                                        | otherwise = estaDisponibleElLocker locker xs

--4
ocuparLocker :: Identificacion -> MapaDeLockers -> MapaDeLockers
ocuparLocker _ [] = undefined
ocuparLocker locker ((identificacion, (disponibilidad, ubicacion)):xs)  | locker == identificacion = (identificacion, (False, ubicacion)) : xs
                                                                        | otherwise = (identificacion, (disponibilidad, ubicacion)) : (ocuparLocker locker xs)

