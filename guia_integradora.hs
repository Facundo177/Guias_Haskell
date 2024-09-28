
-- Sistema de stock

productos :: [String] -> [(String, Int)]
productos [] = []
productos listaProductos = crearInventarioProductos listaProductos [] 
                        where   crearInventarioProductos [] _ = []
                                crearInventarioProductos (x:xs) inventario  | existeEnInventario x inventario = crearInventarioProductos xs (actualizarStock x inventario)
                                                                            | otherwise = (x,1) : inventario
                                existeEnInventario _ [] = False
                                existeEnInventario producto ((nombre, cantidad):xs) | producto == nombre = True
                                                                                    | otherwise = existeEnInventario producto xs
                                actualizarStock _ [] = []                                                    
                                actualizarStock producto ((nombre, cantidad):xs)    | producto == nombre = (nombre, cantidad + 1) : xs
                                                                                    | otherwise = (nombre, cantidad) : actualizarStock producto xs


stockDeProducto :: [(String, Int)] -> String -> Int
stockDeProducto [] _ = 0
stockDeProducto ((nombre, cantidad):xs) producto    | producto == nombre = cantidad
                                                    | otherwise = stockDeProducto xs producto


dineroEnStock :: [(String, Int)] ->[(String, Float)] -> Float
dineroEnStock [] _ = 0
dineroEnStock _ [] = 0
dineroEnStock ((producto, cantidad):xs) precios = dineroPorProducto (producto, cantidad) precios + dineroEnStock xs precios
                                    where   dineroPorProducto _ [] = 0
                                            dineroPorProducto (producto, cantidad) ((nombre, precio):xs)    | producto == nombre = fromIntegral cantidad * precio
                                                                                                            | otherwise = dineroPorProducto (producto, cantidad) xs


aplicarOferta :: [(String, Int)] -> [(String, Float)] -> [(String,Float)]
aplicarOferta _ [] = []
aplicarOferta [] _ = []
aplicarOferta ((producto, cantidad):xs) precios | cantidad > 10 = actualizarPrecio producto precios
                                                | otherwise = aplicarOferta xs precios    
                                                where   actualizarPrecio _ [] = []
                                                        actualizarPrecio producto ((nombre, precio):xs) | nombre == producto = (nombre, precio*0.8):xs
                                                                                                        | otherwise = (nombre, precio) : actualizarPrecio producto xs                                                                                              





------------------------------------------------------------------------------------------------------------------------------------------




-- Perfectos Amigos

divisoresPropios :: Int -> [Int]
divisoresPropios n = divisoresDeN n (n-1)
                        where   divisoresDeN _ 0 = []
                                divisoresDeN n i        | mod n i == 0 = divisoresDeN n (i-1) ++ [i]
                                                        | otherwise = divisoresDeN n (i-1)


sonAmigos :: Int -> Int -> Bool
sonAmigos _ 0 = False
sonAmigos 0 _ = False
sonAmigos n m = sumaLista (divisoresPropios n) == m && sumaLista (divisoresPropios m) == n
                where   sumaLista [] = 0
                        sumaLista (x:xs) = x + sumaLista xs


losPrimerosNPerfectos :: Int -> [Int]
losPrimerosNPerfectos 0 = []
losPrimerosNPerfectos 1 = [6]
losPrimerosNPerfectos n = losPrimerosNPerfectos (n-1) ++ [perfectoN n]
                        where   sumaLista [] = 0
                                sumaLista (x:xs) = x + sumaLista xs
                                esPerfecto n = sumaLista (divisoresPropios n) == n
                                siguientePerfecto n     | esPerfecto (n+1) = n+1
                                                        | otherwise = siguientePerfecto (n+1)
                                perfectoN 1 = 6
                                perfectoN n = siguientePerfecto (perfectoN (n-1))
                                

listaDeAmigos :: [Int] -> [(Int,Int)]
listaDeAmigos [] = []
listaDeAmigos [x] = []
listaDeAmigos (x:xs) = amigosDeUnNumero x xs ++ listaDeAmigos xs
                        where   amigosDeUnNumero _ [] = []
                                amigosDeUnNumero n (x:xs)       | sonAmigos n x = (n, x) : amigosDeUnNumero n xs
                                                                | otherwise = amigosDeUnNumero n xs



{-

recursivo :: Int -> Int -> [Int]
recursivo 0 _ = []
recursivo n m   | sonAmigos m m = m : recursivo (n-1) (m+1)
                | otherwise = recursivo n (m+1)

losPrimerosNPerfectosMica :: Int -> [Int]
losPrimerosNPerfectosMica n = recursivo n 0

-}




------------------------------------------------------------------------------------------------------------------------------------------




-- Sopa de números

type Fila = [Int]
type Tablero = [Fila]
type Posicion = (Int, Int)
type Camino = [Posicion]


maximo :: Tablero -> Int
maximo [] = 0
maximo (fila:xs) = maximoEntre (maximoDeFila fila) (maximo xs)
                where   maximoEntre x y | x > y = x
                                        | otherwise = y
                        maximoDeFila [x] = x
                        maximoDeFila (x:xs) = maximoEntre (x) (maximoDeFila xs)



masRepetido :: Tablero -> Int
masRepetido [] = undefined
masRepetido tablero = fst (masApariciones (aparicionesDeCadaNumero (aplanarTablero tablero) []))
                        where   aplanarTablero [] = []
                                aplanarTablero (x:xs) = x ++ (aplanarTablero xs)
                                aparicionesDeCadaNumero [] listaTuplas = listaTuplas
                                aparicionesDeCadaNumero (x:xs) listaTuplas = aparicionesDeCadaNumero xs (actualizarListaApariciones x listaTuplas) 
                                actualizarListaApariciones n [] = [(n,1)]
                                actualizarListaApariciones n ((numero, apariciones):xs) | n == numero = (numero, apariciones+1):xs
                                                                                        | otherwise = (numero, apariciones) : actualizarListaApariciones n xs
                                masApariciones [(numero, apariciones)] = (numero, apariciones)
                                masApariciones ((numero_A, apariciones_A):(numero_B, apariciones_B):xs) | apariciones_A > apariciones_B = masApariciones ((numero_A, apariciones_A):xs)
                                                                                                        | otherwise = masApariciones ((numero_B, apariciones_B):xs)



valoresDeCamino :: Tablero -> Camino -> [Int]
valoresDeCamino [] _ = []
valoresDeCamino _ [] = []
valoresDeCamino tablero ((fila, columna):xs) = (encontrarColumna (encontrarFila tablero fila) columna) : (valoresDeCamino tablero xs)
                                                where   encontrarFila [] _ = undefined
                                                        encontrarFila (fila:xs) 1 = fila
                                                        encontrarFila (fila:xs) numero_fila = encontrarFila xs (numero_fila-1)
                                                        encontrarColumna [] _ = undefined
                                                        encontrarColumna (columna:xs) 1 = columna 
                                                        encontrarColumna (columna:xs) numero_columna = encontrarColumna xs (numero_columna-1)



esCaminoFibo :: [Int] -> Int -> Bool
esCaminoFibo [] _ = False
esCaminoFibo valores i = mismosElementos valores (fibonacciDesdeHasta i (largo valores))
                        where   mismosElementos [] [] = True
                                mismosElementos (x:xs) (y:ys) = x == y && mismosElementos xs ys
                                largo [] = 0
                                largo (x:xs) = 1 + largo xs

                                fibonacci :: Int -> Int
                                fibonacci 0 = 0
                                fibonacci 1 = 1
                                fibonacci i = fibonacci (i-1) + fibonacci (i-2)

                                fibonacciDesdeHasta :: Int -> Int -> [Int]
                                fibonacciDesdeHasta _ 0 = [0]
                                fibonacciDesdeHasta i j | i >= j = [fibonacci i]
                                                        | otherwise = [fibonacci i] ++ (fibonacciDesdeHasta (i+1) j)



------------------------------------------------------------------------------------------------------------------------------------------




-- ¡Vamos Campeones!


atajaronSuplentes :: [(String, String)] -> [Int] -> Int -> Int
atajaronSuplentes [] _ _ = 0
atajaronSuplentes _ [] _ = 0
atajaronSuplentes arquerosPorEquipo goles totalGolesTorneo = totalGolesTorneo - (totalGolesDeTitulares goles)
                                                        where   totalGolesDeTitulares [] = 0
                                                                totalGolesDeTitulares (x:xs) = x + totalGolesDeTitulares xs



equiposValidos :: [(String, String)] -> Bool
equiposValidos [] = True
equiposValidos (x:xs) = not (arqueroYClubIguales x) && not (hayRepeticiones x xs) && equiposValidos xs
                        where   hayRepeticiones _ [] = False
                                hayRepeticiones (equipo, arquero) ((e, a):xs)   | equipo == e || arquero == a = True
                                                                                | otherwise = hayRepeticiones (equipo, arquero) xs
                                arqueroYClubIguales (equipo, arquero) = equipo == arquero



porcentajeDeGoles :: String -> [(String, String)] -> [Int] -> Float
porcentajeDeGoles _ [] _ = 0.0
porcentajeDeGoles _ _ [] = 0.0
porcentajeDeGoles arquero arquerosPorEquipo goles = division (golesRecibidos arquero arquerosPorEquipo goles) (totalGolesDeTitulares goles) 
                                                        where   totalGolesDeTitulares [] = 0
                                                                totalGolesDeTitulares (x:xs) = x + totalGolesDeTitulares xs
                                                                golesRecibidos _ [] _ = 0
                                                                golesRecibidos _ _ [] = 0
                                                                golesRecibidos nombre ((equipo, arquero):xs) (y:ys)     | nombre == arquero = y
                                                                                                                        | otherwise = golesRecibidos nombre xs ys
                                                                division :: Int -> Int -> Float
                                                                division a b = fromIntegral a / fromIntegral b                                             



vallaMenosVencida :: [(String, String)] -> [Int] -> String
vallaMenosVencida [] _ = undefined
vallaMenosVencida _ [] = undefined
vallaMenosVencida arquerosPorEquipo goles = snd (clubYArqueroConMenosGoles arquerosPorEquipo goles)
                                        where   clubYArqueroConMenosGoles [x] _ = x
                                                clubYArqueroConMenosGoles (x1:x2:xs) (y1:y2:ys) | y1 < y2 = clubYArqueroConMenosGoles (x1:xs) (y1:ys)
                                                                                                | otherwise = clubYArqueroConMenosGoles (x2:xs) (y2:ys)
