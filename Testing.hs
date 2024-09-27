module Testing where  -- nombre del archivo como módulo (CON MAYUSCULA)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)




{-

PROBAR EN GHCI
 
import Test.HUnit

PROBAR

:l Tests.hs

PROBAR en consola

run


*Tests> run
Cases: 3  Tried: 3  Errors: 0  Failures: 0
Counts {cases = 3, tried = 3, errors = 0, failures = 0}

-}
