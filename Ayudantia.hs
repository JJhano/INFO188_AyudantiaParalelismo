-- PRUEBA 2020
-- Ejercicios:
--
import Control.Parallel.Strategies
import System.Random
import Data.Time.Clock
import System.IO
import System.Environment
import System.IO
import Control.Exception
import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies
import Data.Time.Clock
import Data.Complex
import Data.List
import Text.Printf

-- I) Escriba la funcion 'parMatAdd' que pueda sumar dos matrices en paralelo, con un
-- argumento extra que permita escoger la estrategia de evaluacion, asi poder 
-- escoger WHNF o NF. Las matrices siempre seran cuadradas y se pasan como listas row-major. 
-- Ej: matrix 3x3 de numeros consecutivos --> [1,2,3,4,5,6,7,8,9]
--     Ejemplo: 
--          parMatAdd [1,2,3,4,5,6,7,8,9] [1,1,1,2,2,2,3,3,3] `using` STRATEGIA
--          --> [2,3,4,6,7,8,10,11,12]
--
--
--

-- Función para sumar dos elementos de la matriz
addElements :: Int -> Int -> Int
addElements x y = x + y

-- Función para sumar dos matrices en paralelo con estrategia de evaluación
parMatAdd :: [Int] -> [Int] -> Strategy Int -> [Int]
parMatAdd [] [] _ = []
parMatAdd [] l _ = l
parMatAdd l [] _ = l
parMatAdd (x:xs) (y:ys) strat = ((x+y) `using` strat) : parMatAdd xs ys strat

parMatAdd' :: [Int] -> [Int] -> [Int]
parMatAdd' [] [] = []
parMatAdd' [] l  = l
parMatAdd' l [] = l
parMatAdd' (x:xs) (y:ys) = (x+y) : parMatAdd' xs ys 

-- Función para generar una lista de n números aleatorios en el rango [1,3]
generateRandomList :: Int -> Int -> [Int]
generateRandomList n seed = take n $ randomRs (1, 100) (mkStdGen seed)

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.3fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)


-- Ejemplo de uso:
main :: IO ()
main = do
    let matrix1 = generateRandomList 10000000 109 :: [Int]
    let matrix2 = generateRandomList 10000000 18 :: [Int]
    printf ("Calculando.........................")
    hFlush stdout
    t0 <- getCurrentTime
    result <- evaluate( parMatAdd matrix1 matrix2 rseq)
    result `deepseq` printf "parallel done: "
    printTimeSince t0

    t1 <- getCurrentTime
    result <- evaluate( parMatAdd' matrix1 matrix2)
    result `deepseq` printf "normal done: "
    printTimeSince t1
    -- if (length result) < 100
    --     then print result
    --     else return()
    -- print result
    return()


-- II) Escriba la funcion 'simCA' que simule en paralelo 
-- la evolucion de un automata celular de 1 dimension.
--
-- INPUT:  una lista donde cada elemento es una celula en estado 0 o 1.
-- OUTPUT: otra lista con el nuevo estado del automata celular.
-- BORDES: puede asumir que existen valores '0' rodeando los bordes.
--
-- REGLA de evolucion: el proximo estado de cada celda se calcula 
-- en base a su estado actual y el de su vecino izq y der. Los posibles 
-- casos son:
--      tn   -> 111  110  101  100  011  010  001  000
--      tn+1 ->  0    1    0    1    1    0    1    0
--
--      Ejemplo:
--          simCA [0, 1, 0, 1, 1, 0, 1] 
--          -->   [1, 0, 0, 1, 1, 0, 0]
--
--          a) implemente la funcion en paralelo, decida usted si usara 
--              estrategias o accelerate.
--          b) generalize su funcion y reciba un segundo argumento, que es 
--             un entero indicando la regla de simulacion. La regla 
--             descrita anteriormente correspondia a la regla 90 (01011010)