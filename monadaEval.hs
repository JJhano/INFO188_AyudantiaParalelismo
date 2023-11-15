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

-- Función para sumar dos matrices en paralelo con estrategia de evaluación
parMatAdd :: [Int] -> [Int] -> Strategy Int -> [Int]
parMatAdd [] [] _ = []
parMatAdd [] l _ = l
parMatAdd l [] _ = l
parMatAdd (x:xs) (y:ys) strat = ((x+y) `using` strat) : parMatAdd xs ys strat

-- Función para sumar dos matrices normal
parMatAdd' :: [Int] -> [Int] -> [Int]
parMatAdd' [] [] = []
parMatAdd' [] l  = l
parMatAdd' l [] = l
parMatAdd' (x:xs) (y:ys) = (x+y) : parMatAdd' xs ys 

-- Función para generar una lista de n números aleatorios en el rango [1,100]
generateRandomList :: Int -> Int -> [Int]
generateRandomList n seed = take n $ randomRs (1, 100) (mkStdGen seed)
-- Funcion para imprimir el tiempo
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.3fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)


-- Ejemplo de uso:
main :: IO ()
main = do
    -- Argumentos de ejecucion
    args <- getArgs
    if (length args) /= 1
        then error $ "run as ./monadaeval n +RTS -NX -ls -RTS"
        else return ()
    printf ("Calculando.........................\n")
    let n = read (args !! 0) :: Int
    let matrix1 = generateRandomList n 109 
    let matrix2 = generateRandomList n 18 
    -- Ejecucion normal
    t1 <- getCurrentTime
    result <- evaluate(parMatAdd' matrix1 matrix2)
    result `deepseq` printf "normal done: "
    printTimeSince t1
    -- Ejecucion en paralelo 
    t0 <- getCurrentTime
    result2 <- evaluate(parMatAdd matrix1 matrix2 rpar)
    result2 `deepseq` printf "parallel done: "
    printTimeSince t0

    -- if (length result) < 100
    --     then print result
    --     else return()
    -- print result
    return()