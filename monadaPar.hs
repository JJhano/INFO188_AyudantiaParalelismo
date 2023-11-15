

-- 1) Calcular de forma paralela la funcion de fibonacci usandio monada par

-- Utilizar el main dado para medir el tiempo de las funciones creadas

import Data.Time.Clock
import Text.Printf
-- import Text.Read
import System.Environment
import System.IO
import Control.DeepSeq
import Control.Monad.Par
import Data.Time.Clock


printTimeSince t0 = do
	t1 <- getCurrentTime
	printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

-- Funcion fibonnacci normal
fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x = (fibonacci $ x - 1) + (fibonacci $ x - 2)

--Funcion fibonacci paralela
fib :: Int -> Int
fib 0  = 1 
fib 1  = 1
fib x = runPar $ do 
	i <- new
	j <- new 
    -- Ejecucion en paralelo
	fork (put i(fibonacci $ x - 1)) 
	fork (put j(fibonacci $ x - 2))
	a <- get i
	b <- get j 
	return (a + b)


main :: IO ()
main = do
	args <- getArgs
	if (length args) /= 1
		then error $ "Run as ./monadapar n +RTS -NX -ls -RTS <example in makefile>"
		else return () 
	let n = read (args !! 0) :: Int
	t0 <- getCurrentTime
	let comp = fib n
	comp `deepseq` (printTimeSince t0) -- forzar computo de la funcion
	print comp
	
