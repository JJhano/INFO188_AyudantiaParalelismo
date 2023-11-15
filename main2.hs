-- PRUEBA 2 INFO188 - 2021
--
-- Q1) (1pt) Compile (make) y ejecute el programa, por ejemplo asi: 
--      ./prog 64 4000
--      - ¿Que es lo que genera? Puede buscar bibliografia (citarla si es asi)
--      - ¿Que rol juegan los parametros? 
--      - ¿Que impacto tienen n y k en el rendimiento?
--      - Proponga dos inputs (Los llamaremos CONF1 y CONF2):
--          - Una config de n,k  donde la grilla es extensa, pero no hay tanto trabajo por celda.
--          - Una config de n,k donde la grilla es relativamente pequena, pero hay mucho trabajo por celda.
-- 
--
-- Q2) (2.5pts) Paralelize el programa usando `parListChunk`, (Haga una copia de este archivo, con nombre Q2.hs)
--      - Busque en la literatura como se usa y explique que es lo que hace
--      - Encuentre el parametro de parListChunk que le da mayor acceleracion paralela.
--      - Reporte cual fue el tiempo de ejecucion al usar 1, 2, 4, 8 threads. (una tabla para cada CONF)
--      - Comente sus resultados en relacion a CONF1 y CONF2, fueron los que esperaba? si? no? argumente por que.
--
--
-- Q3)(2.5pts) Proponga una forma de paralelizar el programa usando la Monada Par. (Haga una copia con nombre Q3.hs)
--      - Explique claramente su plan de paralelizacion.
--      - Reporte cual fue el tiempo de ejecucion al usar 1, 2, 4, 8 threads. (una tabla para cada CONF)
--      - Compare el rendimiento obtenido con su paralelizacion anterior en la pregunta Q2.
--      - Existe alguna otra config de inputs donde esta modalidad de paralelizacion pueda lograr mejor aceleracion Q2?
--         
import System.Environment
import System.IO
import Control.Exception
-- import Control.DeepSeq
-- import Control.Parallel
-- import Control.Parallel.Strategies
import Control.Monad.Par
import Data.Time.Clock
import Data.Complex
import Data.List
import Text.Printf

-- calculo
calc :: Complex Float -> Complex Float -> Int -> Char
calc z c 0 = if (magnitude z) <= 2.0
                then '*'
                else ' '
calc z c k = let znext = z*z + c
             in calc znext c (k-1)

-- proceso
proceso :: Int  -> Int -> Par [Char]
proceso n k = let r1 = -1.5
                  dr = 3.0/(fromIntegral n) :: Float
                  im1 = 1.0
                  dim = 2.0/(fromIntegral n) :: Float
              in
                barridoPar n k n n r1 dr im1 dim r1 im1

barridoPar :: Int -> Int -> Int -> Int -> Float -> Float -> Float -> Float -> Float -> Float -> Par [Char]
barridoPar n k 0 0  r1 dr i1 di x y = return []
barridoPar n k 0 ny r1 dr i1 di x y = do
  let z = 0.0 :+ 0.0
      c = x :+ y
      xnext = r1
      ynext = y - di
  rest <- spawn $ barridoPar n k n (ny-1) r1 dr i1 di xnext ynext
  restVal <- get rest
  return ((calc z c k) : restVal)
barridoPar n k nx ny r1 dr i1 di x y = do
  let z = 0.0 :+ 0.0
      c = x :+ y
      xnext = x + dr
  rest <- spawn $ barridoPar n k (nx-1) ny r1 dr i1 di xnext y
  restVal <- get rest
  return ((calc z c k) : restVal)

barrido :: Int -> Int -> Int -> Int -> Float -> Float -> Float -> Float -> Float -> Float -> [Char]
barrido n k 0 0  r1 dr i1 di x y = []
barrido n k 0 ny r1 dr i1 di x y = let  
                                        z = (0.0 :+ 0.0)
                                        c = (x :+ y) :: (Complex Float)
                                        xnext = r1
                                        ynext = y-di
                                  in (calc z c k) : barrido n k n (ny-1) r1 dr i1 di xnext ynext
barrido n k nx ny r1 dr i1 di x y = let z = (0.0 :+ 0.0)
                                        c = (x :+ y)
                                        xnext = x+dr
                                    in (calc z c k) : barrido n k (nx-1) (ny) r1 dr i1 di xnext y

printSpace :: (Show a) => [a] -> Int -> String
printSpace [] _ = ""
printSpace cs n = (intersperse ' ' (show $ take (n+1) cs)) ++ "\n" ++ printSpace (drop (n+1) cs) n


printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.3fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)


main:: IO ()
main = do
    -- I) ARGS
    args <- getArgs
    if (length args) /= 3
        then error $ "run as ./prog n k j\nn = diminio de n x n\nk = iteraciones\nj= chunck size"
        else return ()
    let n = read (args !! 0) :: Int
    let k = read (args !! 1) :: Int
    let j = read (args !! 2) :: Int
    printf "Setup n=%i   k=%i\n" n k


    -- II) CALCULO 
    printf ("Calculando.........................\n")
    hFlush stdout
    t0 <- getCurrentTime
    r <- evaluate (runPar $ proceso n k)
    -- r `deepseq` printf "done paralelo: "
    printTimeSince t0

    -- III) resultado (imprimir solo si n es relativamente pequeno, para evitar flood de print)
    if n <= 256
        then printf $ printSpace r n
        else return ()
    return ()


