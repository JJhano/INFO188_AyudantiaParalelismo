-- PRUEBA 2020
-- Ejercicios:
--
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
parMatAdd :: Strategy a -> Strategy [a] ->Strategy [a]
parMatAdd sa    



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