SRC=main.hs
SRC2=monadaPar.hs
SRC3=monadaEval.hs
PROG=prog
MONADAEVAL=monadaeval
MONADAPAR=monadapar
all:
	ghc -dynamic -threaded -eventlog -rtsopts ${SRC} -o prog
	ghc -dynamic -threaded -eventlog -rtsopts ${SRC2} -o monadapar
	ghc -dynamic -threaded -eventlog -rtsopts -package random ${SRC3} -o monadaeval
	#   -dynamic  : para algunas distribuciones (ej: Arch Linux)
	#   -threaded : permitir el uso de threads
	#   -eventlog : registrar los eventos en un log para futuro uso (threadscope)
	#   -rtsopts  : habilitar opciones RTS en tiempo de ejecucion
	#
	#   Al ejecutar:
	#   ./prog args +RTS -NX -ls -RTS
	#   NX -> N1, N2, .... numero de CPU threads en un pool.

clean:
	rm -f $(PROG) $(MONADAPAR) $(MONADAEVAL) *.o *.hi