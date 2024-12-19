## File generated by the BNF Converter (bnfc 2.9.5).

# Makefile for building the parser and test program.

GHC        = ghc
GHC_OPTS   = -package array -package containers -package mtl
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : interpreter

# Rules for building the parser.

AbsIcedAmericano.hs LexIcedAmericano.x ParIcedAmericano.y PrintIcedAmericano.hs IcedAmericano.hs TypeChecker.hs Interpreter.hs ErrorMessage.hs: IcedAmericano.cf
	bnfc --haskell --functor IcedAmericano.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

# TestIcedAmericano : AbsIcedAmericano.hs LexIcedAmericano.hs ParIcedAmericano.hs PrintIcedAmericano.hs TypeChecker.hs Interpreter.hs ErrorMessage.hs TestIcedAmericano.hs
# 	${GHC} ${GHC_OPTS} $@

interpreter : AbsIcedAmericano.hs LexIcedAmericano.hs ParIcedAmericano.hs PrintIcedAmericano.hs TypeChecker.hs Interpreter.hs ErrorMessage.hs IcedAmericano.hs
	${GHC} ${GHC_OPTS} -o interpreter AbsIcedAmericano.hs LexIcedAmericano.hs ParIcedAmericano.hs PrintIcedAmericano.hs TypeChecker.hs Interpreter.hs ErrorMessage.hs IcedAmericano.hs

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi interpreter

# EOF