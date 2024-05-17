interpreter: Main.hs TypeChecker.hs Interpreter.hs Bnfc/Abs.hs Bnfc/ErrM.hs Bnfc/Lex.hs Bnfc/Par.hs Bnfc/Print.hs Bnfc/Skel.hs Bnfc/Test.hs
	mkdir -p build
	ghc -Wall --make -outputdir build Main.hs -o $@

clean:
	rm -rf build interpreter

.PHONY: clean