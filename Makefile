.PHONY: clean distclean

po2db: po2db.hs
	ghc --make -O2 $^

clean:
	$(RM) *.hi *.o

distclean: clean
	$(RM) po2db