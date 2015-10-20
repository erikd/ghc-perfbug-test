TARGETS = simplest-test

GHC_DUMP_FLAGS = -dsuppress-uniques -dsuppress-all -ddump-to-file -ddump-ds \
					-ddump-simpl -ddump-simpl-iterations -ddump-simpl-stats

GHC = ghc
GHCFLAGS = -Wall -Werror -fwarn-tabs -fPIC $(GHC_DUMP_FLAGS) -O3

hsfiles = $(shell find   -name \*.hs -o -name \*.lhs) *.hs

test :
	make clean
	make simplest-test
	./simplest-test


simplest-test : simplest-test.hs $(hsfiles) $(bench_hsfiles)
	cabal exec -- $(GHC) $(GHCFLAGS) --make $< -o $@

clean :
	@rm -f simplest-test *.o *.hi *.dump*
