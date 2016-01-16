TARGETS = simplest-test

GHC_DUMP_FLAGS = -dsuppress-uniques -dsuppress-all -ddump-to-file -ddump-ds \
					-ddump-simpl -ddump-simpl-iterations -ddump-simpl-stats

GHC = ghc
GHCFLAGS = -Wall -fwarn-tabs -fPIC $(GHC_DUMP_FLAGS) -O3

hsfiles = $(shell find . -name \*.hs)

test :
	make clean
	make simplest-test
	./simplest-test


simplest-test : simplest-test.hs $(hsfiles) cbits/primitive-memops.c
	$(GHC) $(GHCFLAGS) --make $< cbits/primitive-memops.c -o $@

clean :
	@find . -name \*.o | xargs rm -f
	@find . -name \*.hi | xargs rm -f
	@find . -name \*.dump* | xargs rm -f
	@rm -f simplest-test
