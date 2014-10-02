TARGETS = simplest-test

GHC_DUMP_FLAGS = -dsuppress-uniques -dsuppress-all -ddump-to-file -ddump-ds \
					-ddump-simpl -ddump-simpl-iterations -ddump-simpl-stats

GHC = ghc
GHCFLAGS = -Wall -Werror -fwarn-tabs -fPIC $(GHC_DUMP_FLAGS) -O3

hsfiles = $(shell find   -name \*.hs -o -name \*.lhs) *.hs


demo :
	make clean
	make simplest-test
	cp -r Integer Integer.slow
	touch Integer/Natural.hs
	make simplest-test
	@echo
	@echo "    Fast version etas : `grep -c eta Integer/Natural.dump-simpl`"
	@echo "    Slow version etas : `grep -c eta Integer.slow/Natural.dump-simpl`"
	@echo


simplest-test : simplest-test.hs $(hsfiles) $(bench_hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

clean :
	@rm -f $(TARGETS) bench-integer.html Check/Bench[GS0-9].hs Check/New[0-9].hs
	@find . -name \*.o -o -name \*.hi -o -name \*.s -o -name \*.ll -o -name \*.hcr -o -name \*.dump-* | xargs rm -f
	@rm -rf Integer.slow/
