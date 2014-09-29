TARGETS = check-integer bench-integer

GHC_DUMP_FLAGS = -ddump-to-file -ddump-prep -ddump-cmm -ddump-opt-cmm -ddump-stg -ddump-asm

GHC = ghc
GHCFLAGS = -Wall -Werror -fwarn-tabs -fPIC $(GHC_DUMP_FLAGS) -O3 $(PRAGMAS)

hsfiles = $(shell find Common/ Check/ New*/ -name \*.hs -o -name \*.lhs) *.hs $(checkfiles)

bench_hsfiles = Check/Bench1.hs Check/Bench3.hs

checkfiles = Check/New1.hs Check/New3.hs

today := $(shell date "+%Y%m%d")

PRAGMAS = -XCPP -XMagicHash -XUnboxedTuples -XUnliftedFFITypes

BROWSER ?= firefox


all : $(TARGETS)

check : check-integer
	./check-integer # | tee check.log

check-integer : check-integer.hs $(hsfiles) $(checkfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

bench-integer : bench-integer.hs $(hsfiles) $(bench_hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

Check/New1.hs : Check/NewX.template.hs
	sed "s/NewX/New1/" $+ > $@

Check/New3.hs : Check/NewX.template.hs
	sed "s/NewX/New3/" $+ > $@


Check/Bench1.hs : Check/BenchX.template.hs
	sed "s/BenchX/Bench1/;s/NewX/New1/" $+ > $@

Check/Bench3.hs : Check/BenchX.template.hs
	sed "s/BenchX/Bench3/;s/NewX/New3/" $+ > $@


bench-integer.html : bench-integer
	./bench-integer --no-gc -o bench-integer.html --template=Criterion/report.tpl
	chmod a+r bench-integer.html

date-bench : bench-integer.html
	cp $< bench-integer-$(today).html

view-bench : bench-integer.html
	$(BROWSER) bench-integer.html


Stamp/ghc-version :
	Scripts/ghc-version.sh
	touch $@


clean :
	@rm -f $(TARGETS) bench-integer.html Check/Bench[GS0-9].hs Check/New[0-9].hs
	@find . -name \*.o -o -name \*.hi -o -name \*.s -o -name \*.ll -o -name \*.hcr -o -name \*.dump-* | xargs rm -f
	@rm -rf New3/GHC/Integer.slow/

hlint :
	hlint $(shell find Common/ Check/ Hnew*/ -name \*.hs)

realclean :
	@rm -f Stamp/*
