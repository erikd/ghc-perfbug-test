# ghc-perfbug-test

During work on my [haskell-big-integer-experiment][bigint] project I found a
nasty performance bug. This project is a drastically stripped down version of
the [haskell-big-integer-experiment][bigint] codebase to demonstrate the bug.


# The problem

During development of [haskell-big-integer-experiment][bigint] I found one
function that would run slower if the whole project was built from scratch, but
run quickly of the file it was in was `touch`-ed and only it and dependent files
were rebuilt.

In my testing, this pathology has be 100% reliable and completely unfathomable.
Why does touching a file and rebuilding the binary cause such a significant
speed improvement.


# Requirements.

The requirements are:

* ghc >= 7.8 (uses GHC.Prim features not in 7.6).
* Hspec and Criterion libraries.
* Make


# Steps to reproduce

1. Clone the repo and change directory into it.
2. Run `make clean bench-integer.html` which will compile and run the benchmarking
program resulting in a file named `bench-integer.html`. If you view this HTML file
in a web browser, you will see that tests New3-[ABC] are significantly slower than
New1-[ABC].
3. Run `touch New3/GHC/Integer/Natural.hs && make bench-integer.html` which just
builds the touched file and all its dependencies and runs the benchmark resulting
in a version of the file `bench-integer.html`. In this new file, the New1-ABC]
tests are about the same speed as they were in step 2., but the New3[ABC] tests
are now significantly fasted that the New1-[ABC] tests.

Steps 2. and 3. can be run over and over again with basically the same results.



[bigint]: https://github.com/erikd/haskell-big-integer-experiment
