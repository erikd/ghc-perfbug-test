
import qualified Integer.Internals as X
import qualified Integer.Natural as X

main :: IO ()
main = putStrLn . X.hexShowNatural $
    X.timesNatural (X.mkNatural [1, 2, 4]) (X.mkNatural [1, 2])
