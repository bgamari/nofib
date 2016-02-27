{-# LANGUAGE RecordWildCards #-}
import Slurp
import System.Environment
import qualified Data.Map as M
import Control.Monad.Trans.Writer
import Data.DList (DList, singleton)
import Data.Foldable

main :: IO ()
main = do
    f:_ <- getArgs
    res <- parse_log <$> readFile f
    let samps = execWriter $ traverse buildResults res
    putStrLn $ unlines $ map (\(k,v) -> (take 40 $ k++repeat ' ')++"\t"++v) $ toList samps

sample :: Show a => String -> a -> Writer (DList (String, String)) ()
sample k v = tell $ singleton (k, show v)

buildResults :: Results -> Writer (DList (String, String)) ()
buildResults (Results{..}) = do
    forM_ (M.assocs compile_time)   $ \(k,v) -> sample ("compile-time/"++k) v
    forM_ (M.assocs compile_allocs) $ \(k,v) -> sample ("compile-allocs/"++k) v
    forM_ (M.assocs module_size)    $ \(k,v) -> sample ("module-size/"++k) v
    forM_ binary_size               $ \v     -> sample "binary-size" v
    forM_ link_time                 $ \v     -> sample "link-time" v
    sample "run-time" (geomMean run_time)
    sample "elapsed-time" (geomMean elapsed_time)
    sample "mut-time" (geomMean mut_time)
    sample "mut-elasped-time" (geomMean mut_elapsed_time)
    sample "gc-time" (geomMean gc_time)
    sample "allocs" (arithMean allocs)

arithMean :: Real a => [a] -> Float
arithMean xs = (realToFrac $ sum xs) / (realToFrac (length xs))

geomMean :: Real a => [a] -> Float
geomMean xs = (realToFrac $ product xs)**(1/realToFrac (length xs))
