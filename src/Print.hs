module Print (
    printdir
) where

import System.Directory

import Tree
import System.IO
import Text.Printf
import Data.List
import Data.Ord

printdir :: Tree -> IO ()
printdir (Folder mname msize mfiles) = do
    putStrLn $ printf "item: %-12s [%s]\n" mname $ fSizeToStr msize
    putStrLn "Relative       Size  Name"
    putStrLn "--------       ----  ----"
    mapM_ (printitem msize) $ sortOn (Data.Ord.Down . size) mfiles

printitem :: Integer -> Tree -> IO ()
printitem totalsize item = 
    putStrLn $ printf "%4.1f%%  %12s  %-35.35s" 
        ((f (size item) / f totalsize * 100) :: Float)
        (fSizeToStr $ size item) (name item)
        where f = fromIntegral



fSizeToStr :: Integer -> String
fSizeToStr v 
    | v <= 0 = "ERROR"
    | otherwise = printf "%.2f %s" (num :: Float) suffix
    where
        num = fromIntegral v / 10^(3*ordre)
        suffix = ["B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB"]!!ordre
        ordre = ordre_f v 7 -- start with zetta
        ordre_f n c
            | n >= 10^(c*3) = if c>=0 then c else c+1
            | otherwise = ordre_f n (c-1)