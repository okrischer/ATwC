{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO
import Data.List (permutations)
import qualified Data.ByteString.Builder as B

main :: IO ()
main = do
  testFile <- openFile "inputUS.txt" WriteMode
  let testCases = take 50 $ [n:xs | x:xs <- permutations [1,2,3,4,5,6], n <- [1..6], n /= x]
  let n = length testCases
  hPrint testFile n
  B.hPutBuilder testFile $ writeTestCases testCases
  hClose testFile

writeTestCases :: [[Int]] -> B.Builder
writeTestCases = foldr ((<>) . buildInts) mempty 

buildInts :: [Int] -> B.Builder
buildInts [] = mempty
buildInts [x] = B.intDec x <> B.char7 '\n'
buildInts (x:xs) = B.intDec x <> B.char7 ' ' <> buildInts xs