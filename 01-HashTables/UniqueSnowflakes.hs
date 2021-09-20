import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.Char (isDigit)
import Data.List (unfoldr, elemIndices)
import Data.Maybe (isNothing, fromMaybe)

main :: IO ()
main = do
  input <- B.getContents
  let n:snowflakes = B.lines input
  if foundIdentical snowflakes M.empty
    then putStrLn "Twin snowflakes found."
    else putStrLn "No two snowflakes are alike."

readInt :: B.ByteString -> Maybe (Int, B.ByteString)
readInt = B.readInt . B.dropWhile (not . isDigit)

foundIdentical :: [B.ByteString] -> M.Map Int [Int] -> Bool
foundIdentical [] _ = False
foundIdentical (xs:xss) sfMap =
  found || foundIdentical xss updatedMap
  where (found, updatedMap) = searchMap (unfoldr readInt xs) sfMap

searchMap :: [Int] ->  M.Map Int [Int] -> (Bool, M.Map Int [Int])
searchMap sf sfMap
  | isNothing result = (False, M.insert hash sf sfMap)
  | contains values sf = (True, sfMap)
  | otherwise = (False, M.insertWith (++) hash sf sfMap)
  where hash = sum sf `rem` 100000
        result = M.lookup hash sfMap
        values = fromMaybe [] result
  
contains :: [Int] -> [Int] -> Bool
contains values = checkIdent (split values) 

split :: [Int] -> [[Int]]
split [] = []
split xs = fst pair : split (snd pair)
  where pair = splitAt 6 xs

checkIdent :: [[Int]] -> [Int] -> Bool
checkIdent [] _ = False
checkIdent (xs:xss) sf = areEqual xs sf || checkIdent xss sf

areEqual :: [Int] -> [Int] -> Bool
areEqual [] _ = False
areEqual s1@(x:xs) s2
  | x `notElem` s2 = False
  | otherwise = checkPair s1 s2 (elemIndices x s2)

checkPair :: [Int] -> [Int] -> [Int] -> Bool
checkPair _ _ [] = False
checkPair s1 s2 (x:xs) = 
  all (\ (i, j) -> s1 !! i == s2 !! j) shiftR ||
  all (\ (i, j) -> s1 !! i == s2 !! j) shiftL ||
  checkPair s1 s2 xs
  where
    shiftR = [(n, (n+x) `mod` 6) | n <- [0..5]]
    shiftL' = [(n, x-n) | n <- [0..5]]
    shiftL = map (\ (i,j) -> if j < 0 then (i, j+6) else (i,j)) shiftL'