import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M
import Data.Char (isDigit)
import Data.List (unfoldr, elemIndices)
import Data.Maybe (isNothing, fromMaybe)

main :: IO ()
main = do
  n:snowflakes <- unfoldr readInt <$> B.getContents
  if foundIdentical snowflakes M.empty
    then putStrLn "Twin snowflakes found."
    else putStrLn "No two snowflakes are alike."

readInt :: B.ByteString -> Maybe (Int, B.ByteString)
readInt = B.readInt . B.dropWhile (not . isDigit)

foundIdentical :: [Int] -> M.IntMap [Int] -> Bool
foundIdentical [] _ = False
foundIdentical sfs sfMap =
  found || foundIdentical (drop 6 sfs) updatedMap
  where (found, updatedMap) = searchMap (take 6 sfs) sfMap

searchMap :: [Int] -> M.IntMap [Int] -> (Bool, M.IntMap [Int])
searchMap sf sfMap
  | isNothing result = (False, M.insert hash sf sfMap)
  | checkIdent values sf = (True, sfMap)
  | otherwise = (False, M.insertWith (++) hash sf sfMap)
  where hash = sum sf `rem` 100000
        result = M.lookup hash sfMap
        values = fromMaybe [] result
  
checkIdent :: [Int] -> [Int] -> Bool
checkIdent [] _ = False
checkIdent values sf = areEqual (take 6 values) sf || checkIdent (drop 6 values) sf

areEqual :: [Int] -> [Int] -> Bool
areEqual [] _ = False
areEqual s1@(x:xs) s2
  | x `notElem` s2 = False
  | otherwise = checkPair s1 s2 (elemIndices x s2)

checkPair :: [Int] -> [Int] -> [Int] -> Bool
checkPair _ _ [] = False
checkPair s1 s2 (j:js) = 
  all (\(i, j) -> s1 !! i == s2 !! j) shiftR ||
  all (\(i, j) -> s1 !! i == s2 !! j) shiftL ||
  checkPair s1 s2 js
  where
    shiftR = [(i, (i+j)   `mod` 6) | i <- [0..5]]
    shiftL = [(i, (j-i+6) `mod` 6) | i <- [5,4..0]]
