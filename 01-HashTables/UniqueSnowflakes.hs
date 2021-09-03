import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Control.Monad as C
import Data.Char (isDigit)
import Data.List (unfoldr, elemIndex)
import Data.Maybe (isNothing, fromMaybe)

main :: IO ()
main = do
  [n] <- unfoldr readInt <$> B.getLine
  snowflakes <- C.forM [1..n] (const (unfoldr readInt <$> B.getLine))
  if foundIdentical snowflakes M.empty
    then putStrLn "Twin snowflakes found."
    else putStrLn "No two snowflakes are alike."

readInt :: B.ByteString -> Maybe (Int, B.ByteString)
readInt = B.readInt . B.dropWhile (not . isDigit)

foundIdentical :: [[Int]] -> M.Map Int [Int] -> Bool
foundIdentical [] _ = False
foundIdentical (xs:xss) sfMap =
  found || foundIdentical xss updatedMap
  where (found, updatedMap) = searchMap xs sfMap

searchMap :: [Int] ->  M.Map Int [Int] -> (Bool, M.Map Int [Int])
searchMap sf acc
  | isNothing result = (False, M.insert hash sf acc)
  | contains values sf = (True, acc)
  | otherwise = (False, M.insertWith (++) hash sf acc)
  where hash = sum sf `mod` 100000
        result = M.lookup hash acc
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
areEqual _ [] = False
areEqual s1@(x:xs) s2@(y:ys)
  | x `notElem` s2 = False
  | otherwise =
    let i = fromMaybe 0 (elemIndex x s2)
        shiftR = [(n, (n+i) `mod` 6) | n <- [0..5]]
        shiftL' = [(n, i-n) | n <- [0..5]]
        shiftL = map (\ (i,j) -> if j < 0 then (i, j+6) else (i,j)) shiftL'
    in checkPair s1 s2 shiftR || checkPair s1 s2 shiftL

checkPair :: [Int] -> [Int] -> [(Int, Int)] -> Bool
checkPair s1 s2 = all ((== True) . (\ (i, j) -> s1 !! i == s2 !! j)) 
