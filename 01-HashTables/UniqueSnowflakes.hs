import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.Char (isDigit)
import Data.List (unfoldr, elemIndex)
import Data.Maybe (isNothing, fromMaybe)

main :: IO ()
main = do
  [n] <- unfoldr readInt <$> B.getLine
  let readInput = [x | x <- [B.getLine], _ <- [1..n]]
  sf <- sequence readInput 
  let snowflakes = convert sf
  if foundIdentical snowflakes
     then putStrLn "Twin snowflakes found."
     else putStrLn "No two snowflakes are alike."
  
foundIdentical :: [[Int]] -> Bool
foundIdentical sf = searchMap sf M.empty

convert :: [B.ByteString] -> [[Int]]
convert = map (unfoldr readInt)

readInt :: B.ByteString -> Maybe (Int, B.ByteString)
readInt = B.readInt . B.dropWhile (not . isDigit)

searchMap :: [[Int]] ->  M.Map Int [Int] -> Bool
searchMap [] _ = False
searchMap (xs:xss) acc
  | isNothing result = searchMap xss (M.insert hash xs acc)
  | contains values xs = True
  | otherwise = searchMap xss (M.insertWith (++) hash xs acc)
  where hash = sum xs `mod` 100000
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
