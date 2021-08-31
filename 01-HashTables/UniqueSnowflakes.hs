import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char (isDigit)
import Data.List (unfoldr)
import Data.Maybe (isNothing, fromMaybe)

main :: IO ()
main = do
  _ <- getLine
  sf <- getSnowflakes
  let snowflakes = convert sf
  if foundIdentical snowflakes
     then putStrLn "Twin snowflakes found."
     else putStrLn "No two snowflakes are alike."
  
getSnowflakes :: IO [B.ByteString]
getSnowflakes = B.getLine >>= f
  where f x = if B.null x then return []
                          else getSnowflakes >>= g
                             where g xs = return (x:xs)

foundIdentical :: [[Integer]] -> Bool
foundIdentical sf = searchMap sf M.empty

convert :: [B.ByteString] -> [[Integer]]
convert = map (unfoldr readInteger)

readInteger :: B.ByteString -> Maybe (Integer, B.ByteString)
readInteger = B.readInteger . B.dropWhile (not . isDigit)

searchMap :: [[Integer]] ->  M.Map Integer [Integer] -> Bool
searchMap [] _ = False
searchMap (xs:xss) acc
  | isNothing result = searchMap xss (M.insert hash xs acc)
  | contains values xs = True
  | otherwise = searchMap xss (M.insertWith (++) hash xs acc)
  where hash = sum xs `mod` 100000
        result = M.lookup hash acc
        values = fromMaybe [] result
  
contains :: [Integer] -> [Integer] -> Bool
contains values = checkIdent (split values)

split :: [Integer] -> [[Integer]]
split [] = []
split xs = fst pair : split (snd pair)
  where pair = splitAt 6 xs

checkIdent :: [[Integer]] -> [Integer] -> Bool
checkIdent [] _ = False
checkIdent (xs:xss) sf = identPair xs sf || checkIdent xss sf

identPair :: [Integer] -> [Integer] -> Bool
identPair xs ys = S.fromList xs == S.fromList ys && areEqual xs ys

areEqual :: [Integer] -> [Integer] -> Bool 
areEqual xs ys
  | x /= y =
    let shiftL = dropWhile (/= x) ys ++ takeWhile (/= x) ys
        shiftR = reverse (tail (dropWhile (/= x) ys) ++ takeWhile (/= x) ys ++ [x])
    in xs == shiftL || xs == shiftR
  | otherwise = xs == ys
  where x = head xs
        y = head ys
