import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isDigit)
import Data.List (unfoldr, splitAt)

main :: IO ()
main = do
  _ <- getLine
  snowflakes <- getSnowflakes
  if foundIdentical snowflakes
     then putStrLn "Twin snowflakes found."
     else putStrLn "No two snowflakes are alike."
  
getSnowflakes :: IO [B.ByteString]
getSnowflakes = B.getLine >>= f
  where f x = if B.null x then return []
                          else getSnowflakes >>= g
                             where g xs = return (x:xs)

foundIdentical :: [B.ByteString] -> Bool
foundIdentical = findDuplicate . getMap . convert

convert :: [B.ByteString] -> [[Integer]]
convert = map (unfoldr readInteger)

readInteger :: B.ByteString -> Maybe (Integer, B.ByteString)
readInteger = B.readInteger . B.dropWhile (not . isDigit)

getMap :: [[Integer]] -> M.Map Integer [Integer]
getMap sf = createMap (sf, M.empty)

createMap :: ([[Integer]], M.Map Integer [Integer]) -> M.Map Integer [Integer]
createMap ([], acc) = acc
createMap (xs:xss, acc) = createMap (xss, M.insertWith (++) hash xs acc)
  where hash = getHash xs 1000

getHash :: [Integer] -> Integer -> Integer
getHash sf n = sum sf `rem` n

findDuplicate :: M.Map Integer [Integer] -> Bool
findDuplicate sf =
  let potDups = M.filterWithKey (\k a -> length a > 6) sf
  in M.size potDups /= 0 && checkIdent (M.elems potDups)

checkIdent :: [[Integer]] -> Bool
checkIdent [] = False
checkIdent (xs:xss) = identical dups || checkIdent xss
  where dups = split xs
  
split :: [Integer] -> [[Integer]]
split [] = []
split xs = fst pair : split (snd pair)
  where pair = splitAt 6 xs  

identical :: [[Integer]] -> Bool
identical [] = False
identical (xs:xss) = equals xs xss || identical xss

equals :: [Integer] -> [[Integer]] -> Bool
equals _ [] = False
equals xs (ys:yss) =
  identPair xs ys || equals xs yss

identPair :: [Integer] -> [Integer] -> Bool
identPair xs ys =
  (S.difference set1 set2 == S.empty && S.difference set2 set1 == S.empty) &&
    isEqual xs ys
  where set1 = S.fromList xs
        set2 = S.fromList ys

isEqual :: [Integer] -> [Integer] -> Bool 
isEqual [] _ = True
isEqual _ [] = False
isEqual s1@(x:xs) s2@(y:ys) =
  if x == y then isEqual xs ys
            else isEqual s1 (ys ++ [y])
