import            Control.Applicative         ((<$>))
import            Control.Monad               (replicateM)
import            Data.Char                   (digitToInt, intToDigit)
import qualified  Data.ByteString.Char8 as B  (ByteString, foldl', getLine)

main :: IO ()
main = do
    n  <- getInt
    ns <- replicateM n getInt
    putStrLn $ int2str $ sum ns

getInt :: IO Int
getInt = bs2int <$> B.getLine

-- bs2int bs : the integer corresponding to the binary bytestring 'bs'
bs2int :: B.ByteString -> Int
bs2int = B.foldl' (\acc c -> 2 * acc + digitToInt c) 0

-- int2str n : the 32-bit string corresponding to integer 'n'
int2str :: Int -> String
int2str n = reverse $ take 32 $ go n ++ repeat '0'
  where
    go n = let (q, r) = divMod n 2 in
               intToDigit r : if q == 0 then [] else go q
