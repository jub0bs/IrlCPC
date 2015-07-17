import Data.Char     ( digitToInt, intToDigit )
import Data.List     ( foldl', intercalate, intersperse, transpose )
import Control.Monad ( replicateM )
import Numeric       ( readInt, showIntAtBase )

main :: IO ()
main =  do
    time  <- secondsFromColumns <$> getMatrixByCol
    delta <- secondsFromColumns <$> getMatrixByCol
    printCols $ matrixFromSeconds $ time + delta

-- getMatrixCol : get the columns of a 4-by-n matrix from stdin
getMatrixByCol :: IO [String]
getMatrixByCol = transpose <$> replicateM 4 getBitLine

-- printCols css : pretty-print the matrix 'css' (given by columns)
printCols :: [String] -> IO ()
printCols = putStrLn . intercalate "\n" . map (intersperse ' ') . transpose

-- getBitLine : get a row of bits from stdin
getBitLine :: IO String
getBitLine = concat . words <$> getLine

secPerHour, minPerHour, secPerMin, hoursPerDay :: Int
secPerHour  = minPerHour * secPerMin
minPerHour  = 60
secPerMin   = 60
hoursPerDay = 24

-- secondsFromColumns : the number of seconds corresponding to matrix 'css'
secondsFromColumns :: [String] -> Int
secondsFromColumns css = sum $ zipWith (*) weights digits
  where
    digits  = map bin2dec $ css
    weights = [ 10 * secPerHour, secPerHour
              , 10 * secPerMin , secPerMin
              , 10             , 1
              ]

-- matrixFromSeconds n : the matrix (by columns) corresponding to the number
--                       of seconds 'n'
matrixFromSeconds :: Int -> [String]
matrixFromSeconds n =
      map (zeropad 4 . dec2bin)
    $ concatMap tensAndUnits
          [hours, minutes, seconds]
  where
    n'               = n `mod` (hoursPerDay * secPerHour)
    (mins, seconds)  = divMod n' secPerMin
    (hours, minutes) = divMod mins minPerHour

-- tensAndUnits : the list composed of the tens and units in integer 'n'
tensAndUnits :: Int -> [Int]
tensAndUnits n = [t,u]
  where
    (t, u) = divMod n 10

-- bin2dec cs : the integer correponding to bit string 'cs'
bin2dec :: String -> Int
bin2dec = fst . head . readInt 2 (`elem` "01") digitToInt

-- dec2bin n : the binary representation of integer 'n'
dec2bin :: Int -> String
dec2bin n = showIntAtBase 2 intToDigit n ""

-- zeropad n cs : the string of length at least 'n' obtained by padding string
--                'cs' withh zeros on the left
zeropad :: Int -> String -> String
zeropad n cs = padding ++ cs
  where
    padding  = replicate padWidth '0'
    padWidth = max 0 $ n - length cs
