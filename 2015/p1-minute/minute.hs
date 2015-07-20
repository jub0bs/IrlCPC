import Data.Char            ( digitToInt
                            , intToDigit
                            )
import Data.List            ( foldl'
                            , intercalate
                            , intersperse
                            , transpose
                            )
import Control.Monad        ( replicateM )
import Numeric              ( readInt
                            , showIntAtBase
                            )
import Data.Time.LocalTime  ( TimeOfDay(..)
                            , dayFractionToTimeOfDay
                            , timeOfDayToDayFraction
                            )
import Data.Ratio           ( (%) )

main :: IO ()
main =  do
    time  <- timeOfDayFromCols <$> getMatrixCols
    delta <- timeOfDayFromCols <$> getMatrixCols
    printCols $ colsFromTimeOfDay $ addModDay time delta

-- getMatrixCols : get the columns of a 4-by-n matrix from stdin
getMatrixCols :: IO [String]
getMatrixCols = transpose <$> replicateM 4 getBitLine

-- getBitLine : get a row of bits from stdin
getBitLine :: IO String
getBitLine = concat . words <$> getLine

-- secondsFromCols : the number of seconds corresponding to matrix 'css'
timeOfDayFromCols :: [String] -> TimeOfDay
timeOfDayFromCols css =
    TimeOfDay (h1 `times10Plus` h0)
              (m1 `times10Plus` m0)
              (fromIntegral $ s1 `times10Plus` s0)
  where
    [h1, h0, m1, m0, s1, s0] = map bin2dec css
    t `times10Plus` u        = 10 * t + u

-- addModDay t t' : the time of day obtained by adding 't' and 't''
addModDay :: TimeOfDay -> TimeOfDay -> TimeOfDay
addModDay t t' = dayFractionToTimeOfDay $ mod1 $ dayFrac + dayFrac'
  where
    dayFrac  = timeOfDayToDayFraction t
    dayFrac' = timeOfDayToDayFraction t'
    mod1     = until (< 1) (subtract 1) -- inefficient, but does the job here

-- printCols css : pretty-print the matrix 'css' (given by columns)
printCols :: [String] -> IO ()
printCols = putStrLn . intercalate "\n" . map (intersperse ' ') . transpose

-- colsFromTimeOfDay n : the matrix (by columns) corresponding to the
--                       number of seconds 'n'
colsFromTimeOfDay :: TimeOfDay -> [String]
colsFromTimeOfDay t =
      map (zeroPad 4 . dec2bin)
    $ concatMap tensAndUnits
    $ pure ($ t) <*> [todHour, todMin, round . todSec]

-- bin2dec cs : the integer correponding to bit string 'cs'
bin2dec :: String -> Int
bin2dec = fst . head . readInt 2 (`elem` "01") digitToInt

-- dec2bin n : the binary representation of integer 'n'
dec2bin :: Int -> String
dec2bin n = showIntAtBase 2 intToDigit n ""

-- tensAndUnits n : a list composed of the tens and units in integer 'n'
tensAndUnits :: Int -> [Int]
tensAndUnits n = [tens, units]
  where (tens, units) = divMod n 10

-- zeropad n cs : the string of length at least 'n' obtained by padding string
--                'cs' withh zeros on the left
zeroPad :: Int -> String -> String
zeroPad n cs = padding ++ cs
  where
    padding  = replicate padWidth '0'
    padWidth = max 0 $ n - length cs
