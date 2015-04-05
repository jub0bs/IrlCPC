import           Control.Applicative ((<$>))
import           Data.List           (foldl')
import qualified Data.Set as S

main = do
    getLine
    ns <- map read . words <$> getLine :: IO [Int]
    if isSumFree ns
    then print 1
    else print 0

-- setFromList ns : the set obtained by inserting all integers in list 'ns'
--                  in the empty set
setFromList :: [Int] -> S.Set Int
setFromList = foldl' (flip S.insert) S.empty

-- isSumFree ns : is the list of integers 'ns' sum-free?
isSumFree :: [Int] -> Bool
isSumFree ns = all (\z -> not $ S.member z set) allSums
  where
    set     = setFromList ns
    allSums = [x + y | x <- ns, y <- ns, x /= y]

-- Notes:
--   * overall complexity: O(n^2 log n)
--   * example 1 indicates that the two integers must be distinct
--   * whether negative integers are allowed is unclear
