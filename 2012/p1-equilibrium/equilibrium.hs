import qualified  Data.ByteString.Char8 as B  (getLine, readInt, words)
import            Control.Applicative         ((<$>))
import            Data.Maybe                  (fromJust)

main = getLine              >>
       getIntList           >>=
       putIntList . eqindex

getIntList :: IO [Int]
getIntList = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

putIntList :: [Int] -> IO ()
putIntList [n]      = print n
putIntList (n : ns) =
    putStr (show n) >>
    putChar ' '     >>
    putIntList ns

eqindex :: [Int] -> [Int]
eqindex ns = go ns 0 (sum ns) 0
  where go []       _ _ _    = []
        go (n : ns) x y curr =
            let x' = x + n
                y' = y - n
            in ( if x == y' then (curr :) else id ) $ go ns x' y' (curr + 1)
