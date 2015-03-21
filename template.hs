-- I/O boilerplate

import qualified Data.ByteString.Char8 as B
import           Control.Applicative
import           Data.Maybe

main = undefined

getIntList :: IO [Int]
getIntList = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

putIntList :: [Int] -> IO ()
putIntList [n]    = putStrLn $ show n
putIntList (n:ns) = putStr (show n) >>
                    putChar ' '     >>
                    putIntList ns
