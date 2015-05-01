import Control.Applicative ((<$>))
import Data.List           (sort)

main = do
    n  <- read <$> getLine :: IO Int
    ps <- getPoints n
    let ps' = convexHull ps
    print $ length ps'
    printPoints ps'

getPoint :: IO Point
getPoint = do
    [x, y] <- map read . words <$> getLine
    return $ Point x y

getPoints :: Int -> IO [Point]
getPoints 0 = return []
getPoints n = do
    p  <- getPoint
    ps <- getPoints (n - 1)
    return (p : ps)

printPoints :: [Point] -> IO ()
printPoints []            = return ()
printPoints (Point x y : ps) = do
    putStrLn $ show x ++ " " ++ show y
    printPoints ps

------------------------------
-- Monotone-chain algorithm --
------------------------------

data Point  = Point !Int !Int
    deriving (Eq, Ord)
data Vector = Vector !Int !Int

-- vec p1 p2 : the vector between the two points 'p1' and 'p2'
vec :: Point -> Point -> Vector
vec (Point x1 y1) (Point x2 y2) = Vector (x2 - x1) (y2 - y1)

-- cross v1 v2 : the cross-product of vectors 'v1' and 'v2'
cross :: Vector -> Vector -> Int
cross (Vector x1 y1) (Vector x2 y2) = x1 * y2 - y1 * x2

-- isClockwise o a b : is angle (OA, OB) a clockwise turn?
isClockwise :: Point -> Point -> Point -> Bool
isClockwise o a b = vec o a `cross` vec o b <= 0

-- convexHull ps : the list of points forming the convex hull
--                 of the list of points 'ps'
convexHull :: [Point] -> [Point]
convexHull []     = []
convexHull [p]    = [p]
convexHull points = lower ++ upper
  where
    sorted = sort points
    lower  = chain sorted
    upper  = chain (reverse sorted)

-- chain ps : the longest counterclockwise chain obtained by walking along
--            the list of points 'ps'
chain :: [Point] -> [Point]
chain = go []
  where
    go acc@(r1 : r2 : rs) (x : xs) =
        if isClockwise r2 r1 x
        then go (r2 : rs) (x : xs)
        else go (x : acc) xs
    go acc                (x : xs) = go (x : acc) xs
    go acc                []       = reverse $ tail acc
