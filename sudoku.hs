import System.Environment
import System.IO;

data Cell = Fixed Int | Possible [Int] | ToReplace [Int] deriving (Show,Eq)
type Row = [Cell]
type Grid = [Row]

            -- READ AND SHOW INPUT --


showRowWithPossibilities :: Row -> String
showRowWithPossibilities = foldr (\y x -> (showCell y) ++ x ) "\n"
    where 
        showCell (Fixed x) = show x ++ "          "
        showCell (Possible xs) = '[':(foldr (\y x -> (if y `elem` xs then show y else " " ) ++ x) "]" [1..9])

showRow :: Row -> String
showRow  = foldr (\y x -> (showCell y) ++ x ) "\n" 
    where
        showCell (Fixed x) = show x ++ " "
        showCell _ = " "

showGrid :: Maybe Grid -> String 
showGrid Nothing = "Invalid"
showGrid (Just z)=  foldr (\y x -> showRow y ++ x) "" z

showGridWP :: Grid -> String 
showGridWP = foldr (\y x -> showRowWithPossibilities y ++ x) ""

rInt :: String -> Int
rInt x = read x

readRow :: String -> Row
readRow = map (\x -> if x == '.' then Possible [1..9] else Fixed (rInt [x]))

readGrid :: String -> Grid
readGrid [] =[]
readGrid input = readRow(take 9 input):readGrid (drop 9 input)


--------map column to rows and map boxes to rows----

rotate :: [[a]] -> [[a]]
rotate [] = []
rotate ([]:xss) = rotate xss
rotate ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : rotate (xs : [ t | (_:t) <- xss])

mapFunc :: Grid -> Grid
mapFunc [x,y,z] = (take 3 x ++ take 3 y ++ take 3 z):
                    (take 3 (drop 3 x) ++ take 3 (drop 3 y) ++ take 3 (drop 3 z)):
                    [drop 6 x ++ drop 6 y ++ drop 6 z]

getSquares :: Grid -> Grid
getSquares [] = []
getSquares x = (mapFunc (take 3 x)) ++ getSquares(drop 3 x)
----------------------------------------------
pruneCells :: [Cell] -> [Cell]
pruneCells cells = map pruneCell cells 
  where
    fixeds = [x | Fixed x <- cells]
    s (Possible xs) = foldl (\x y -> filter (/= y) x) xs fixeds
    pruneCell (Possible xs) | length pos == 1 = Fixed (head pos) 
                            | otherwise = Possible pos
                            where pos = s (Possible xs)
    pruneCell x = x



-- prune impossible possibilities

pruneGrid' :: Grid -> Grid
pruneGrid' grid = getSquares (map pruneCells (getSquares g2))
    where g1 = map pruneCells grid
          g2 = rotate (map pruneCells (rotate g1))
           

pruneGrid :: Grid -> Grid
pruneGrid grid = fixM pruneGrid' grid
  where
    fixM f x = let p = f x in
        if p == x then x else fixM f p

--find cell with least ammount of possible digits

positions :: Grid -> [(Integer,[(Integer,Cell)])]
positions grid = zip [1..] (map (zip [1..]) grid)


compareCells :: (Cell,(Integer,Integer)) -> (Cell,(Integer,Integer)) -> (Cell,(Integer,Integer))
compareCells x (Fixed s,(_,_)) =  x
compareCells (Fixed s,(_,_)) x = x
compareCells (Possible x,a) (Possible y, b) | length x > length y = (Possible y,b)
                                  | otherwise = (Possible x,a)

findSmallestPossible:: [(Integer,Cell)] -> Integer -> (Cell,(Integer, Integer))
findSmallestPossible row ind= foldl (\x y -> compareCells x (snd y,(fst y,ind))) (Possible [1..9],(ind,0)) row

findSmallestPossibleInGrid:: Grid -> (Cell,(Integer,Integer))
findSmallestPossibleInGrid grid = foldl (\x y -> compareCells x (findSmallestPossible (snd y) (fst y))) (Possible [1..9],(0,0)) grid'
              where  grid' = positions grid

replaceCell :: Integer -> (Integer, Cell) -> (Cell,(Integer,Integer)) -> (Cell, Cell)
replaceCell ind (cellInd, Fixed s) smallest = (Fixed s, Fixed s)
replaceCell ind (cellInd, Possible x) (Possible smallest,(smX,smY)) | length x > length smallest = (Possible x, Possible x)
                                                                    | smX /= cellInd || smY /= ind = (Possible x, Possible x)
                                                                    | otherwise = (Fixed (head x), Possible (tail x))

--------get next grids to find the solution-----------

nextRow :: Integer -> [(Integer,Cell)] -> (Cell,(Integer,Integer)) -> (Row,Row)
nextRow ind row smallest = foldl (\x y -> (fst x ++ [fst(replaceCell ind y smallest)], snd x ++ [snd (replaceCell ind y smallest)])) ([],[]) row

nextGrid :: Grid -> (Grid,Grid)
nextGrid grid = foldl (\x (a,y) -> (fst x ++ [fst (nextRow a y smallest)],snd x ++ [snd (nextRow a y smallest)] )) ([],[]) grid'
          where smallest = findSmallestPossibleInGrid grid
                grid' = positions grid

--TODO use later
chunk' :: Int -> [a] -> [[a]]
chunk' n list  = takeWhile (\x -> not (null x))  (map (take n) (iterate (drop n) list))

--check if grid is filled

isGridFilled :: Grid -> Bool
isGridFilled grid = null [1 | Possible _ <- concat grid] 

hasDups :: [Int] -> Bool
hasDups l = foldl (\x y -> if length (filter (== y) (l)) /= 1 then x || True else x) False l 

--check if grid is correct

isGridInvalid :: Grid -> Bool
isGridInvalid grid = 
   any isInvalidRow grid || any isInvalidRow (rotate grid) || any isInvalidRow (getSquares grid)
   where 
    isInvalidRow row = 
      let fixeds = [x | Fixed x <- row]
          emptyPossibles = [x | Possible x <- row, null x]
      in hasDups fixeds || not (null emptyPossibles)

 -- combine all functions

maybeAlternative :: Maybe a -> Maybe a -> Maybe a
maybeAlternative Nothing x = x
maybeAlternative x _ = x

solve :: Grid -> Maybe Grid
solve grid = solve' (pruneGrid grid)
  where 
    solve' g
      | isGridInvalid g = Nothing
      | isGridFilled g = Just g
      | otherwise = maybeAlternative (solve g1) (solve g2)
        where (g1,g2) = nextGrid g  

--input to path do pliku, a w pliku input to 81 znaków, gdzie . oznacza nieuzupełnione pole
--pierwsze 9 znaków to 1. wiersz, kolejne 9 to 2. wiersz itd.
main = do
          (firstArg:_) <-getArgs
          s <- readFile firstArg
          putStr (showGrid (solve (readGrid s)))
