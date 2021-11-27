module EasySudokuSolver where
  
import Data.List
import Data.List.Split
import Data.Maybe

less :: (Int, Int) -> (Int, Int) ->  Bool
less (x,y) (x',y')
  | x <= x' && y <= y' = True
  | x > x' = False
  | y > y' = False

findSquares :: [[Int]] -> [[Int]]
findSquares puzzle
  = map concat a
  where
    a = concatMap (chunksOf 3) . transpose $ map (chunksOf 3) puzzle

findPos :: (Int, Int) -> [[Int]] -> Int
findPos (x, y) puzzle
  = puzzle !! x !! y


checkPos :: Int -> Int -> [[Int]] -> [Int]
checkPos x y puzzle
  | findPos (x,y) puzzle == 0 = [1..9] \\ (([1..9] \\ checkRow x) ++ ([1..9] \\ checkColumn y) ++ ([1..9] \\ checkSquare sqrpos))
  | otherwise = []
  where
    sqrpos = whichSquare (x,y)
    checkRow n = [1..9] \\ puzzle !! n
    checkSquare n = [1..9] \\ findSquares puzzle !! n
    checkColumn n = [1..9] \\ transpose puzzle !! n


whichSquare :: (Int, Int) -> Int
whichSquare pos
  | pos `less` (2,2) = 0
  | pos `less` (5,2) = 1
  | pos `less` (8,2) = 2
  | pos `less` (2,5) = 3
  | pos `less` (5,5) = 4
  | pos `less` (8,5) = 5
  | pos `less` (2,8) = 6
  | pos `less` (5,8) = 7
  | pos `less` (8,8) = 8

findSolvableValues :: [[Int]] -> [[[Int]]]
findSolvableValues puzzle = map (\x -> zipWith (\x y-> checkPos x y puzzle)(replicate 9 x) [0..8]) [0..8]
findSingletons :: [[Int]] -> [[Bool]]
findSingletons puzzle = chunksOf 9 [(==1). length $ (findSolvableValues puzzle !! x !! y) | x <-[0..8], y<-[0..8]]

replace :: Int -> Int -> [Int] ->[Int]
replace (-1) _ list = list
replace pos n list
  =  h1 ++ [n] ++ tail h2
  where
    (h1, h2) = splitAt pos list


sudoku :: [[Int]] -> [[Int]]
sudoku puzzle
  | newPuzzle == puzzle = puzzle
  | otherwise = sudoku newPuzzle
  where
    positions = map (elemIndex True) (findSingletons puzzle)
    replaces = map (fromMaybe (-1)) positions
    values = map (filter ((==1) . length)) (findSolvableValues puzzle)
    newPuzzle = map (\x -> replace (replaces !! x) (head. concat $ values !! x) (puzzle !! x)) [0..8]




----------------------------------------------------------------------------------------------
--Puzzles

puzzle :: [[Int]]
puzzle = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,0,0,0,0,6,0],
          [8,0,0,0,6,0,0,0,3],
          [4,0,0,8,0,3,0,0,1],
          [7,0,0,0,2,0,0,0,6],
          [0,6,0,0,0,0,2,8,0],
          [0,0,0,4,1,9,0,0,5],
          [0,0,0,0,8,0,0,7,9]]
          
puzzle2 :: [[Int]]          
puzzle2 =  [[8,4,5,0,0,9,0,0,6],
            [0,0,3,8,0,1,0,5,0],
            [0,0,9,5,0,7,4,3,8],
            [4,3,0,0,1,8,0,0,0],
            [7,0,1,0,5,6,3,8,0],
            [2,0,0,0,9,0,7,6,1],
            [0,6,0,9,3,0,8,0,7],
            [0,8,4,1,7,0,0,0,5],
            [9,1,0,6,0,0,2,4,0]]
