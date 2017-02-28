import Tuple (Digit(..), Tuple4, Grid4, digits, ref,gridRef, row, col, square, squareIndex, exists, mapTuple4, mapGrid4)

data Cell = Unknown | Cell Digit deriving (Eq,Show)
type Sudoku = Grid4 Cell
type Solution = Grid4 Digit
type CellInference = (Cell, [Digit])
type Inference = Grid4 CellInference

sudoku = (  (Cell Four,Unknown,Unknown,Unknown),
            (Unknown,Cell Three,Unknown,Unknown),
            (Cell One,Unknown,Cell Two,Unknown),
            (Unknown,Unknown,Unknown,Cell One))

inferCell :: Sudoku -> Digit -> Digit -> CellInference
inferCell sudoku x y = (c, 
    if c == Unknown 
    then [d | d <- digits, 
        not (exists (col sudoku x) (Cell d)),
        not (exists (row sudoku y) (Cell d)),
        not (exists (square sudoku (squareIndex x y)) (Cell d))] 
    else [])
    where c = gridRef sudoku x y

infer :: Sudoku -> Inference
infer sudoku = (mapGrid4 (\row col cell -> inferCell sudoku col row) sudoku)

transform :: Digit -> Digit -> Digit -> Digit -> Digit -> CellInference -> CellInference
transform x y a col row (c,l)   | x == col && y == row = (Cell a,[])
                                | x == col || y == row || (squareIndex x y) == (squareIndex col row) = (c, filter (\e -> e /= a) l)
                                | otherwise = (c, l)


contractCell :: Inference -> Digit -> Digit -> CellInference -> Inference
contractCell i x y (Unknown, [a]) = mapGrid4 (\row col e -> transform x y a col row e) i
contractCell _ _ _ (Unknown, []) = error "unsolvable"
contractCell i _ _ _ = i

contract :: Inference -> Inference
contract i = foldl (\i (x,y) -> contractCell i x y (gridRef i x y)) i [(x,y) | x <- digits, y <- digits]

iter :: Inference -> Inference
iter i = if i == next 
        then next
        else iter next
    where next = contract i

getDigit :: Cell -> Digit
getDigit Unknown = error "unknown solution"
getDigit (Cell d) = d

solve :: Sudoku -> Solution
solve sudoku = mapGrid4 (\row col (c, _) -> getDigit c) (iter (infer sudoku))

main = putStrLn . show $ solve sudoku
