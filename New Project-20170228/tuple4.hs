module Tuple (Digit(..), Tuple4, Grid4, digits, ref,gridRef, row, col, square, squareIndex, exists, mapTuple4, mapGrid4) where

data Digit = One | Two | Three | Four deriving (Eq,Ord,Enum,Show,Bounded)

type Tuple4 a = (a,a,a,a)
type Grid4 a = Tuple4 (Tuple4 a)

digits :: [Digit]
digits = [minBound..]

ref :: Tuple4 a -> Digit -> a
ref (a,_,_,_) One = a
ref (_,a,_,_) Two = a
ref (_,_,a,_) Three = a
ref (_,_,_,a) Four = a

row :: Grid4 a -> Digit -> Tuple4 a
row = ref

gridRef :: Grid4 a -> Digit -> Digit -> a
gridRef g x y = ref (ref g y) x

col :: Grid4 a -> Digit -> Tuple4 a
col ((a,_,_,_),(b,_,_,_),(c,_,_,_),(d,_,_,_)) One = (a,b,c,d)
col ((_,a,_,_),(_,b,_,_),(_,c,_,_),(_,d,_,_)) Two = (a,b,c,d)
col ((_,_,a,_),(_,_,b,_),(_,_,c,_),(_,_,d,_)) Three = (a,b,c,d)
col ((_,_,_,a),(_,_,_,b),(_,_,_,c),(_,_,_,d)) Four = (a,b,c,d)

square :: Grid4 a -> Digit -> Tuple4 a
square ((a,b,_,_),(c,d,_,_),_,_) One = (a,b,c,d)
square ((_,_,a,b),(_,_,c,d),_,_) Two = (a,b,c,d)
square (_,_,(a,b,_,_),(c,d,_,_)) Three = (a,b,c,d)
square (_,_,(_,_,a,b),(_,_,c,d)) Four = (a,b,c,d)

squareIndex :: Digit -> Digit -> Digit
squareIndex One One = One
squareIndex One Two = One
squareIndex Two One = One
squareIndex Two Two = One

squareIndex Three One = Two
squareIndex Three Two = Two
squareIndex Four One = Two
squareIndex Four Two = Two

squareIndex One Three = Three
squareIndex One Four = Three
squareIndex Two Three = Three
squareIndex Two Four = Three

squareIndex Three Three = Four
squareIndex Three Four = Four
squareIndex Four Three = Four
squareIndex Four Four = Four

exists :: (Eq a) => Tuple4 a -> a -> Bool
exists (a,b,c,d) cell = cell == a || cell == b || cell == c || cell == d

mapTuple4 :: (Digit -> a -> b) -> Tuple4 a -> Tuple4 b
mapTuple4 f (a,b,c,d) = (f One a, f Two b, f Three c, f Four d)

mapGrid4 :: (Digit -> Digit ->  a -> b) -> Grid4 a -> Grid4 b
mapGrid4 f (a,b,c,d) = (mapTuple4 (f One) a,mapTuple4 (f Two) b,mapTuple4 (f Three) c,mapTuple4 (f Four) d)

