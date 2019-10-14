--- Exercises: Eq Instances
--- Write the Eq instance for the datatype provided.

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x1 y1) (Two x2 y2) = (x1 == x2) && (y1 == y2)

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt x)   (TisAnInt y)   = x == y
    (==) (TisAString a) (TisAString b) = a == b
    (==) _              _              = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair x1 y1) (Pair x2 y2) = (x1 == x2) && (y1 == y2)

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x1 y1) (Tuple x2 y2) = (x1,y1) == (x2,y2)

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x1)   (Hello x2)   = x1 == x2
    (==) (Goodbye x1) (Goodbye x2) = x1 == x2
    (==) _            _            = False