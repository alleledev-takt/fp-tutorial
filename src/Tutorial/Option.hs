{-# OPTIONS_GHC -Wall #-}
module Tutorial.Option where

-------------------------------------------------------------------------------
-- The `Option` Data Type
--
-- An algebraic data type (specifically, a sum type), that allows us
-- to express that some value is optional. There's either `Some` value
-- of type `a` or there's `None`.
data Option a
  = Some a
  | None

-- an `Option Int` with shape `Some`
val1 :: Option Int
val1 = Some 10

-- an `Option Int` with shape `None`
val2 :: Option Int
val2 = None

-- adding together two `Option Int` using pattern-matching
f :: Option Int -> Option Int -> Option Int
f x y =
  case (x, y) of
    (Some x', Some y') -> Some (x' + y')
    (None, Some _) -> None
    (Some _, None) -> None
    (None, None) -> None

-- adding together plain `Int`s
add :: Int -> Int -> Int
add x y = x + y

-- scala's `getOrElse`
getOrElse :: Option a -> a -> a
getOrElse x def =
  case x of
    (Some val) -> val
    None -> def

-- an arbitrary program dealing with optionality by using `getOrElse`
program :: Int
program = do
  let x = Some 1
      y = Some 2
  add (getOrElse x 0) (getOrElse y 0)

-------------------------------------------------------------------------------
-- Color data type
--
-- It allows us to talk about Red or Green or Blue. (but no other colors :( ).
data Color
  = Red
  | Green
  | Blue

-- converting a `Color` to a `String` using pattern-matching.
--
-- This works conveniently because `Color` only has 3 possible values.
colorToString :: Color -> String
colorToString c =
  case c of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"

-- converting a `String` to a `Color`.
--
-- This requires us to use the `Option` type because there's A LOT
-- more `String`s than there are colors, and we have to write a
-- function that handles all possible inputs.
stringToColor :: String -> Option Color
stringToColor c =
  case c of
    "red" -> Some Red
    "green" -> Some Green
    "blue" -> Some Blue
    -- the _ here is a wildcard match - a catch-all case
    _ -> None

-------------------------------------------------------------------------------
-- Working with Recursive Data Types and Recursion
-------------------------------------------------------------------------------
-- an approximation of Haskell and Scala's list data type
{-
data [a]
  = []
  | a : [a]
-}

-- just a list of `Int`
someVals1 :: [Int]
someVals1 = [1,2,3,4]

-- the same list of `Int` as someVals1
someVals2 :: [Int]
someVals2 = 1 : [2,3,4]

-- looking up a number recursively. we have to handle two cases, and
-- we do so with pattern-matching.
lookupNumber :: Int -> [Int] -> Option Int
lookupNumber x xs =
  case xs of
    (v:xs') -> if v == x then (Some v) else lookupNumber x xs'
    [] -> None

-- the same as `lookupNumber` above, but works for all data types that
-- implement the `Eq` interface, giving them access to the `==`
-- operator.
lookup2 :: Eq a => a -> [a] -> Option a
lookup2 x xs =
  case xs of
    (v:xs') -> if v == x then (Some v) else lookup2 x xs'
    [] -> None

-- a scala version for lookup2
-- def lookup2[A](x: A, xs: List[A]): Option[A] = xs match {
--   case List(v,xs') => if v == x then Some(x) else lookup2(x, xs')
--   case List() => None
-- }
--

example1 :: Option Int
example1 = lookupNumber 10 []

example2 :: Option Int
example2 = lookupNumber 10 [1,2,3,4]

example3 :: Option Int
example3 = lookupNumber 10 [1,2,10,3,5,10,7]

example4 :: Option Int
example4 = lookup2 10 [1,2,3,4]

example5 :: Option String
example5 = lookup2 "cat" ["fish", "red", "cat", "blue"]

-------------------------------------------------------------------------------
-- Using Sum Types to Simplify Programs
-------------------------------------------------------------------------------

-- a simple switch type
data TwoSwitch = TwoSwitch Bool Bool

-- toggling one switch using pattern-matching
toggle :: Bool -> Bool
toggle True = False
toggle False = True

-- toggling a switch using if-else
toggle2 :: Bool -> Bool
toggle2 x =
  if x == True
  then False
  else True

-- toggling a switch in a very silly way. it's like some apartments
-- I've lived at and it never makes sense...
toggleSwitch :: TwoSwitch -> TwoSwitch
toggleSwitch (TwoSwitch a b) = TwoSwitch (toggle a) (toggle b)

-- representing all possible states our switch can be in.
data SwitchState
  = AllOff
  | FirstOn
  | SecondOn
  | AllOn

-- determining the state of a `TwoSwitch` using pattern-matching
inspectSwitch :: TwoSwitch -> SwitchState
inspectSwitch (TwoSwitch a b) =
  case (a,b) of
    (True, True) -> AllOn
    (True, False) -> FirstOn
    (False, True) -> SecondOn
    (False, False) -> AllOff

-- makes a switch with the desired state
setSwitch :: SwitchState -> TwoSwitch
setSwitch state =
  case state of
    AllOff -> TwoSwitch False False
    FirstOn -> TwoSwitch True False
    SecondOn -> TwoSwitch False True
    AllOn -> TwoSwitch True True

-- given a `SwitchState`, computes a next "reasonable" `SwitchState`
-- using pattern-matching
nextState :: SwitchState -> SwitchState
nextState state =
  case state of
    AllOff -> FirstOn
    FirstOn -> SecondOn
    SecondOn -> AllOn
    AllOn -> AllOff

-- a smarter `toggle` function for our TwoSwitch
cycleSwitch :: TwoSwitch -> TwoSwitch
cycleSwitch switch =
  let currentState = inspectSwitch switch
      goToState = nextState currentState
  in setSwitch goToState

-- bonus: does the same thing as the function above, but uses a terser
-- but harder-to-read style of programming called "point-free".
--
-- It's a style of programming that skips on naming function
-- arguments, and instead achieves the results by using only function
-- composition. This will come up sometimes in functional programming,
-- but is Never strictly necessary. Provided as a reference. It's more
-- common in Haskell than in Scala.
cycleSwitchPointFree :: TwoSwitch -> TwoSwitch
cycleSwitchPointFree = setSwitch . nextState . inspectSwitch
