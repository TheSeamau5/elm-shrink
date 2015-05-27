module Shrink
  ( Shrinker
  , noShrink
  , void
  , bool
  , order
  , int
  , atLeastInt
  , float
  , atLeastFloat
  , char
  , atLeastChar
  , character
  , string
  , maybe
  , result
  , list
  , array
  , tuple
  , tuple3
  , tuple4
  , tuple5
  , convert
  , map
  , andMap
  , keepIf
  , dropIf
  , merge
  ) where
{-| Library containing a collection of basic shrinking strategies and
helper functions to help you construct shrinking strategies.

# Shinker
@docs Shrinker

# Shrinkers
@docs noShrink, void, bool, order, int, atLeastInt, float, atLeastFloat, char, atLeastChar, character, string, maybe, result, list, array, tuple, tuple3, tuple4, tuple5

# Useful functions
@docs convert, keepIf, dropIf, map, andMap, merge

-}

import List
import Array  exposing (Array)
import Char
import String
import Trampoline exposing (Trampoline(..), trampoline)

{-| Shrinker type.
A shrinker is a function that takes a value and returns a list of values that
are in some sense "smaller" than the given value. If there are no such values
conceptually, then the shrinker should just return the empty list.
-}
type alias Shrinker a = a -> List a

---------------
-- SHRINKERS --
---------------

{-| Empty shrinker. Always returns the empty list.
-}
noShrink : Shrinker a
noShrink _ = []

{-| Shrink the empty tuple. Equivalent to `noShrink`.
-}
void : Shrinker ()
void = noShrink

{-| Shrinker of bools.
-}
bool : Shrinker Bool
bool b = case b of
  True  -> [False]
  False -> []

{-| Shrinker of Order.
-}
order : Shrinker Order
order o = case o of
  GT -> [EQ, LT]
  LT -> [EQ]
  EQ -> []


{-| Shrinker of integers.
-}
int : Shrinker Int
int n =
  if n < 0
  then
    -n :: List.map ((*) -1) (seriesInt 0 -n)
  else
    seriesInt 0 n

{-| Construct a shrinker of ints which considers the given int to
be most minimal.
-}
atLeastInt : Int -> Shrinker Int
atLeastInt min n =
  if n < 0 && n >= min
  then
    -n :: List.map ((*) -1) (seriesInt 0 -n)
  else
    seriesInt (max 0 min) n

{-| Shrinker of floats.
-}
float : Shrinker Float
float n =
  if n < 0
  then
    -n :: List.map ((*) -1) (seriesFloat 0 -n)
  else
    seriesFloat 0 n


{-| Construct a shrinker of floats which considers the given float to
be most minimal.
-}
atLeastFloat : Float -> Shrinker Float
atLeastFloat min n =
  if n < 0 && n >= min
  then
    -n :: List.map ((*) -1) (seriesFloat 0 -n)
  else
    seriesFloat (max 0 min) n


{-| Shrinker of chars.
-}
char : Shrinker Char
char =
  convert Char.fromCode Char.toCode int

{-| Construct a shrinker of chars which considers the given char to
be most minimal.
-}
atLeastChar : Char -> Shrinker Char
atLeastChar char =
  convert Char.fromCode Char.toCode (atLeastInt (Char.toCode char))

{-| Shrinker of chars which considers the empty space as the most
minimal char and omits the control key codes.

Equivalent to:

    atLeastChar (Char.fromCode 32)
-}
character : Shrinker Char
character =
  atLeastChar (Char.fromCode 32)


{-| Shrinker of strings. Considers the empty string to be the most
minimal string and the space to be the most minimal char.

Equivalent to:

    convert String.fromList String.toList (list character)
-}
string : Shrinker String
string =
  convert String.fromList String.toList (list character)


{-| Maybe shrinker constructor.
Takes a shrinker of values and returns a shrinker of Maybes.
-}
maybe : Shrinker a -> Shrinker (Maybe a)
maybe shrink m = case m of
  Just a  -> Nothing :: List.map Just (shrink a)
  Nothing -> []

{-| Result shrinker constructor.
Takes a shrinker of errors and a shrinker of values and returns a shrinker
of Results.
-}
result : Shrinker error -> Shrinker value -> Shrinker (Result error value)
result shrinkError shrinkValue r = case r of
  Ok value  -> List.map Ok  (shrinkValue value)
  Err error -> List.map Err (shrinkError error)

{-| List shrinker constructor.
Takes a shrinker of values and returns a shrinker of Lists.
-}
list : Shrinker a -> Shrinker (List a)
list shrink l =
  let
      -- n : Int
      n = List.length l

      -- shrinkOne : Shrinker a -> Shrinker (List a)
      shrinkOne l = case l of
        [] -> []
        x :: xs ->
          List.map (flip (::) xs) (shrink x)
          ++ List.map ((::) x) (shrinkOne xs)

      -- removes : Int -> Int -> Shrinker (List a)
      removes k n l =
        if | k > n          -> []
           | List.isEmpty l -> [[]]
           | otherwise ->
               let
                   first = List.take k l -- List a
                   rest  = List.drop k l -- List a
               in
                   rest :: List.map ((++) first) (removes k (n - k) rest)

  in
      List.concatMap (\k -> removes k n l) (takeWhile (\x -> x > 0) (iterate n (\n -> n // 2) n))
      ++ shrinkOne l


{-| Array shrinker constructor.
Takes a shrinker of values and returns a shrinker of Arrays.
-}
array : Shrinker a -> Shrinker (Array a)
array shrink =
  convert Array.fromList Array.toList (list shrink)


{-| 2-Tuple shrinker constructor.
Takes a tuple of shrinkers and returns a shrinker of tuples.
-}
tuple : (Shrinker a, Shrinker b) -> Shrinker (a, b)
tuple (shrinkA, shrinkB) (a, b) =
     List.map ((,) a) (shrinkB b)
  ++ List.map (flip (,) b) (shrinkA a)
  ++ List.map2 (,) (shrinkA a) (shrinkB b)



{-| 3-Tuple shrinker constructor.
Takes a tuple of shrinkers and returns a shrinker of tuples.
-}
tuple3 : (Shrinker a, Shrinker b, Shrinker c) -> Shrinker (a, b, c)
tuple3 (shrinkA, shrinkB, shrinkC) (a, b, c) =
     List.map  (\c   -> (a,b,c)) (shrinkC c)
  ++ List.map  (\b   -> (a,b,c)) (shrinkB b)
  ++ List.map  (\a   -> (a,b,c)) (shrinkA a)
  ++ List.map2 (\b c -> (a,b,c)) (shrinkB b) (shrinkC c)
  ++ List.map2 (\a c -> (a,b,c)) (shrinkA a) (shrinkC c)
  ++ List.map2 (\a b -> (a,b,c)) (shrinkA a) (shrinkB b)
  ++ List.map3 (,,) (shrinkA a)  (shrinkB b) (shrinkC c)



{-| 4-Tuple shrinker constructor.
Takes a tuple of shrinkers and returns a shrinker of tuples.
-}
tuple4 : (Shrinker a, Shrinker b, Shrinker c, Shrinker d) -> Shrinker (a, b, c, d)
tuple4 (shrinkA, shrinkB, shrinkC, shrinkD) (a, b, c, d) =
     List.map  (\d     -> (a,b,c,d)) (shrinkD d)
  ++ List.map  (\c     -> (a,b,c,d)) (shrinkC c)
  ++ List.map  (\b     -> (a,b,c,d)) (shrinkB b)
  ++ List.map  (\a     -> (a,b,c,d)) (shrinkA a)
  ++ List.map2 (\c d   -> (a,b,c,d)) (shrinkC c) (shrinkD d)
  ++ List.map2 (\b d   -> (a,b,c,d)) (shrinkB b) (shrinkD d)
  ++ List.map2 (\a d   -> (a,b,c,d)) (shrinkA a) (shrinkD d)
  ++ List.map2 (\b c   -> (a,b,c,d)) (shrinkB b) (shrinkC c)
  ++ List.map2 (\a c   -> (a,b,c,d)) (shrinkA a) (shrinkC c)
  ++ List.map2 (\a b   -> (a,b,c,d)) (shrinkA a) (shrinkB b)
  ++ List.map3 (\b c d -> (a,b,c,d)) (shrinkB b) (shrinkC c) (shrinkD d)
  ++ List.map3 (\a c d -> (a,b,c,d)) (shrinkA a) (shrinkC c) (shrinkD d)
  ++ List.map3 (\a b c -> (a,b,c,d)) (shrinkA a) (shrinkB b) (shrinkC c)
  ++ List.map4 (,,,)     (shrinkA a) (shrinkB b) (shrinkC c) (shrinkD d)





{-| 5-Tuple shrinker constructor.
Takes a tuple of shrinkers and returns a shrinker of tuples.
-}
tuple5 : (Shrinker a, Shrinker b, Shrinker c, Shrinker d, Shrinker e) -> Shrinker (a, b, c, d, e)
tuple5 (shrinkA, shrinkB, shrinkC, shrinkD, shrinkE) (a, b, c, d, e) =
     List.map  (\e       -> (a,b,c,d,e)) (shrinkE e)
  ++ List.map  (\d       -> (a,b,c,d,e)) (shrinkD d)
  ++ List.map  (\c       -> (a,b,c,d,e)) (shrinkC c)
  ++ List.map  (\b       -> (a,b,c,d,e)) (shrinkB b)
  ++ List.map  (\a       -> (a,b,c,d,e)) (shrinkA a)
  ++ List.map2 (\d e     -> (a,b,c,d,e)) (shrinkD d) (shrinkE e)
  ++ List.map2 (\c e     -> (a,b,c,d,e)) (shrinkC c) (shrinkE e)
  ++ List.map2 (\b e     -> (a,b,c,d,e)) (shrinkB b) (shrinkE e)
  ++ List.map2 (\a e     -> (a,b,c,d,e)) (shrinkA a) (shrinkE e)
  ++ List.map2 (\c d     -> (a,b,c,d,e)) (shrinkC c) (shrinkD d)
  ++ List.map2 (\b d     -> (a,b,c,d,e)) (shrinkB b) (shrinkD d)
  ++ List.map2 (\a d     -> (a,b,c,d,e)) (shrinkA a) (shrinkD d)
  ++ List.map2 (\b c     -> (a,b,c,d,e)) (shrinkB b) (shrinkC c)
  ++ List.map2 (\a c     -> (a,b,c,d,e)) (shrinkA a) (shrinkC c)
  ++ List.map2 (\a b     -> (a,b,c,d,e)) (shrinkA a) (shrinkB b)
  ++ List.map3 (\c d e   -> (a,b,c,d,e)) (shrinkC c) (shrinkD d) (shrinkE e)
  ++ List.map3 (\b d e   -> (a,b,c,d,e)) (shrinkB b) (shrinkD d) (shrinkE e)
  ++ List.map3 (\a d e   -> (a,b,c,d,e)) (shrinkA a) (shrinkD d) (shrinkE e)
  ++ List.map3 (\a c d   -> (a,b,c,d,e)) (shrinkA a) (shrinkC c) (shrinkD d)
  ++ List.map3 (\a b d   -> (a,b,c,d,e)) (shrinkA a) (shrinkB b) (shrinkD d)
  ++ List.map3 (\a b c   -> (a,b,c,d,e)) (shrinkA a) (shrinkB b) (shrinkC c)
  ++ List.map4 (\b c d e -> (a,b,c,d,e)) (shrinkB b) (shrinkC c) (shrinkD d) (shrinkE e)
  ++ List.map4 (\a c d e -> (a,b,c,d,e)) (shrinkA a) (shrinkC c) (shrinkD d) (shrinkE e)
  ++ List.map4 (\a b d e -> (a,b,c,d,e)) (shrinkA a) (shrinkB b) (shrinkD d) (shrinkE e)
  ++ List.map4 (\a b c d -> (a,b,c,d,e)) (shrinkA a) (shrinkB b) (shrinkC c) (shrinkD d)
  ++ List.map5 (,,,,)        (shrinkA a) (shrinkB b) (shrinkC c) (shrinkD d) (shrinkE e)



----------------------
-- HELPER FUNCTIONS --
----------------------

{-| Convert a Shrinker of a's into a Shrinker of b's using two inverse functions.

If you use this function as follows:

    shrinkerB = f g shrinkerA

Make sure that

    `f(g(x)) == x` for all x

Or else this process will generate garbage.
-}
convert : (a -> b) -> (b -> a) -> Shrinker a -> Shrinker b
convert f f' shrink b =
  List.map f (shrink (f' b))


{-| Filter out the results of a shrinker. The resulting shrinker
will only produce shrinks which satisfy the given predicate.
-}
keepIf : (a -> Bool) -> Shrinker a -> Shrinker a
keepIf predicate shrink a =
  List.filter (predicate) (shrink a)

{-| Filter out the results of a shrinker. The resulting shrinker
will only throw away shrinks which satisfy the given predicate.
-}
dropIf : (a -> Bool) -> Shrinker a -> Shrinker a
dropIf predicate =
  keepIf (not << predicate)


{-| Merge two shrinkers.
-}
merge : Shrinker a -> Shrinker a -> Shrinker a
merge shrink1 shrink2 a =
  unique
    (shrink1 a ++ shrink2 a)

{-| Re-export of `List.map`
This is useful in order to compose shrinkers, especially when used in
conjunction with `andMap`.

Example:

    type alias Vector =
      { x : Float
      , y : Float
      , z : Float
      }

    vector : Shrinker Float
    vector {x,y,z} =
      Vector
        `map`    float x
        `andMap` float y
        `andMap` float z

-}
map : (a -> b) -> List a -> List b
map =
  List.map


{-| Apply a list of functions on a list of values.

    andMap = List.map2 (<|)

This is useful in order to compose shrinkers, especially when used in
conjunction with `andMap`.
-}
andMap : List (a -> b) -> List a -> List b
andMap =
  List.map2 (<|)


-----------------------
-- PRIVATE FUNCTIONS --
-----------------------
seriesInt : Int -> Int -> List Int
seriesInt low high =
  trampoline (seriesInt' low high [])

seriesInt' low high accum =
  if | low >= high -> Done accum
     | low == high - 1 -> Done (accum ++ [low])
     | otherwise ->
        let
            low' = low + ((high - low) // 2)
        in
            Continue (\() -> seriesInt' low' high (accum ++ [low]))


seriesFloat : Float -> Float -> List Float
seriesFloat low high =
  trampoline (seriesFloat' low high [])

seriesFloat' low high accum =
  if low >= high - 0.0001
  then
    Done accum
  else
    let
        low' = low + ((high - low) / 2)
    in
        Continue (\() -> seriesFloat' low' high (accum ++ [low]))


unique : List a -> List a
unique list = case list of
  [] -> []
  x :: xs ->
    if List.member x xs
    then
      unique xs
    else
      x :: unique xs


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  trampoline (takeWhile' predicate list [])

takeWhile' predicate list accum = case list of
  [] ->
    Done accum
  x :: xs ->
    if predicate x
    then
      Continue (\() -> takeWhile' predicate xs (accum ++ [x]))
    else
      Done accum

iterate : Int -> (a -> a) -> a -> List a
iterate n f a =
  trampoline (iterate' n f a [])

iterate' n f a accum =
  if n <= 0
  then
    Done accum
  else
    Continue (\() -> iterate' (n - 1) f (f a) (accum ++ [a]))
