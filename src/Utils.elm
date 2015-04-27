module Utils where

import List


seriesInt : Int -> Int -> List Int
seriesInt low high =
  if | low >= high -> []
     | low == high - 1 -> [low]
     | otherwise ->
        let
            low' = low + ((high - low) // 2)
        in
            low :: seriesInt low' high

seriesFloat : Float -> Float -> List Float
seriesFloat low high =
  if low >= high - 0.0001
  then
    []
  else
    let low' = low + ((high - low) / 2)
    in
      low :: seriesFloat low' high


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
takeWhile pred list = case list of
  []  -> []
  x :: xs ->
    if pred x
    then
      x :: takeWhile pred xs
    else
      []

iterate : Int -> (a -> a) -> a -> List a
iterate n f a =
  if n <= 0
  then
    []
  else
    a :: iterate (n - 1) f (f a)
