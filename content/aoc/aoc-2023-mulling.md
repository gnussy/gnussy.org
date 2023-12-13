---
author: "Mulling"
title: "AoC 2023"
date: 2023-12-04T23:25:02-03:00
description: "Haskell type torture"
draft: false
---

- [Day 1](#day-1)
- [Day 2](#day-2)
- [Day 3](#day-3)
- [Day 4](#day-4)
- [Day 5](#day-5)

## Day 1
This problem kinda stinks, plus I'm still very rusty with the ol' Haskell.

```haskell
import Control.Applicative

import Data.Char
import Data.List

main :: IO ()
main = interact (show . sum . map (liftA2 (\ x y -> read [x, y] :: Int) head last . digits) . lines)
  where
    digits [] = []
    digits l@(x:xs) | isPrefixOf "one"   l = '1':digits xs
                    | isPrefixOf "two"   l = '2':digits xs
                    | isPrefixOf "three" l = '3':digits xs
                    | isPrefixOf "four"  l = '4':digits xs
                    | isPrefixOf "five"  l = '5':digits xs
                    | isPrefixOf "six"   l = '6':digits xs
                    | isPrefixOf "seven" l = '7':digits xs
                    | isPrefixOf "eight" l = '8':digits xs
                    | isPrefixOf "nine"  l = '9':digits xs
                    | isDigit x            =  x :digits xs
                    | otherwise            =     digits xs
    digits' = filter isDigit
```

It's quite funny that the final binary has 14Mb...

## Day 2
### Part 1
Overall the solution is a convoluted mess, only because "normal" strings don't have `splitOn` and we have to resort to using `Data.Text`. Still getting the hang of Applicatives again.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

main :: IO ()
main = interact (show . sum . map (valid . ([(";"`T.splitOn`)]<*>) . T.splitOn ":" . T.pack). lines)
  where
    valid [[game], sets] | all (foldr (\xs acc -> acc && valid' (T.words xs)) True) ([(","`T.splitOn`)] <*> sets) = (read . last . words) (T.unpack game) :: Int
    valid _ = 0
    valid' [num, color] | color == "red"   = parse num <= 12
                        | color == "green" = parse num <= 13
                        | color == "blue"  = parse num <= 14
    valid' _ = False
    parse xs = read (T.unpack xs) :: Int
```
### Part 2
Another round of type tetris.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

main :: IO ()
main = interact (show . sum. map (power . tail . T.splitOn ":" . T.pack). lines)
  where
    power sets = go [0, 0, 0] $ concat ([T.words] <*> sets)
    go :: [Int] -> [T.Text] -> Int
    go [r, g, b] (num:color:xs) | T.isPrefixOf "red"   color = go [max (parse num) r, g, b] xs
                                | T.isPrefixOf "green" color = go [r, max (parse num) g, b] xs
                                | T.isPrefixOf "blue"  color = go [r, g, max (parse num) b] xs
    go acc _ = product acc
    parse xs = read (T.unpack xs) :: Int
```

And without Data.Text things get way simpler.

```haskell
import Data.List

main :: IO ()
main = interact (show . sum . ([power . tail . dropWhile (/=':')] <*>) . lines)
  where
    power :: String -> Int
    power = go [0,0,0] . words
    go [r, g, b] (num:color:xs) | isPrefixOf "red"   color = go [max (read num) r, g, b] xs
                                | isPrefixOf "green" color = go [r, max (read num) g, b] xs
                                | isPrefixOf "blue"  color = go [r, g, max (read num) b] xs
    go acc _ = product acc
```

## Day 3
### Part 1
Not looking forward to part 2.

```haskell
import Control.Applicative
import Data.Char

zipper :: Maybe [Bool] -> [[Char]] -> [([Char], [Bool])]
zipper _ [] = []
zipper (Just prev) [xs] = [(xs, prev)]
zipper (Just prev) (xs : xss@(next : _)) = (xs, zipWith (||) prev (getSymbols <$> next)) : zipper (Just $ getSymbols <$> xs) xss
zipper Nothing (xs : xss@(next : _)) = (xs, getSymbols <$> next) : zipper (Just $ getSymbols <$> xs) xss
zipper Nothing _ = []

go :: Bool -> [Char] -> [Char] -> [Bool] -> [[Char]]
go True acc [] [] = [acc]
go _ _ [] [] = []
go emit acc (c : cs) (p : ps)
  | isDigit c = go (emit || p) (acc <> [c]) cs ps
  | emit || p || c /= '.' = case acc of
      [] -> go (p || c /= '.') [] cs ps
      _ -> acc : go (p || c /= '.') [] cs ps
  | otherwise = go False [] cs ps
go _ _ _ _ = []

getSymbols :: Char -> Bool
getSymbols = liftA2 (&&) (not . isDigit) (/= '.')

tags :: ([Char], [Bool]) -> [[Char]]
tags (xs, syms) = go False [] xs syms

main :: IO ()
main = interact (show . sum . map (\xs -> read xs :: Int) . concatMap tags . zipper Nothing . lines)
```

### Part 2
```haskell
-- TODO:
```

## Day 4
This was a fun day of mostly figuring out Haskell's standard library.

### Part 1
```haskell
import Control.Applicative
import Data.Set qualified as S

main :: IO ()
main =
  interact
    ( show
        . sum
        . map ((2 ^) . pred)
        . filter (> 0)
        . ( S.size
              . liftA2 S.intersection head last
              . (S.fromList . ((\xs -> read xs :: Int) <$>) . words <$>)
              . split (== '|')
              . tail
              . dropWhile (/= ':')
              <$>
          )
        . lines
    )
  where
    split fn xs = case break fn xs of
      (a, _ : b) -> a : split fn b
      (a, _) -> [a]
```
With a little bit of massagin', we can make the final solution 3 loc, tho, It does run a little slower. My guess is that `Data.Set.intersection` much faster than `Data.List.intersect`.
```haskell
import Data.List

main :: IO ()
main =
  interact
    ( show
        . sum
        . map ((2 ^) . pred)
        . filter (> 0)
        . ( length
              . uncurry intersect
              . ( \(a, b) ->
                    ( read <$> words a,
                      read <$> words (tail b)
                    ) ::
                      ([Int], [Int])
                )
              . span (/= '|')
              . tail
              . dropWhile (/= ':')
              <$>
          )
        . lines
    )
```

### Part 2
This took some helping to run in a decent time...
```haskell
import Data.List

main :: IO ()
main =
  interact
    ( show
        . sum
        . foldr
          ( (\i acc -> 1 + sum (take i acc) : acc)
              . ( length . uncurry intersect . \(a, b) ->
                    ( read
                        <$> words a,
                      read <$> words (tail b)
                    ) ::
                      ([Int], [Int])
                )
              . span (/= '|')
              . tail
              . dropWhile (/= ':')
          )
          []
        . lines
    )
```

## Day 5
### Part 1
Pretty boring day, 90% of this is parsing -- and preprocessing for part 2. Instead of using Parsec, I've made my own Applicative parser.

```haskell
{-# OPTIONS_GHC -Wall -Wextra #-}

import Control.Applicative
import Data.Char

data R a = R (a, String) | E deriving (Show)

newtype P a = P (String -> R a)

parse :: P a -> String -> R a
parse (P a) = a

instance Functor R where
  fmap _ E = E
  fmap f (R (a, xs)) = R (f a, xs)

instance Functor P where
  fmap f p = P $ fmap f . parse p

instance Applicative P where
  pure a = P $ \xs -> R (a, xs)
  p' <*> p'' = P $ \xs -> f <$> parse p' $ xs
    where
      f (R (a, xs)) = parse (a <$> p'') xs
      f E = E

instance Alternative P where
  empty = P $ const E
  p' <|> p'' = P $ \xs -> f xs <$> parse p' $ xs
    where
      f xs E = parse p'' xs
      f _ (R (a, xs)) = R (a, xs)

char :: (Char -> Bool) -> P Char
char f = P f'
  where
    f' (x : xs) | f x = R (x, xs)
    f' _ = E

token :: P a -> P a
token p = w *> p <* w
  where
    w = many . char $ isSpace

string :: String -> P String
string = foldr (\x -> (<*>) $ (:) <$> char (== x)) $ pure []

numbers :: P [Int]
numbers = token $ many $ read <$> (some . char) isDigit <* char isSpace

identifier :: String -> P String
identifier = token . string

newtype Range = Range (Int, Int, Int) deriving (Show)

seeds :: P [Int]
seeds = identifier "seeds:" *> numbers

ranges :: String -> P [Range]
ranges xs = go <$> (identifier xs *> numbers)
  where
    go (t : f : r : xs') = Range (t, f, r) : go xs'
    go _ = []

parser :: P ([Int], [[Range]])
parser =
  join
    <$> seeds
    <*> ranges "seed-to-soil map:"
    <*> ranges "soil-to-fertilizer map:"
    <*> ranges "fertilizer-to-water map:"
    <*> ranges "water-to-light map:"
    <*> ranges "light-to-temperature map:"
    <*> ranges "temperature-to-humidity map:"
    <*> ranges "humidity-to-location map:"
  where
    join a b c d e f g h = (a, [b, c, d, e, f, g, h]) -- Is there a better way to do this ???

mapping :: Int -> [Range] -> Int
mapping s ((Range (t, f, r)) : rs)
  | s >= f && s <= f + r - 1 = t + (s - f)
  | otherwise = mapping s rs
mapping s [] = s

main :: IO ()
main = interact (show . minimum . liftA2 run fst snd . unpack . parse parser)
  where
    unpack (R (r, _)) = r
    unpack _ = undefined
    run (x : xs) rs = next x rs : run xs rs
    run _ _ = []
    next s (r : rs) = next (mapping s r) rs
    next s _ = s
```
