---
author: "Mulling"
title: "AoC 2023"
date: 2023-12-04T23:25:02-03:00
description: "Haskell type torture"
draft: false
---

- [Day 1](#day-1)
- [Day 2](#day-2)

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
