---
author: "Daniel Boll"
title: "AoC 2023"
date: 2023-12-01T09:44:14-03:00
description: "Using Elixir & OCaml"
draft: false
---

- [Day 1 - Elixir](#day-1-elixir)

## Day 1 Elixir

Here am I once again saving Christmas, duty's calling amirite?

The `Trebuchet` challenge from Advent of Code already got me started in the Pipe chain rabbit hole in Elixir, my first solution was simple

```elixir
defmodule Day1 do
  @spec part_one(String.t()) :: integer()
  def part_one(file) do
    load_file(file) # Load the file, LOL
    |> Enum.map(fn calibration -> # Then I would map for each line
      numbers =
        calibration
        |> String.split("") # Split the characters of each line
        |> Enum.filter(&Regex.match?(~r/\d/, &1)) # So I can filter only the digits
      
      # Resulting in something like ["1", "2", "5"], ["7"]

      numbers # Now the plan was to concatenate the first and last digits of this list
      |> Kernel.hd() # Taking the head
      |> Kernel.<>(List.last(numbers)) # And concatenating into the last digit
      |> String.to_integer() # Converting into integer
    end)
    |> Enum.sum() # And then summing in the end
  end

  @spec load_file(String.t()) :: [String.t()]
  def load_file(file) do
    File.read!(file)
    |> String.trim_trailing()
    |> String.split("\n")
  end
end
```

Of course not much later I would spend a bit more time to try to achieve through a full pipe chain, and I ended up with this:

```elixir
defmodule Day1 do
  @spec part_one(String.t()) :: integer()
  def part_one(file) do
    File.read!(file)
    |> String.trim_trailing()
    |> String.split("\n")
    |> Enum.reduce(0, fn calibration, acc -> # Instead of mapping then summing, a reduce fits better
      ~r/\d/
      |> Regex.scan(calibration) # I know scan for the digits I want, but the scan returns me:
                                 # A list of lists is returned, where each entry in the primary list represents a
                                 # match and each entry in the secondary list represents the captured contents.
      |> List.flatten()
      |> (&(List.first(&1) <> List.last(&1))).() # From this list I again concatenate (<>) the first and last element
      |> String.to_integer() # Convert to integer
      |> Kernel.+(acc) # And increment the reduce accumulator
    end)
  end
end
```

Now to adapt for the second part I would only need to have a custom converter to handle "one" as 1

```elixir
defmodule Day1 do
  @spec solution(String.t()) :: integer()
  def solution(file) do
    File.read!(file)
    |> String.trim_trailing()
    |> String.split("\n")
    |> Enum.reduce(0, fn calibration, acc ->
      # Now I handle more words
      ~r/(\d|one|two|three|four|five|six|seven|eight|nine)/
      |> Regex.scan(calibration)
      |> List.flatten()
      # And convert the number
      |> (&(convert(List.first(&1)) <> convert(List.last(&1)))).()
      |> String.to_integer()
      |> Kernel.+(acc)
    end)
  end

  # The base cases are "one" - "nine"
  @spec convert(String.t()) :: String.t()
  def convert("one"), do: "1"
  def convert("two"), do: "2"
  def convert("three"), do: "3"
  def convert("four"), do: "4"
  def convert("five"), do: "5"
  def convert("six"), do: "6"
  def convert("seven"), do: "7"
  def convert("eight"), do: "8"
  def convert("nine"), do: "9"

  # Else just return the same string
  def convert(number), do: number
end
```
