---
author: "Daniel Boll"
title: "AoC - Day 1"
date: 2023-12-01T09:44:14-03:00
description: "Using Elixir & OCaml"
draft: false
---

Uh, hi?

```elixir
defmodule Day1 do
  @spec part_one(String.t()) :: integer()
  def part_one(file) do
    load_file(file)
    |> Enum.map(fn calibration ->
      numbers =
        calibration
        |> String.split("")
        |> Enum.filter(&Regex.match?(~r/\d/, &1))

      numbers
      |> Kernel.hd()
      |> Kernel.<>(List.last(numbers))
      |> String.to_integer()
    end)
    |> Enum.sum()
  end

  @spec load_file(String.t()) :: [String.t()]
  def load_file(file) do
    File.read!(file)
    |> String.trim_trailing()
    |> String.split("\n")
  end
end
```
