module Food

open Raylib_cs
open GlobalConsts
open ProjectUtils
open Types

let rec spawnNewFood snek =
    let foodX, foodY = Raylib.GetRandomValue(0, gridSize-1), Raylib.GetRandomValue(0, gridSize-1)

    snek
    |> List.contains (foodX, foodY)
    |> function
        |true -> spawnNewFood snek
        |false -> Food (foodX, foodY)