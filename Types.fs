module Types

open Raylib_cs
open GlobalConsts
open ProjectUtils

type Direction =
    Up | Down | Left | Right
    member this.toVector =
        match this with
        |Up     ->  0, -1
        |Down   ->  0,  1
        |Right  ->  1,  0
        |Left   -> -1,  0

    static member getUserDir lastDir =
        match Raylib.GetKeyPressed(), lastDir with
        |W, Down    -> Down
        |A, Right   -> Right
        |S, Up      -> Up
        |D, Left    -> Left
        |W, _       -> Up
        |A, _       -> Left
        |S, _       -> Down
        |D, _       -> Right
        |_ -> lastDir

type Snake = Snake of (int*int) list

type Food = Food of (int*int) with
    static member spawnNewFood =
        let rec aux (Snake snek) =
            let gridSizeDecr = gridSize - 1
            let food 
                = Raylib.GetRandomValue(0, gridSizeDecr)
                , Raylib.GetRandomValue(0, gridSizeDecr)

            snek
            |> List.contains food
            |> function true -> aux (Snake snek) | false -> Food food

        aux

type Score = Score of int

type GameState = 
    | ActiveGame of Snake*Direction*Food
    | GameOver of Score with

    static member init() =
        let centre = gridSize >>> 1
        let snek = Snake [centre, centre]
        ActiveGame
            ( snek
            , Left
            , Food.spawnNewFood snek )