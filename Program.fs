#nowarn 3391

open Raylib_cs
open GlobalConsts
open Types

let testSnek = 
    gridSize >>> 1 
    |> fun x -> (x, x) 
    |> Snake.init

let (|Pos|) i 
    = i % gridSize * cellSize
    , cellSize * i / gridSize

let initWindow() = do
    Raylib.InitWindow(res, res, "no step on snek")
    Raylib.SetTargetFPS gameSpeed

let draw snek = do
    Raylib.BeginDrawing()
    Raylib.ClearBackground Color.Beige

    Snake.draw snek

    Raylib.EndDrawing()

let rec update (gamestate: GameState) =
    match (Raylib.WindowShouldClose():bool) with
    |true -> ()
    |false -> do

    draw gamestate
    
    gamestate 
    |> Snake.update
    |> update

[<EntryPoint>]
let main _ =
    do
        initWindow()
        (update << GameState.init)()
    0