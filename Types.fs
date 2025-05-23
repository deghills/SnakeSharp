module Types

type Direction =
        Up | Down | Left | Right

type Snake = (int*int) list

type GameState = {   
    RememberedDirection : Direction
    Snek : Snake }