module Types

type Direction =
        Up | Down | Left | Right

type Snake = (int*int) list

type Food = Food of (int*int)

type GameState = Snake*Direction*Food