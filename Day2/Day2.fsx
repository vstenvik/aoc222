open System
open System.IO


type Move = Rock | Paper | Scissors
type Game = (Move * Move)

let parseMove = function
          | "A" | "X" -> Rock
          | "B" | "Y" -> Paper
          | "C" | "Z" -> Scissors
          | s -> raise (Exception($"{s} is not a valid input"))
          
let parseRow (str: string) =
    let split = str.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    (parseMove split[0], parseMove split[1])
let getWinScore (game: Game) =
    match game with
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> 3
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> 6
    | _ -> 0
let getMoveScore = function
                   | Rock -> 1
                   | Paper -> 2
                   | Scissors -> 3

let getGameScore (game: Game) =
    let win = getWinScore game
    let move = game |> snd |> getMoveScore
    win + move

let input = File.ReadAllLines "./Day2/input.txt"

input
|> Seq.map (parseRow >> getGameScore)
|> Seq.sum