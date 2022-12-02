open System
open System.IO
type Move = Rock | Paper | Scissors
type Game = (Move * Move)

let parseMove = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | s -> raise (Exception($"{s} is not a valid input"))

let parseCounterMove = function
    | Rock, "X" -> Scissors
    | Rock, "Y" -> Rock
    | Rock, "Z" -> Paper
    | Paper, "X" -> Rock
    | Paper, "Y" -> Paper
    | Paper, "Z" -> Scissors
    | Scissors, "X" -> Paper
    | Scissors, "Y" -> Scissors
    | Scissors, "Z" -> Rock
    | (_, s) -> raise (Exception($"{s} is not a valid countermove"))
 
let parseRow (str: string) =
    let split = str.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    let move = parseMove split[0]
    (move, parseCounterMove (move, split[1]))

let getWinScore = function
    // Ties
    | Rock, Rock
    | Paper, Paper
    | Scissors, Scissors -> 3
    // Wins
    | Rock, Paper
    | Paper, Scissors
    | Scissors, Rock -> 6
    // Losses
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