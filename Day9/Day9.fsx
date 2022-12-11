open System
open System.IO
open System.Text.RegularExpressions

type Move =
    | Up of count: int
    | Left of count: int
    | Right of count: int
    | Down of count: int


module Move =
    let regex = Regex("(?<dir>\S) (?<count>\d+)")
    let parse (str: string) =
        let m = regex.Match(str)
        if m.Success then
            let count = int <| m.Groups["count"].Value
            match m.Groups["dir"].Value with
            | "U" -> Up count
            | "R" -> Right count
            | "L" -> Left count
            | "D" -> Down count
            | s -> failwith $"{s} is not a valid direction"
        else
            failwith $"'{str}' is invalid"
    let expand = function
        | Up c -> List.replicate c (Up 1)
        | Left c -> List.replicate c (Left 1)
        | Right c -> List.replicate c (Right 1)
        | Down c -> List.replicate c (Down 1)

type Position = int * int

module Position =
    let apply (x, y) move =
        match move with
        | Up c -> (x, y + c)
        | Down c -> (x, y - c)
        | Left c -> (x - c, y)
        | Right c -> (x + c, y)
    let isTooFar (x1: int, y1: int) (x2,y2) =
        (Math.Abs(x2 - x1) >= 2) || (Math.Abs(y2-y1) >= 2)

type MoveState = {
    visited: Set<Position>
    head: Position
    tail: Position
}

module MoveState =
    let apply state move =
        let newHead = Position.apply state.head move
        let newTail = if Position.isTooFar state.tail newHead
                      then state.head
                      else state.tail
        { state with
            head = newHead
            tail = newTail
            visited = Set.add newTail state.visited }
        
let initialState = {
    head = 0,0
    tail = 0,0
    visited = set [(0,0)]
}

let example = File.ReadAllLines "./Day9/input.txt"
              |> Seq.map Move.parse
              |> Seq.collect Move.expand
              |> Seq.fold MoveState.apply initialState

example.visited.Count