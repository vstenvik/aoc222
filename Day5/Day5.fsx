open System
open System.IO
open System.Text.RegularExpressions

type State = char list list
type MoveCmd = {
    amount: int
    from: int
    dest: int
}

let readFile filename = 
    let input = (File.ReadAllText $"./Day5/{filename}.txt").Split("\r\n\r\n")
                |> Seq.map (fun s -> s.Split("\r\n") |> List.ofArray)
                |> List.ofSeq
    let stacks = input[0]
                |> List.rev
                |> List.skip 1
    let moves = input[1]
    stacks, moves
    
module Stack =
    let parseLine (input:string) =
        [1..4..input.Length]
        |> List.map (fun i ->
                match input[i] with
                | ' ' -> None
                | s -> Some s)
        
    let parse (input : string list) =
        input
        |> List.map parseLine
        |> List.transpose
        |> List.map (List.choose id)

module MoveCmd =
    let moveRegex = Regex("move (?<amount>\d+) from (?<from>\d+) to (?<dest>\d+)", RegexOptions.Compiled)
    let parseMove (input: string) =
        let m = moveRegex.Match input
        {
            amount = m.Groups["amount"].Value |> int;
            from = m.Groups["from"].Value |> int;
            dest = m.Groups["dest"].Value |> int;
        }
    let parse (input: string list) =
        input
        |> List.map parseMove
    

let updateAt n value list =
    list
    |> Seq.mapi (fun i e -> if i = n then value else e)

let move (stack: State) (move: MoveCmd) =
    let moving, rest = stack[move.from - 1]
                       |> List.rev
                       |> List.splitAt move.amount
                       |> (fun (a,b) -> List.rev a, List.rev b)
    stack
    |> updateAt (move.from - 1) rest
    |> updateAt (move.dest - 1) (stack[move.dest - 1] @ moving)

let moveMany (stack: State) (moves: MoveCmd list) =
    moves
    |> List.fold move stack

let stacksRaw, movesRaw = readFile "input"
let stack = Stack.parse stacksRaw
let moves = MoveCmd.parse movesRaw

// wrong: NFSLFRZZM

moveMany stack moves 
|> List.map List.last
|> (fun s -> String.Join("", s))