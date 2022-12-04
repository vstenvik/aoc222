open System
open System.IO
let (|Lowercase|Uppercase|) (c:char) =
    if (int c) >= (int 'a') && (int c) <= (int 'z') then
        Lowercase c
    else
        Uppercase c
let getPriority = function
   | Lowercase c -> (int c) - (int 'a') + 1
   | Uppercase c -> (int c) - (int 'A') + 27
    
let groupsOf (n: int) (input: 't list) =
    let rec inner state input2 =
        let n2 = Math.Min(List.length input2, n)
        let group = input2 |> List.take n2
        let rest = input2 |> List.skip n2
        let newState = state @ [group]
        if rest.Length = 0 then
            newState
        else
            inner newState rest
            
    inner [] input


let reducer (a: Set<char>) (b: Set<char>) =
    a |> Set.filter b.Contains
    
let findBadge (group: string list) =
    group
    |> List.map (Set.ofSeq)
    |> List.reduce reducer
    |> Set.toList
    |> List.exactlyOne

let input = File.ReadAllLines "./Day3/input.txt" |> List.ofSeq

input
|> groupsOf 3
|> List.map (findBadge >> getPriority)
|> List.sum