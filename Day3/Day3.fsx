open System.IO


let ex = "vJrwpWtwJgWrhcsFMMfFFhFp"

let split (str: string) = 
    let length = str.Length;
    str |> List.ofSeq |> List.splitAt (length/2)

type BothCompartments = char list * char list
let inBoth ((left, right): BothCompartments) =
    left
    |> List.filter (fun e -> right |> List.contains e)
    |> Set

let getItems = split >> inBoth

let (|Lowercase|Uppercase|) (c:char) =
    if (int c) >= (int 'a') && (int c) <= (int 'z') then
        Lowercase c
    else
        Uppercase c
    

let getPriority = function
   | Lowercase c -> (int c) - (int 'a') + 1
   | Uppercase c -> (int c) - (int 'A') + 27
    

let input = File.ReadAllLines "./Day3/input.txt"

input
|> Seq.map (getItems >> Seq.map getPriority >> Seq.sum)
|> Seq.sum