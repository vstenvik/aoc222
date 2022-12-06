open System.IO

let findMarkerIndex (input:string) =
    let rec next (str: char list) (i: int) =
        let unique = str |> Seq.take 4
        if unique |> set |> Set.count |> (=) 4 then i
        else next (str |> List.skip 1) (i + 1)
    next (List.ofSeq input) 4

let findMessageIndex (input:string) =
    let rec next (str: char list) (i: int) =
        let unique = str |> Seq.truncate 14
        if unique |> Seq.length |> (<) 14 then None
        elif unique |> set |> Set.count |> (=) 14 then Some i
        else next (str |> List.skip 1) (i + 1)
    next (List.ofSeq input) 14
let input = File.ReadAllText "./Day6/input.txt"

findMarkerIndex input
findMessageIndex input