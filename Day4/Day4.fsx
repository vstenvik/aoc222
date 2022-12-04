open System
open System.IO

let toInt (x:string) = Int32.Parse x

type ElfPairs = (int * int) * (int * int)

let parseRange (str:string) =
    let [|start;last|] = str.Split("-")
    toInt start, toInt last
    
let parseLine (input:string) =
    let [|first;second|] = input.Split(",")
    parseRange first, parseRange second
    
let pairsFullyOverlap (((x1, x2), (y1,y2)):ElfPairs) =
    let setA = set {x1..x2}
    let setB = set {y1..y2}
    Set.isSubset setA setB || Set.isSubset setB setA

let pairsOverlap (((x1, x2), (y1,y2)):ElfPairs) =
    let setA = set {x1..x2}
    let setB = set {y1..y2}
    let overlap = Set.intersect setA setB |> Set.count
    overlap > 0

let solve_1 (input: string list) =
    input
    |> Seq.map parseLine
    |> Seq.filter pairsFullyOverlap
    |> Seq.length
let solve_2 (input: string list) =
    input
    |> Seq.map parseLine
    |> Seq.filter pairsOverlap
    |> Seq.length
    
let input = File.ReadAllLines "./Day4/input.txt" |> List.ofSeq

solve_1 input
solve_2 input