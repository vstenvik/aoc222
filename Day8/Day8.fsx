open System
open System.IO

type Grid = int array array
let toInt (x:char) = Int32.Parse (String [|x|])

module Grid = 
    let col n (grid: Grid) =
        [| for row in grid -> row[n] |]
    let row n (grid: Grid) = grid[n]

let isVisible (row, col) (grid: Grid) =
    let value = grid[row][col]
    let top, bottomRest = Grid.col col grid |> Array.splitAt row
    let left, rightRest = Grid.row row grid |> Array.splitAt col
    let bottom = bottomRest |> Array.skip 1
    let right = rightRest |> Array.skip 1
    [top; bottom; left; right]
    |> List.exists (Array.forall ((>) value))

let getViewDistance (value: int) (distance: int[]) =
    let d = distance
           |> Array.takeWhile ((>) value)
           |> Array.length
    if d = distance.Length then d else (d + 1)
    
let getViewScore (row, col) (grid: Grid) =
    let value = grid[row][col]
    let topPart, bottomRest = Grid.col col grid |> Array.splitAt row
    let leftPart, rightRest = Grid.row row grid |> Array.splitAt col
    let top = topPart |> Array.rev
    let left = leftPart |> Array.rev
    let bottom = bottomRest |> Array.skip 1
    let right = rightRest |> Array.skip 1
    [top; left; right; bottom]
    |> List.map (getViewDistance value)
    |> List.reduce (*)

let getOuterTrees (input: Grid) =
    input.Length * 2 + input[0].Length * 2 - 4

let solve_1 (input: Grid) =
    let inner = [for i = 1 to (input.Length - 2) do
                    for j = 1 to (input[0].Length - 2) do
                        if isVisible (i,j) input then yield (i,j)]
                |> List.length
    let outside = getOuterTrees input
    inner + outside
    
let solve_2 (input: Grid) =
    [for i = 1 to (input.Length - 2) do
        for j = 1 to (input[0].Length - 2) do
            yield getViewScore (i,j) input]
    |> List.max
    
let input = File.ReadAllLines "./Day8/input.txt"
            |> Array.map (fun s -> s |> Seq.map toInt |> Array.ofSeq)

solve_1 input
solve_2 input