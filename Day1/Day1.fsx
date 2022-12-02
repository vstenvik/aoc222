open System
open System.IO
let toInt (x:string) = Int32.Parse x

let input = File.ReadAllText "./Day1/input.txt"

input.Split("\r\n\r\n")
|> Array.map (fun str ->
    str.Split("\r\n")
    |> Array.map toInt
    |> Seq.sum)
|> Seq.sortDescending
|> Seq.take 3
|> Seq.sum