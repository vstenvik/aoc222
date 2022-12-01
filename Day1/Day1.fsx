open System
open System.IO

let example = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""

let toInt (x:string) = Int32.Parse x
let input = File.ReadAllText "./Day1/input.txt"


input.Split("\r\n\r\n")
|> Array.map (fun str ->
    str.Split("\r\n")
    |> Array.map toInt
    |> Seq.sum)
|> Seq.max