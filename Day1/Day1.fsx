
open System
open System.Collections.Generic
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

let toInt (x:string) = match Int32.TryParse x with
                        | true, i -> Some i
                        | _ -> None
let input = File.ReadAllLines "./Day1/input.txt"


let list = List<List<string>>()
list.Add(List())

input
|> Seq.iter (fun inp ->
             if inp = "" then list.Add(List())
             else )
