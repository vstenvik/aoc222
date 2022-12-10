open System
open System.IO
open System.Text.RegularExpressions

type FileEntry = { name: string; size: int; }

type Entry =
    | FileEntry of FileEntry
    | Folder of Folder
    
and Folder = {
    name: string
    size: int
    entries: Entry list
}

type Command =
    | Up
    | To of name: string
    | Ls

type InputLine =
    | Command of Command
    | Entry of Entry

let (|IsCommand|_|) value =
    let r = Regex("\$ (?<cmd>\S+)( (?<arg>\S+))?")
    let m = r.Match(value)
    if not m.Success then None
    else 
        let cmd = m.Groups["cmd"].Value
        if cmd = "ls" then Some Ls
        elif cmd = "cd" then
            match m.Groups["arg"].Value with
            | ".." -> Some Up
            | "" -> raise (ArgumentException("Invalid argument"))
            | name -> Some (To name)
        else None
    
let (|IsFolder|_|) value =
    let r = Regex("dir (?<name>\S+)")
    let m = r.Match(value)
    if not m.Success then None
    else Some (Entry.Folder {
                name = m.Groups["name"].Value
                size = 0
                entries = []})
    
let (|IsFile|_|) value =
    let r = Regex("(?<size>\d+) (?<name>\S+)")
    let m = r.Match(value)
    if not m.Success then None
    else Some (Entry.FileEntry {
                name = m.Groups["name"].Value
                size =  int m.Groups["size"].Value
                })

type State = {
    path: string list
    root: Folder
}
let getSize = function
   | FileEntry f -> f.size
   | Folder f -> f.size
   
let getName = function
   | FileEntry f -> f.name
   | Folder f -> f.name

let add (file: Entry) (folder: Folder) =
    { folder with
          size = folder.size + (getSize file)
          entries =  file::folder.entries }

let addToFolder (entry: Entry) (path: string list) (rootFolder: Folder) =
    let rec inner (innerPath: string list) (folder: Folder) =
        match innerPath with
        | [_] -> { folder with entries = entry :: folder.entries; size = folder.size + (getSize entry)}
        | p ->
                    let rest,drop = List.splitAt (p.Length - 1) p
                    { folder with entries = folder.entries
                                            |> List.map (fun e ->
                                                match e with
                                                | Folder f when f.name = (rest|>List.last) -> Folder (inner rest f)
                                                | x -> x)
                                            size = folder.size + (getSize entry)}
    inner path rootFolder

let navigate (state: State) = function
    | Up -> {state with path = state.path |> List.skip 1}
    | To name when name = "/" -> { state with path = ["/"] }
    | To name ->
        { state with path = name :: state.path }
    | Ls -> state
let parseCommand line: InputLine =
    match line with
    | IsCommand c -> InputLine.Command c
    | IsFolder f -> InputLine.Entry f
    | IsFile f -> InputLine.Entry f
    | l -> raise (ArgumentException($"Parse error '{l}'"))
    
let reducer (state: State) (line: InputLine) =
    match line with
    | Command c -> navigate state c
    | Entry e ->
        { state with root = (addToFolder e state.path state.root) }
let initialFolder = {
    name = "/"
    size = 0
    entries = []
}

// let testinitialFolder = {
//     name = "/"
//     size = 0
//     entries = [Folder {name = "a"; size = 0; entries = []}]
// }
//
// addToFolder (FileEntry {name="file.txt"; size = 22}) ["a"; "/"] testinitialFolder

let parse input =
    input
    |> List.ofSeq
    |> List.map parseCommand       
    |> List.fold reducer { path = ["/"]; root = initialFolder }
    
let flatten (folder: Folder) =
    let rec inner list f =
        match f with
        | FileEntry _ -> [f]
        | Folder f2 when f2.entries.Length = 0 -> [f]
        | Folder f3 ->
            let newList = f :: list
            newList @ List.collect (inner []) f3.entries
    inner [] (Entry.Folder folder)

let state = parse <| File.ReadAllLines "./Day7/input.txt"
let available = 70000000 - state.root.size
let needed = 30000000 - available

flatten state.root
|> List.filter (fun e ->
                match e with
                | Folder f -> f.size >= needed
                | FileEntry _ -> false)
|> List.sortBy (getSize)
|> List.head
