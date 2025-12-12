open System.IO
open System

let ranges (input:string) = input.Split ","
let example = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/example.txt") 
    |> Array.head
    |> ranges
let input = 
    File.ReadAllLines (__SOURCE_DIRECTORY__ + "/input.txt") 
    |> Array.head 
    |> ranges

let generateIDRanges (range:string) =
    let startID, endID = 
        range.Split "-" 
        |> fun arr -> arr.[0] |> int, arr.[1] |> int
    [startID..endID]

example
|> Array.Parallel.map generateIDRanges