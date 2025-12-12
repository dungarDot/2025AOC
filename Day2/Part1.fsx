open System.IO
open System

let ranges (input:string) = input.Split ","
let example = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/example.txt") 
    |> Array.head
    |> ranges
let data = 
    File.ReadAllLines (__SOURCE_DIRECTORY__ + "/input.txt") 
    |> Array.head 
    |> ranges

