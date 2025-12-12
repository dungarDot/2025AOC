open System.IO
open System

let example = File.ReadAllLines(__SOURCE_DIRECTORY__ + "/example.txt")
let data = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/input.txt")