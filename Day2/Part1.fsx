open System.IO
open System

let filePath = __SOURCE_DIRECTORY__ + "/input.txt"
let files = File.ReadAllLines filePath