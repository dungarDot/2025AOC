// Correct answer.
open System.IO
open System

let filePath = __SOURCE_DIRECTORY__ + "/input.txt"
let files = File.ReadAllLines filePath
module BruteFORCE =
    type  BruteDial = {
        Position: int
        Zeros: int
    }

    type BruteOperation =
    | L of int
    | R of int

    let BruteParse (direction: string, value: int) =
        match direction with
        | "L" -> L value
        | "R" -> R value
        | _ -> failwith "Unexpected direction"

    let Create() = 
        {   Position = 50
            Zeros = 0 }

    let turnLeft dial = 
        let newPosition = dial.Position - 1
        match newPosition with 
        | -1 -> { dial with Position = 99 }
        | 0 -> { dial with Position = newPosition; Zeros = dial.Zeros + 1 }
        | _ -> { dial with Position = newPosition }

    let turnRight dial = 
        let newPosition = dial.Position + 1
        match newPosition with 
        | 100 -> { dial with Position = 0; Zeros = dial.Zeros + 1  }
        | _ -> { dial with Position = newPosition }

    let Spin dial operation   =
        match operation with
        | L value -> 
            [1 .. value]
            |> List.fold (fun acc _ -> turnLeft acc) dial
        | R value -> 
            [1 .. value]
            |> List.fold (fun acc _ -> turnRight acc) dial

let bruteDial = BruteFORCE.Create()

files  
|> Array.Parallel.map(fun line -> 
    let direction = line.Substring(0,1)
    let value = decimal (line.Substring 1)
    BruteFORCE.BruteParse(direction, int value)) 
|> Array.fold BruteFORCE.Spin bruteDial
