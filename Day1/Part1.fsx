// Correct answer.
open System.IO
open System

let filePath = __SOURCE_DIRECTORY__ + "/input.txt"
let files = File.ReadAllLines(filePath)

type Operation =
    | L of decimal
    | R of decimal
module Operation =
    let Parse (direction: string, value: decimal) =
        // printfn "Parsing operation: %s%d" direction value
        match direction with
        | "L" -> L value
        | "R" -> R value
        | _ -> failwith "Unexpected direction"

type Dial = {
    Position: decimal
    Adjust: decimal
    NumberOf0s : int
}

module Dial =
    let Create() = { Position = 50m; Adjust = 0m; NumberOf0s = 0 }

    let PositionAdjust dial  =
        match dial.Position >= 100m, dial.Position < 0m with
        | true, _ -> 
            let adjust = Math.Floor (dial.Position / 100m) * 100m
            printfn "Wrapping around from %M with %M to get %M" dial.Position adjust (dial.Position - adjust)
            { dial with Position = dial.Position - adjust }
        | _, true -> 
            let adjust = Math.Floor (dial.Position / 100m) * -100m
            printfn "Wrapping around from %M with %M to get %M" dial.Position adjust (dial.Position + adjust)
            { dial with Position = dial.Position + adjust }
        | _ -> 
            printfn "No wrap needed for %M" dial.Position
            dial

    let ZeroCheck dial = 
        if dial.Position = 0m then 
            printfn "Hit zero, count at %d" (dial.NumberOf0s + 1)
            { dial with NumberOf0s = dial.NumberOf0s + 1 }
        else
            dial

    let DoTheThing operation dial =
        match operation with
        | L value -> 
            printfn "Doing L %M on %M" value dial.Position
            { dial with Position = dial.Position - value; Adjust = -value }
            |> PositionAdjust
            |> ZeroCheck
        | R value -> 
            printfn "Doing R %M on %M" value dial.Position
            { dial with Position = dial.Position + value; Adjust = value }
            |> PositionAdjust
            |> ZeroCheck

let dial = Dial.Create()

files  
// |> Array.take 10
|> Array.Parallel.map(fun line -> 
    let direction = line.Substring(0,1)
    let value = decimal (line.Substring 1)
    Operation.Parse(direction, value)) 
|> Array.fold (fun dial operation -> Dial.DoTheThing operation dial) dial