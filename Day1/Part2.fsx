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
    AdjustZerosPassed: int
    NumberOf0s : int
}

module Dial =
    let Create() = 
        {   Position = 50m
            Adjust = 0m
            AdjustZerosPassed = 0
            NumberOf0s = 0 }
    // Process:
    // 1. Move dial according to operation
    // 2. if dial = 100 then set to 0
    // 3. Check how many 0's were passed during the move
    // 4. Update total 0's hit
    let PositionAdjust dial  =
        printfn "Adjusting position for %M" dial.Position
        match dial.Position >= 100m, dial.Position <= 0m with
        | true, _ -> 
            let position = if dial.Position = 100m then 0m else dial.Position
            let zeroesPassed = Math.Floor (position / 100m)
            let adjust = zeroesPassed * 100m
            let finalPosition = if position - adjust = 100m then 0m else position - adjust
            printfn "Wrapping around from %M with %M to get %M with an additional %d" dial.Position adjust finalPosition (int zeroesPassed)
            { dial with Position = finalPosition; AdjustZerosPassed = int zeroesPassed }
        | _, true -> 
            let zeroStart = if dial.Position = 0m then -1m else 0m
            let zeroesPassed = Math.Floor (dial.Position / 100m) * -1m + zeroStart
            let adjust = zeroesPassed * 100m
            let finalPosition = if dial.Position + adjust = 100m then 0m else dial.Position + adjust
            printfn "Wrapping around from %M with %M to get %M with an additional %d" dial.Position adjust finalPosition (int zeroesPassed)
            { dial with Position = finalPosition; AdjustZerosPassed = int zeroesPassed }
        | _ -> 
            printfn "No wrap needed for %M" dial.Position
            {dial with AdjustZerosPassed = 0 }

    let ZeroCheck dial = 
        if dial.Position = 0m then 
            printfn "Zero landed on, count at %d" (dial.NumberOf0s + 1)
            { dial with NumberOf0s = dial.NumberOf0s + 1 + dial.AdjustZerosPassed; AdjustZerosPassed = 0 }
            
        else
            printfn "No zero landed on. Passed %d zeros for a total of %d" dial.AdjustZerosPassed (dial.NumberOf0s + dial.AdjustZerosPassed)
            { dial with NumberOf0s = dial.NumberOf0s + dial.AdjustZerosPassed; AdjustZerosPassed = 0 }

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