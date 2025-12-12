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
    ZerosPassed : int
    Log: string array
}

module Dial =
    let Create() = 
        {   Position = 50m
            ZerosPassed = 0 
            Log = [||] }

    let basicLog dial spinValue newPosition finalZero =
            printfn $"CurrentZeros: {dial.ZerosPassed} | StartingPosition {dial.Position} | SpinValue: {spinValue} | NewPosition: {newPosition} | FinalZero: {finalZero} | FinalCalc : {dial.ZerosPassed + finalZero}"  

    let private complexLog dial spinValue adjustAmount newPosition finalPosition adjustPassedZeros startingZeroAdjust finalZero =
        printfn $"CurrentZeros: {dial.ZerosPassed} | StartingPosition {dial.Position} | SpinValue: {spinValue} | AdjustAmount: {adjustAmount} | NewPosition: {newPosition} | FinalPosition: {finalPosition} | AdjustPassedZeros: {adjustPassedZeros} | StartingZeroAdjust: {startingZeroAdjust} | FinalZero: {finalZero} | FinalCalc : {dial.ZerosPassed + adjustPassedZeros + startingZeroAdjust + finalZero}"

    let newPositionAdjust dial spinValue =
        let startingZeroAdjust = if dial.Position = 0m then -1 else 0
        let newPosition = if dial.Position + spinValue = 100m then 0m else dial.Position + spinValue

        match newPosition > 100m, newPosition < 0m with
        | true, _ ->
            let adjustAmount = Math.Floor (newPosition / 100m) * 100m
            let finalPosition = if newPosition - adjustAmount = 100m then 0m else newPosition - adjustAmount
            let finalZero = if finalPosition = 0m then 1 else 0
            let adjustPassedZeros = int (adjustAmount / 100m)
            complexLog dial spinValue adjustAmount newPosition finalPosition adjustPassedZeros startingZeroAdjust finalZero
            { dial with Position = finalPosition; ZerosPassed = dial.ZerosPassed + adjustPassedZeros + startingZeroAdjust + finalZero }

        | _, true ->
            let adjustAmount = Math.Floor (newPosition / 100m) * -100m
            let finalPosition = if newPosition + adjustAmount = 100m then 0m else newPosition + adjustAmount
            let finalZero = if finalPosition = 0m then 1 else 0
            let adjustPassedZeros = int (adjustAmount / 100m)
            complexLog dial spinValue adjustAmount newPosition finalPosition adjustPassedZeros startingZeroAdjust finalZero
            { dial with Position = finalPosition; ZerosPassed = dial.ZerosPassed + adjustPassedZeros + startingZeroAdjust + finalZero }

        | _ ->
            let finalZero = if newPosition = 0m then 1 else 0
            basicLog dial spinValue newPosition finalZero
            { dial with Position = newPosition; ZerosPassed = dial.ZerosPassed + finalZero }

    let Spin dial operation   =
        let spin =
            match operation with
            | L value -> newPositionAdjust dial -value
            | R value -> newPositionAdjust dial value

        spin

let dial = Dial.Create()

// files  
// // |> Array.take 10
// |> Array.Parallel.map(fun line -> 
//     let direction = line.Substring(0,1)
//     let value = decimal (line.Substring 1)
//     Operation.Parse(direction, value)) 
// |> Array.fold Dial.Spin dial

module BruteFORCE =
    type  BruteDial = {
        Position: int
        Zeros: int
    }

    type BruteOperation =
    | L of int
    | R of int

    let BruteParse (direction: string, value: int) =
        // printfn "Parsing operation: %s%d" direction value
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
// |> Array.take 10
|> Array.Parallel.map(fun line -> 
    let direction = line.Substring(0,1)
    let value = decimal (line.Substring 1)
    BruteFORCE.BruteParse(direction, int value)) 
|> Array.fold BruteFORCE.Spin bruteDial
