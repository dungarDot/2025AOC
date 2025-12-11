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
}

module Dial =
    let Create() = 
        {   Position = 50m
            ZerosPassed = 0 }

    let newPositionAdjust dial spinValue =
        let startingZeroAdjust = if dial.Position = 0m then -1 else 0
        let newPosition = if dial.Position + spinValue = 100m then 0m else dial.Position + spinValue
        match newPosition > 100m, newPosition < 0m with
        | true, _ ->
            let adjustAmount = Math.Floor (newPosition / 100m) * 100m
            let finalPosition = if newPosition - adjustAmount = 100m then 0m else newPosition - adjustAmount
            let finalZero = if finalPosition = 0m then 1 else 0
            let adjustPassedZeros = int (adjustAmount / 100m)
            printfn $"CurrentZeros: {dial.ZerosPassed} | StartingPosition {dial.Position} | SpinValue: {spinValue} | AdjustAmount: {adjustAmount} | NewPosition: {newPosition} | FinalPosition: {finalPosition} | AdjustPassedZeros: {adjustPassedZeros} | StartingZeroAdjust: {startingZeroAdjust} | FinalZero: {finalZero} | FinalCalc : {dial.ZerosPassed + adjustPassedZeros + startingZeroAdjust + finalZero}"
            { dial with Position = finalPosition; ZerosPassed = dial.ZerosPassed + adjustPassedZeros + startingZeroAdjust + finalZero }
        | _, true ->
            let adjustAmount = Math.Floor (newPosition / 100m) * -100m
            let finalPosition = if newPosition + adjustAmount = 100m then 0m else newPosition + adjustAmount
            let finalZero = if finalPosition = 0m then 1 else 0
            let adjustPassedZeros = int (adjustAmount / 100m)
            printfn $"CurrentZeros: {dial.ZerosPassed} | StartingPosition {dial.Position} | SpinValue: {spinValue} | AdjustAmount: {adjustAmount} | NewPosition: {newPosition} | FinalPosition: {finalPosition} | AdjustPassedZeros: {adjustPassedZeros} | StartingZeroAdjust: {startingZeroAdjust} | FinalZero: {finalZero} | FinalCalc : {dial.ZerosPassed + adjustPassedZeros + startingZeroAdjust + finalZero}"
            { dial with Position = finalPosition; ZerosPassed = dial.ZerosPassed + adjustPassedZeros + startingZeroAdjust + finalZero }
            
        | _ ->
            let finalZero = if newPosition = 0m then 1 else 0
            printfn $"CurrentZeros: {dial.ZerosPassed} | StartingPosition {dial.Position} | SpinValue: {spinValue} | NewPosition: {newPosition} | FinalZero: {finalZero} | FinalCalc : {dial.ZerosPassed + finalZero}"
            { dial with Position = newPosition; ZerosPassed = dial.ZerosPassed + finalZero }

    let Spin dial operation   =
        let spin =
            match operation with
            | L value -> newPositionAdjust dial -value
            | R value -> newPositionAdjust dial value

        spin



let dial = Dial.Create()

files  
// |> Array.take 10
|> Array.Parallel.map(fun line -> 
    let direction = line.Substring(0,1)
    let value = decimal (line.Substring 1)
    Operation.Parse(direction, value)) 
|> Array.fold Dial.Spin dial