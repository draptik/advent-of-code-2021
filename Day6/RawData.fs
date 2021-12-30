module Day6.RawData

[<Literal>]
//let sample = "../../../input.txt"
let sample = "../../../sample.txt"
let inputData = System.IO.File.ReadAllLines(sample) |> Array.toList

let rawInitialInternalTimers = inputData.Head.Split(',') |> List.ofArray |> List.map int

