module Day5.RawData

[<Literal>]
//let sample = "../../../input.txt"
let sample = "../../../sample.txt"
let inputData = System.IO.File.ReadAllLines(sample) |> Array.toList
