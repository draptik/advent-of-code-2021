module Day4.RawData

open System

[<Literal>]
let sample = "../../../sample.txt"
let inputData = System.IO.File.ReadAllLines(sample) |> Array.toList

type Draw = int
type Draws = Draw list

let toDraws (s : string) : Draws =
    s.Split(',')
    |> List.ofArray
    |> List.map int

let draws = inputData.Head |> toDraws

let remaining = inputData.Tail
