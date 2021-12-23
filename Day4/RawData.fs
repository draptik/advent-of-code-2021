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
    |> List.map (fun x -> x |> int)

type CellValue = int
type CellState = Marked | NotMarked
type Position = int * int
type Cell = {
    Position: Position
    State: CellState
    Value: CellValue
}
type BingoCard = Cell list
//type Board = BingoCard list


let draws = inputData.Head |> toDraws

let remaining = inputData.Tail

