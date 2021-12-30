module Day5.Part1

open System
open System.Text.RegularExpressions

open Day5.RawData

type XCoordinate = int
type YCoordinate = int
type Coordinate = XCoordinate * YCoordinate
type StartCoordinate = Coordinate
type EndCoordinate = Coordinate
type Coverage = Coordinate list

type LineDefinition = {
    Start: StartCoordinate
    End: EndCoordinate
}

type OrientedLine =
    | Horizontal of LineDefinition
    | Vertical of LineDefinition
    | Diagonal of LineDefinition
    | Other

type NumberOfOverlaps = int
type Overlap =
    | NoOverlap
    | Overlaps of (Coordinate * NumberOfOverlaps) list
    
let rowToLine row : LineDefinition =
    let pattern = "^(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)$"
    let matched = Regex.Match(row, pattern)
    
    {
        Start = int matched.Groups.["x1"].Value, int matched.Groups.["y1"].Value
        End = int matched.Groups.["x2"].Value, int matched.Groups.["y2"].Value
    }
    
let rowsToLines rows = rows |> List.map rowToLine

let getOrientedLine line =
    let x1, y1 = line.Start
    let x2, y2 = line.End
    match (x1, y1, x2, y2) with
    | x1, y1, x2, y2 when x1 = x2 && y1 = y2 -> Other 
    | x1, _, x2, _ when x1 = x2 -> Horizontal line
    | _, y1, _, y2 when y1 = y2 -> Vertical line
    | _ -> Other

let getCoverage line : Coverage option =
    let sort a b = if a < b then [a..b] else [b..a]
    match line with
    | Horizontal l ->
        let x1, y1 = l.Start 
        let _, y2 = l.End
        let result =
            sort y1 y2 
            |> List.map (fun y -> x1,y)
        Some result    
    | Vertical l ->
        let x1, y1 = l.Start 
        let x2, _ = l.End
        let result =
            sort x1 x2 
            |> List.map (fun x -> x,y1)
        Some result    
    | _ -> None

let getOverlaps (coverages: Coverage option list) : Overlap =
    let overlaps =
        coverages
        |> List.choose id
        |> List.collect id
        |> List.groupBy id
        |> List.map (fun pair ->
            let coord = fst pair
            let count = snd pair |> List.length
            if count > 1 then
                Some (coord, count)
            else
                None
            )
        |> List.choose id
    
    match overlaps with
    | [] -> NoOverlap
    | xs -> Overlaps xs

let countOverlaps overlaps =
    match overlaps with
    | NoOverlap -> 0
    | Overlaps xs -> xs.Length