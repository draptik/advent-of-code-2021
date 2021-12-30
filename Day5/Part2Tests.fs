module Day5.Part2Tests

open Xunit
open Swensen.Unquote

open Day5.RawData
open Day5.Part1
open Day5.Part2
    
[<Fact>]
let ``check sample data`` () =
    let input = inputData
    let lines = rowsToLines input
    let overlaps =
        lines
        |> List.map (getOrientedLine' >> getCoverage')
        |> getOverlaps

    let actual = countOverlaps overlaps        

    let expectedNumberOfOverlaps = 12
    actual =! expectedNumberOfOverlaps
