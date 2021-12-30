module Day5.Part1Tests

open Xunit
open Swensen.Unquote

open Day5.RawData
open Day5.Part1

[<Fact>]
let ``check rowToLine`` () =
    let input = "0,9 -> 5,9"
    let actual = rowToLine input
    let expected = { Start = 0,9; End = 5,9 }
    actual =! expected

[<Fact>]
let ``check rowsToLines`` () =
    let input = inputData
    let actual = rowsToLines input
    let expectedLength = 10
    actual |> List.length =! expectedLength
    let expectedFirst = { Start = 0,9; End = 5,9 }
    actual.Head =! expectedFirst

[<Fact>]
let ``check getCoverage H`` () =
    let line = { Start = 1,0; End = 1,3 }
    let expected = Some [1,0; 1,1; 1,2; 1,3]
    let actual = line |> getOrientedLine |> getCoverage 
    actual =! expected

[<Fact>]
let ``check getCoverage V`` () =
    let line = { Start = 0,0; End = 3,0 }
    let expected = Some [0,0; 1,0; 2,0; 3,0]
    let actual = line |> getOrientedLine |> getCoverage 
    actual =! expected    

[<Fact>]
let ``check getCoverage when start is larger than end`` () =
    let line = { Start = 3,0; End = 0,0 }
    let expected = Some [0,0; 1,0; 2,0; 3,0]
    let actual = line |> getOrientedLine |> getCoverage 
    actual =! expected    

(*
.1.
121
.1.
*)
[<Fact>]
let ``a cross in a 3x3 matrix overlaps in the middle`` () =
    let line1 = { Start = 0,1; End = 2,1 }
    let line2 = { Start = 1,0; End = 1,2 }
    let coverage1 = line1 |> getOrientedLine |> getCoverage
    let coverage2 = line2 |> getOrientedLine |> getCoverage
    
    let actualOverlaps = getOverlaps [coverage1; coverage2]
    
    let expectedOverlaps = Overlaps [(1,1), 2]
    actualOverlaps =! expectedOverlaps
    
    let numberOfOverlaps = countOverlaps actualOverlaps
    numberOfOverlaps =! 1

[<Fact>]
let ``check sample data`` () =
    let input = inputData
    let lines = rowsToLines input
    let overlaps =
        lines
        |> List.map (getOrientedLine >> getCoverage)
        |> getOverlaps

    let actual = countOverlaps overlaps        

    let expectedNumberOfOverlaps = 5
    actual =! expectedNumberOfOverlaps

    