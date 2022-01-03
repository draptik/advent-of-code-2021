module Day7.Part1Tests

open System
open Xunit
open Swensen.Unquote

open Day7.Part1

let samples = [16;1;2;0;4;2;7;1;2;14]

let input = "../../../input.txt"
let inputData =
    System.IO.File.ReadAllLines(input)
    |> Array.toList
    |> List.head

let data = inputData.Split(',') |> List.ofArray |> List.map int

[<Fact>]
let ``cheapest target position`` () =
    let actual = samples |> List.sort |> getPositionUsingLeastFuel  //samples
    let expected = 2
    actual =! expected

[<Fact>]
let ``solve part 1`` () =
    let actual = data |> List.sort |> getFuelUsageAtBestPosition
    let expected = 37
    actual =! expected
    
[<Fact>]
let ``check getTargetPositions`` () =
    let actual = samples |> getTargetPositions
    let expected = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16] 
    actual =! expected

[<Fact>]
let ``check getTargetPositions min and max`` () =
    let actual = data |> getTargetPositions
    let min = actual |> List.min
    let max = actual |> List.max
    min =! 0
    max =! 1911
    
[<Theory>]
[<InlineData(16,2,14)>]
[<InlineData(1,2,1)>]
[<InlineData(2,2,0)>]
[<InlineData(0,2,2)>]
let ``check calcFuel`` (p1, p2, expected) =
    let actual = calcFuel p1 p2
    actual =! expected

[<Fact>]
let ``check getTargetPositionWithLeastFuelUsage`` () =
    let input = [1,10; 2,1; 3,5; 4,2; 5,0]
    let actual = getTargetPositionWithLeastFuelUsage input
    let expected = 5
    actual =! expected
    
[<Fact>]
let ``check getTargetPositionsAndFuelSummaries`` () =
    let targetPositions = [1;2;3]
    let crabPositions = [1;2;3;3;3]
    let actual = getTargetPositionsAndFuelSummaries targetPositions crabPositions
    let expected = [(1, 7); (2, 4); (3, 3)]
    actual =! expected
    
[<Fact>]
let ``check getTargetPositionsAndFuelSummaries 2`` () =
    let crabPositions = samples
    let targetPositions = crabPositions |> getTargetPositions
    let actual = getTargetPositionsAndFuelSummaries targetPositions crabPositions
    let expected = [(0, 49); (1, 41); (2, 37); (3, 39); (4, 41); (5, 45); (6, 49); (7, 53); (8, 59);
         (9, 65); (10, 71); (11, 77); (12, 83); (13, 89); (14, 95); (15, 103); (16, 111)]
    actual =! expected

[<Fact>]
let ``check calcFuelForCrabsAtPosition`` () =
    let targetPosition = 1
    let crabPositions = [0;1;2;2;1]
    let actual = calcFuelForCrabsAtPosition crabPositions targetPosition
    let expected = 1,3;
    actual =! expected