module Day7.Part2Tests

open Xunit
open Swensen.Unquote

open Day7.Part1
open Day7.Part2

let samples = [16;1;2;0;4;2;7;1;2;14]
let input = "../../../input.txt"
let inputData =
    System.IO.File.ReadAllLines(input)
    |> Array.toList
    |> List.head

let data = inputData.Split(',') |> List.ofArray |> List.map int
 
[<Theory>]
[<InlineData(16,5,66)>]
[<InlineData(1,5,10)>]
[<InlineData(2,5,6)>]
[<InlineData(0,5,15)>]
[<InlineData(4,5,1)>]
[<InlineData(2,5,6)>]
let ``check calcFuel part 2`` (p1, p2, expected) =
    let actual = calcFuel' p1 p2
    actual =! expected


[<Fact>]
let ``solve part 2`` () =
    let actual = samples |> List.sort |> getFuelUsageAtBestPosition'
    let expected = 168
    actual =! expected
    
