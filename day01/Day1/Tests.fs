module Tests

open System
open Xunit

[<Literal>]
let input = "../../../../input.txt"

[<Fact>]
let ``input has 2000 lines`` () =
    let lines = System.IO.File.ReadAllLines(input)
    Assert.Equal(2000, lines.Length)

let countNumberOfIncreasingEntries samples =
    samples
    |> List.fold(
            fun (acc, prev) elem -> 
                if elem > prev then
                    // printfn "INCREASING: %s / %s" prev elem
                    (acc + 1, elem)
                else
                    // printfn "NOT INCREASING: %s / %s" prev elem
                    (acc, elem)
        ) 
        (0, samples.[0])
    |> fst

[<Fact>]
let ``detect if next is larger than previous`` () =
    let samples = 
        [
            199
            200
            208
            210
            200
            207
            240
            269
            260
            263            
        ]
    let expected = 7
    let actual = countNumberOfIncreasingEntries samples
    Assert.Equal(expected, actual)


[<Fact>]
let ``count increasing lines in input`` () =
    let samples = 
        System.IO.File.ReadAllLines(input) 
        |> Array.toList 
        |> List.map int

    let expected = 1502
    let actual = countNumberOfIncreasingEntries samples
    Assert.Equal(expected, actual)
