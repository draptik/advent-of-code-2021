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
let ``day 1 solution works`` () =
    let samples = 
        System.IO.File.ReadAllLines(input) 
        |> Array.toList 
        |> List.map int

    let expected = 1502
    let actual = countNumberOfIncreasingEntries samples
    Assert.Equal(expected, actual)

// List.groupBy
// List.indexed
// List.insertManyAt
// List.skip
// List.skipWhile
// List.take
// List.takeWhile
// List.transpose
// List.windowed <--

[<Fact>]
let ``try windowed function - works`` () =
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
    
    let w = samples |> List.windowed 3
    // debug
    w |> List.iter (fun window -> printfn "%A" window)
    
    let sums = w |> List.map (fun window -> window |> List.sum)
    // debug
    sums |> List.iter (fun sum -> printfn "%i" sum)

    let actual = countNumberOfIncreasingEntries sums
    let expected = 5
    Assert.Equal(expected, actual)

let getWindowedSums windowSize samples =
    let w = samples |> List.windowed windowSize
    // debug
    // w |> List.iter (fun window -> printfn "%A" window)
    
    let sums = w |> List.map (fun window -> window |> List.sum)
    // debug
    // sums |> List.iter (fun sum -> printfn "%i" sum)
    sums

[<Fact>]
let ``day 2: count windowed increasing sums`` () =
    let samples = 
        System.IO.File.ReadAllLines(input) 
        |> Array.toList 
        |> List.map int

    let actual = samples |> getWindowedSums 3 |> countNumberOfIncreasingEntries
    let expected = 1538
    Assert.Equal(expected, actual)