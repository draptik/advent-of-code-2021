module Tests

open System
open Xunit

[<Literal>]
let input = "../../../input.txt"
let inputData = System.IO.File.ReadAllLines(input) |> Array.toList

[<Fact>]
let ``Experiments - strings to matrix`` () =
    let demoInput = [
        "00100"
        "11110"
        "10110"
        "10111"
        "10101"
        "01111"
        "00111"
        "11100"
        "10000"
        "11001"
        "00010"
        "01010"
    ]
    
    let width = demoInput.[0].Length // assume that all entries have the same length
    let height = demoInput.Length
    // printfn "width: %i, height: %i" width height

    let singleRawInputRow = "00100"
    let toBoolRow (s: string) : bool list = s |> Seq.toList |> List.map (fun c -> c = '1')
    let singleBoolRow = singleRawInputRow |> toBoolRow
    // printfn "singleBoolRow: %A" singleBoolRow

    let toBoolRows rawInputs = rawInputs |> List.map toBoolRow
    let boolRows = toBoolRows demoInput
    // printfn "boolRows: %A" boolRows
    
    let matrix = Array2D.init height width (fun i j -> boolRows.[i].[j])
    // printfn "Matrix: %A" matrix

    let firstColumn = matrix.[*,0]
    // printfn "firstColumn: %A" firstColumn
    
    let numberOfTrueValuesInFirstColumn = firstColumn |> Array.filter (fun b -> b) |> Array.length
    let numberOfFalseValuesInFirstColumn = (firstColumn |> Array.length) - numberOfTrueValuesInFirstColumn
    // printfn "true: %i, false: %i" numberOfTrueValuesInFirstColumn numberOfFalseValuesInFirstColumn

    Assert.True(true)

[<Fact>]
let ``Experiments - strings to matrix 2`` () =
    let demoInput = [
        "00100"
        "11110"
        "10110"
        "10111"
        "10101"
        "01111"
        "00111"
        "11100"
        "10000"
        "11001"
        "00010"
        "01010"
    ]
    
    let width = demoInput.[0].Length // assume that all entries have the same length
    let height = demoInput.Length

    let toBoolRow (s: string) : bool list = s |> Seq.toList |> List.map (fun c -> c = '1')
    let toBoolRows rawInputs = rawInputs |> List.map toBoolRow
    let boolRows = toBoolRows demoInput
    let matrix = Array2D.init height width (fun i j -> boolRows.[i].[j])

    let getMostCommonValueForColumn (m:bool[,]) (columnIndex:int) : bool =
        let column = m.[*,columnIndex]
        // printfn "column: %A" column
        
        let numberOfTrueValuesInColumn = column |> Array.filter (fun b -> b) |> Array.length
        let numberOfFalseValuesInColumn = (column |> Array.length) - numberOfTrueValuesInColumn
        // printfn "true: %i, false: %i" numberOfTrueValuesInColumn numberOfFalseValuesInColumn
        
        // not sure what happens when true and false are equal: currently "true wins"
        let mostCommonValue = numberOfTrueValuesInColumn >= numberOfFalseValuesInColumn
        mostCommonValue

    let toDecimal (bs:bool list) =
        let s =
            bs 
            |> List.map (fun b -> if b then "1" else "0")
            |> String.concat ""
        Convert.ToInt32(s, 2)
    // printfn "toDecimal: %A" (toDecimal [true; false; true; true; false]) // 22

    let getGammaRate =
        [0..(width - 1)]
        |> List.map (fun columnIndex -> getMostCommonValueForColumn matrix columnIndex)
        |> toDecimal
    // printfn "getGammaRate: %A" getGammaRate

    let getEpsilonRate gammaRate =
        let rec intToBinary i =
            match i with
            | 0 | 1 -> string i
            | _ ->
                let bit = string (i % 2)
                (intToBinary (i / 2)) + bit
        let binaryString = intToBinary gammaRate
        
        let binInverted = 
            binaryString
                .Replace("0", "*")
                .Replace("1", "0")
                .Replace("*", "1")

        // printfn "binInverted: %A" binInverted
        Convert.ToInt32(binInverted, 2)

    // printfn "epsilon: %A" (getEpsilonRate getGammaRate)

    Assert.True(true)

[<Fact>]
let ``day 3 - part 1 should work`` () =
    let width = inputData.[0].Length // assume that all entries have the same length
    let height = inputData.Length

    let toBoolRow (s: string) : bool list = s |> Seq.toList |> List.map (fun c -> c = '1')
    let toBoolRows rawInputs = rawInputs |> List.map toBoolRow
    let boolRows = toBoolRows inputData
    let matrix = Array2D.init height width (fun i j -> boolRows.[i].[j])

    let getMostCommonValueForColumn (m:bool[,]) (columnIndex:int) : bool =
        let column = m.[*,columnIndex]
        // printfn "column: %A" column
        
        let numberOfTrueValuesInColumn = column |> Array.filter (fun b -> b) |> Array.length
        let numberOfFalseValuesInColumn = (column |> Array.length) - numberOfTrueValuesInColumn
        // printfn "true: %i, false: %i" numberOfTrueValuesInColumn numberOfFalseValuesInColumn
        
        // not sure what happens when true and false are equal: currently "true wins"
        let mostCommonValue = numberOfTrueValuesInColumn >= numberOfFalseValuesInColumn
        mostCommonValue

    let toDecimal (bs:bool list) =
        let s =
            bs 
            |> List.map (fun b -> if b then "1" else "0")
            |> String.concat ""
        Convert.ToInt32(s, 2)

    let getGammaRate =
        [0..(width - 1)]
        |> List.map (fun columnIndex -> getMostCommonValueForColumn matrix columnIndex)
        |> toDecimal
    // printfn "getGammaRate: %A" getGammaRate

    let getEpsilonRate gammaRate =
        let rec intToBinary i =
            match i with
            | 0 | 1 -> string i
            | _ ->
                let bit = string (i % 2)
                (intToBinary (i / 2)) + bit
        let binaryString = intToBinary gammaRate
        
        let binInverted = 
            binaryString
                .Replace("0", "*")
                .Replace("1", "0")
                .Replace("*", "1")

        // printfn "binInverted: %A" binInverted
        Convert.ToInt32(binInverted, 2)

    let gammaRate = getGammaRate
    let epsilonRate = getEpsilonRate gammaRate
    let result = gammaRate * epsilonRate
    // printfn "gammaRate: %A" gammaRate
    // printfn "epsilonRate: %A" epsilonRate
    // printfn "result: %A" result

    let expectedGammaRate = 2663
    Assert.Equal(expectedGammaRate, gammaRate)

    let expectedEpsilonRate = 1432
    Assert.Equal(expectedEpsilonRate, epsilonRate)

    let expectedResult = 3813416
    Assert.Equal(expectedResult, result)

(* Part 1 refactored ============================================================================ *)

let toBoolRow (s: string) : bool list = s |> Seq.toList |> List.map (fun c -> c = '1')

let toBoolRows (rawInputs:string list) : bool list list = rawInputs |> List.map toBoolRow

let initMatrix (height:int) (width:int) (boolRows:bool list list) = Array2D.init height width (fun i j -> boolRows.[i].[j])

let toDecimal (bs:bool list) =
    let s =
        bs 
        |> List.map (fun b -> if b then "1" else "0")
        |> String.concat ""
    Convert.ToInt32(s, 2)

let getMostCommonValueForColumn (m:bool[,]) (columnIndex:int) : bool =
    let column = m.[*,columnIndex]
    let numberOfTrueValuesInColumn = column |> Array.filter (fun b -> b) |> Array.length
    let numberOfFalseValuesInColumn = (column |> Array.length) - numberOfTrueValuesInColumn
    let mostCommonValue = numberOfTrueValuesInColumn >= numberOfFalseValuesInColumn
    mostCommonValue

let getGammaRate (matrix:bool[,]) =
    let width = matrix |> Array2D.length2
    [0..(width - 1)]
    |> List.map (fun columnIndex -> getMostCommonValueForColumn matrix columnIndex)
    |> toDecimal

let rec intToBinary i =
    match i with
    | 0 | 1 -> string i
    | _ ->
        let bit = string (i % 2)
        (intToBinary (i / 2)) + bit

let invertBinaryString (binaryString:string) =
    binaryString
        .Replace("0", "*")
        .Replace("1", "0")
        .Replace("*", "1")

(* we know that epsilon is the binary inversion of gamma *)
let getEpsilonRate gammaRate =
    let invertedBinaryString = gammaRate |> intToBinary |> invertBinaryString
    Convert.ToInt32(invertedBinaryString, 2)

[<Fact>]
let ``day 3 - part 1 refactored`` () =
    let width = inputData.[0].Length // assume that all entries have the same length
    let height = inputData.Length

    let boolRows = toBoolRows inputData
    let matrix = initMatrix height width boolRows

    let gammaRate = getGammaRate matrix
    let expectedGammaRate = 2663
    Assert.Equal(expectedGammaRate, gammaRate)
    
    let epsilonRate = getEpsilonRate gammaRate
    let expectedEpsilonRate = 1432
    Assert.Equal(expectedEpsilonRate, epsilonRate)

    let result = gammaRate * epsilonRate
    let expectedResult = 3813416
    Assert.Equal(expectedResult, result)
    Assert.True(true)
        