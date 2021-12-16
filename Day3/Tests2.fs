module Tests2

open System
open Xunit
open Swensen.Unquote

[<Literal>]
let input = "../../../input.txt"
let inputData = System.IO.File.ReadAllLines(input) |> Array.toList

(* Types ============================================================================================================ *)
type Bit = Zero | One
type Bits = Bit seq
type RowBits = Bits
type ColumnBits = Bits
type GammaRate = Bits
type EpsilonRate = Bits

let toBit (c:char) : Bit =
    if c = '1' then One
    else Zero
let toBits (s:string) : Bits =
    s |> Seq.map toBit
    
let toDecimal (bits: Bits) : int =
    let toString bit =
        match bit with
        | Zero -> "0"
        | One -> "1"
    
    let toDec (s:string) = Convert.ToInt32(s, 2)

    bits
    |> Seq.map toString
    |> String.concat ""
    |> toDec

(* Functions ======================================================================================================== *)
let getBitAt bits index =
    bits |> Seq.item index

let toBitsColumn (bits: Bits seq) (index: int) : ColumnBits =
    bits |> Seq.map (fun bs -> getBitAt bs index)

let getBitsWidth bss =
    bss |> Seq.head |> Seq.length

let getMostCommonValue (bits: ColumnBits) : Bit =
    let numberOfOnes = bits |> Seq.filter (fun b -> b = One) |> Seq.length
    let numberOfZeros = (bits |> Seq.length) - numberOfOnes
    if numberOfOnes >= numberOfZeros then One
    else Zero

let getGammaRate (bss: Bits seq) : GammaRate =
    [0..(getBitsWidth bss)-1]
    |> List.map (toBitsColumn bss)
    |> List.map getMostCommonValue
    |> Seq.ofList

let invertBit b =
    match b with
        | One -> Zero
        | Zero -> One
        
let invertBits bs =
    bs |> Seq.map invertBit

let getEpsilonRate gammaRate : EpsilonRate =
    gammaRate |> invertBits

let calcResult gammaRate epsilonRate =
    (gammaRate |> toDecimal) * (epsilonRate |> toDecimal)
    
[<Fact>]
let ``day3 - part 1 with types`` () =
    let allData = inputData |> List.map toBits
    
    let gammaRate = allData |> getGammaRate 
    let expectedGammaRateDecimal = 2663
    (gammaRate |> toDecimal) =! expectedGammaRateDecimal
    
    let epsilonRate = gammaRate |> getEpsilonRate
    let expectedEpsilonRate = 1432
    (epsilonRate |> toDecimal) =! expectedEpsilonRate
    
    let result = calcResult gammaRate epsilonRate
    let expectedResult = 3813416
    result =! expectedResult

let rawDemo = [
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

type OxygenGeneratorRating = Bits
type C02ScrubberRating = Bits

let getLeastCommonValue (bits: ColumnBits) : Bit =
    bits |> getMostCommonValue |> invertBit

type Index = int

let extractBitFromIndexedBit (_, bit) = bit
let extractIndexFromIndexedBit (index, _) = index
let isBit bitToFilterBy indexedBit = (indexedBit |> extractBitFromIndexedBit) = bitToFilterBy
let getIndexedBitsByFilterBit bitToFilterBy indexedBits = indexedBits |> Seq.indexed |> Seq.filter (isBit bitToFilterBy)
let containsIndices indicesToFilterBy indexedBit = (indexedBit |> extractIndexFromIndexedBit) = indicesToFilterBy
let getIndexedBitsByIndices indices indexedBits = indexedBits |> Seq.indexed |> Seq.filter (containsIndices indices)

let getMostCommonValueWithIndices (bits: ColumnBits) : Bit * Index seq =
    let onesWithIndices = bits |> getIndexedBitsByFilterBit One
    let zerosWithIndices = bits |> getIndexedBitsByFilterBit Zero
    if (Seq.length onesWithIndices) >= (Seq.length zerosWithIndices) then
        One, (onesWithIndices |> Seq.map extractIndexFromIndexedBit)
    else
        Zero, (zerosWithIndices |> Seq.map extractIndexFromIndexedBit)

//let getBitsByIndex (bits: Bit seq) (indices: Index seq) : Bit seq =
//    bits
    
    
let getOxygenGeneratorRating bss : OxygenGeneratorRating =
    let columns = [0..(getBitsWidth bss)-1] |> List.map (toBitsColumn bss)

    let getIndicesByFunc f (column:ColumnBits) : Index seq =
        column |> Seq.indexed |> Seq.filter (fun (i,x) -> f x) |> Seq.map fst
    
    let getRemainingColumn (indices:Index seq) (column: ColumnBits) : Bits=
        column |> Seq.indexed |> Seq.filter (fun (i,x) -> Seq.contains i indices) |> Seq.map snd
    
//    let rec getRemainingColumns f (bits: Bit seq) (remainingColumns:ColumnBits list) : Bits =
//        match remainingColumns with
//        | [] -> bits
//        | head::tail ->
//            let remainingCols =
//                let indices = head |> getIndicesByFunc f
//                tail |> List.map (getRemainingColumn indices)
//            getRemainingColumns f bits remainingCols
//    let actual = getRemainingColumns f columns
//    actual
//            
//        if (remainingColumns |> Seq.length) = 0 then
//            (bits, remainingColumns)
//        else
//            let currentColumn = remainingColumns |> Seq.head
//            let nextRemainingColumns = remainingColumns |> Seq.tail
//            let currentResultBit,validIndicesForNext = currentColumn |> getMostCommonValueWithIndices
//            // based on validIndicesForNext and the nextRemainingColumns: TOOD
//             
//            let nextRoundColumnsPre = remainingColumns |> Seq.indexed validIndicesForNext
////            let foo = nextRoundColumnsPre |> Seq.filter (fun i,elem  ->
////                if elem 
////                )
//        getResults (bits, foo)
        
    // TODO We need a recursive function here
    [One]
    
let getCO2ScrubberRating bss : C02ScrubberRating =
    // TODO 01010
    [Zero; One; Zero; One; Zero] |> Seq.ofList
    
//[<Fact>]
//let ``day 3 - part 2 experiments`` () =
//    let allData = rawDemo |> List.map toBits
//    
//    let oxygenGeneratorRating = getOxygenGeneratorRating allData
//    let expectedOxygenGeneratorRating = 23
//    (oxygenGeneratorRating |> toDecimal) =! expectedOxygenGeneratorRating
//    
//    let co2ScrubberRating = getCO2ScrubberRating allData
//    let expectedCO2ScrubberRating = 10
//    (co2ScrubberRating |> toDecimal) =! expectedCO2ScrubberRating
    
[<Fact>]
let ``playground`` () =
    let column1 = [1;2;3;4;5]
    let column2 = [10;20;30;40;50]
    let column3 = [100;200;300;400;500]
    let allColumns = [column1;column2;column3]
    let f x = x % 2 = 0
    
    // step1: get all indices of 1st column which conform to function f
    let indices1 = column1 |> List.indexed |> List.filter (fun (i,x) -> f x) |> List.map fst
    
    // step2: get entries from column2 at indices
    let reducedCol2 = column2 |> List.indexed |> List.filter (fun (i,x) -> List.contains i indices1) |> List.map snd
    let indices2 = reducedCol2 |> List.indexed |> List.filter (fun (i,x) -> f x) |> List.map fst
    
    //step3: apply to allColumns
    let getIndicesByFunc f column : int list =
        column |> List.indexed |> List.filter (fun (i,x) -> f x) |> List.map fst
        
    let getRemainingColumn indices column : int list =
        column |> List.indexed |> List.filter (fun (i,x) -> List.contains i indices) |> List.map snd
    
    let rec getRemainingColumns f (columns : int list list) : int list =
        match columns with
        | [] -> []
        | head::tail ->
            if List.length tail = 0 then
                head
            else
                let remainingCols =
                    let indices = head |> getIndicesByFunc f
                    tail |> List.map (getRemainingColumn indices)
                getRemainingColumns f remainingCols
    
    let actual = getRemainingColumns f allColumns
    actual =! [200;400]