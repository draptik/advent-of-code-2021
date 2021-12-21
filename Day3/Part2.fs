module Day3.Part2

open System
open Xunit
open Swensen.Unquote

let sampleData = [
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

(* Types and helper ================================================================================================= *)
type Bit = Zero | One
type Bits = Bit list
type ColumnBits = Bits
type Index = int

let toBit (c:char) : Bit =
    if c = '1' then One
    else Zero

let toBits (s:string) : Bits =
    s |> Seq.map toBit |> List.ofSeq

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

let extractBitFromIndexedBit (_, bit) = bit
let extractIndexFromIndexedBit (index, _) = index

let isBit bitToFilterBy indexedBit =
    (indexedBit |> extractBitFromIndexedBit) = bitToFilterBy

let getIndexedBitsByFilterBit bitToFilterBy indexedBits =
    indexedBits |> List.indexed |> List.filter (isBit bitToFilterBy)

(* Logic =============================================================================================================*)
type IndexedBit = Index * Bit
type Ones = IndexedBit list
type Zeros = IndexedBit list
type IndexedBits =
    | Ones
    | Zeros
type IndexedBitBag = Ones * Zeros

let getOnesAndZerosWithIndices (bits: ColumnBits) : IndexedBitBag =
    let onesWithIndices = bits |> getIndexedBitsByFilterBit One
    let zerosWithIndices = bits |> getIndexedBitsByFilterBit Zero
    onesWithIndices, zerosWithIndices

let getMostCommonValueWithIndices (bits: ColumnBits) : Bit * Index list =
    let onesWithIndices, zerosWithIndices = bits |> getOnesAndZerosWithIndices
    if (List.length onesWithIndices) >= (List.length zerosWithIndices) then
        One, (onesWithIndices |> List.map extractIndexFromIndexedBit)
    else
        Zero, (zerosWithIndices |> List.map extractIndexFromIndexedBit)

let getLeastCommonValueWithIndices (bits: ColumnBits) : Bit * Index list =
    let onesWithIndices, zerosWithIndices = bits |> getOnesAndZerosWithIndices
    if (List.length onesWithIndices) >= (List.length zerosWithIndices) then
        Zero, (zerosWithIndices |> List.map extractIndexFromIndexedBit)
    else
        One, (onesWithIndices |> List.map extractIndexFromIndexedBit)

let getRemainingColumn (indices:Index list) (column:Bit list) : ColumnBits =
    column |> List.indexed |> List.filter (fun (i,x) -> List.contains i indices) |> List.map snd

let appendBit bits bit : Bit list =
    bit :: (bits |> List.rev) |> List.rev

let getFirstBitFromColumns (cols:ColumnBits list) =
    cols |> List.map (fun c -> c |> List.head)
    
let calcAll f allColumns =
    
    let rec calcRecursive (resultBits: Bit list) (columns : ColumnBits list) : Bit list =
        match columns with
        | [] ->
            resultBits
        | head::tail ->
            let (value : Bit), (indices : Index list) = f head
            let (newBits : Bit list) = appendBit resultBits value
            let (remainingColumns : ColumnBits list) = tail |> List.map (getRemainingColumn indices)
            
            if (List.length indices) = 1 then
                let finalRemainingBits = getFirstBitFromColumns remainingColumns
                newBits @ finalRemainingBits 
            else
                calcRecursive newBits remainingColumns
    
    let initialBits = []
    let result = calcRecursive initialBits allColumns
    result
    
let convertToColumns data =
    let getBitsWidth bss = bss |> List.head |> List.length
    let getBitAt bits index = bits |> List.item index
    let toBitsColumn (bits: Bits list) (index: int) : ColumnBits = bits |> List.map (fun bs -> getBitAt bs index)

    let bss = data |> List.map toBits
    let columns = [0..(getBitsWidth bss)-1] |> List.map (toBitsColumn bss)
    columns
    
[<Fact>]
let ``oxygen generator rating`` () =
    let columns = convertToColumns sampleData
    let actual = calcAll getMostCommonValueWithIndices columns
    actual =! [One; Zero; One; One; One]
    (toDecimal actual) =! 23

[<Fact>]
let ``co2 scrubber rating`` () =
    let columns = convertToColumns sampleData
    let actual = calcAll getLeastCommonValueWithIndices columns
    actual =! [Zero; One; Zero; One; Zero]
    (toDecimal actual) =! 10

[<Fact>]
let ``appendBit works`` () =
    let list = [One; Zero; Zero]
    let newElement = One
    (appendBit list newElement) =! [One;Zero;Zero;One]

[<Literal>]
let input = "../../../input.txt"
let inputData = System.IO.File.ReadAllLines(input) |> Array.toList

[<Fact(Skip = "not the correct answer - no spoilers")>]
let ``final check`` () =
    let columns = convertToColumns inputData
    
    let co2ScrubberRatingBits = calcAll getLeastCommonValueWithIndices columns
    let co2ScrubberRating = co2ScrubberRatingBits |> toDecimal
    
    let oxygenGeneratorRatingBits = calcAll getMostCommonValueWithIndices columns
    let oxygenGeneratorRating = oxygenGeneratorRatingBits |> toDecimal
    
    let actual = oxygenGeneratorRating * co2ScrubberRating
    
    actual =! 42
    