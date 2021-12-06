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

let toBitsColumn (bits: Bits seq) (index: int) : Bits =
    bits |> Seq.map (fun bs -> getBitAt bs index)

let getBitsWidth bss =
    bss |> Seq.head |> Seq.length

let getMostCommonValue (bits: Bits) : Bit =
    let numberOfOnes = bits |> Seq.filter (fun b -> b = One) |> Seq.length
    let numberOfZeros = (bits |> Seq.length) - numberOfOnes
    if numberOfOnes >= numberOfZeros then One
    else Zero

let getGammaRate (bss: Bits seq) : GammaRate =
    [0..(getBitsWidth bss)-1]
    |> List.map (toBitsColumn bss)
    |> List.map getMostCommonValue
    |> Seq.ofList
    
let invertBits bs =
    bs
    |> Seq.map (fun b ->
        match b with
        | One -> Zero
        | Zero -> One)

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
    