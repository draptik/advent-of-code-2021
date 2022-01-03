let sampleEntry = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
let samples = sampleEntry.Split(" | ")
let rawSignalPatterns = samples.[0].Split(" ") // 10 patterns
let rawOutputValues = samples.[1].Split(" ") // 4 output values

type RawSignalPattern = string

type Segment = A | B | C | D | E | F | G

type Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine

type Wire = A' | B' | C' | D' | E' | F' | G'

type WireElement = Wire list

type DisplayElement = Wire list
    
type CharToWire = char -> Wire
let charToWire : CharToWire =
    fun c ->
        match c with
        | c when c = 'a' -> A'
        | c when c = 'b' -> B'
        | c when c = 'c' -> C'
        | c when c = 'd' -> D'
        | c when c = 'e' -> E'
        | c when c = 'f' -> F'
        | c when c = 'g' -> G'
        | _ -> failwith "invalid wire"

type RawSignalPatternToWireElement = RawSignalPattern -> WireElement
let rawSignalPatternToWireElement : RawSignalPatternToWireElement =
    fun rawSignalPattern ->
        rawSignalPattern
        |> Seq.map charToWire
        |> List.ofSeq
    


