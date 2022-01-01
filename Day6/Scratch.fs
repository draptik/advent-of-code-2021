module Day6.Scratch
open Xunit
open Swensen.Unquote
open Day6.RawData

// from https://github.com/bainewedlock/aoc-2021-fsharp/blob/master/aoc-2021-06/Solution.fs

let parse (input:string) =
    input.Split ','
    |> Array.toList
    |> List.map int
    |> List.groupBy id
    |> List.map (fun (k,v) -> k, uint64 v.Length)

let step fs =
    fs |> List.collect (fun (x,n) ->
        if x = 0
        then [6, n; 8, n]
        else [x-1, n])

let reduce =
    List.groupBy fst
    >> List.map (fun (k,vs) -> k, List.sumBy snd vs)

let genericSolve totalDays (input:string) =
    let rec loop days fs =
        if days = 0 then fs else
        loop (days-1) (fs |> step |> reduce)
    loop totalDays (parse input)
    |> List.sumBy snd
    |> sprintf "%d"

let solve  = genericSolve 80 "3,4,3,1,2"
let solve2 = genericSolve 256 "3,4,3,1,2"

let input = "2,5,3,4,4,5,3,2,3,3,2,2,4,2,5,4,1,1,4,4,5,1,2,1,5,2,1,5,1,1,1,2,4,3,3,1,4,2,3,4,5,1,2,5,1,2,2,5,2,4,4,1,4,5,4,2,1,5,5,3,2,1,3,2,1,4,2,5,5,5,2,3,3,5,1,1,5,3,4,2,1,4,4,5,4,5,3,1,4,5,1,5,3,5,4,4,4,1,4,2,2,2,5,4,3,1,4,4,3,4,2,1,1,5,3,3,2,5,3,1,2,2,4,1,4,1,5,1,1,2,5,2,2,5,2,4,4,3,4,1,3,3,5,4,5,4,5,5,5,5,5,4,4,5,3,4,3,3,1,1,5,2,4,5,5,1,5,2,4,5,4,2,4,4,4,2,2,2,2,2,3,5,3,1,1,2,1,1,5,1,4,3,4,2,5,3,4,4,3,5,5,5,4,1,3,4,4,2,2,1,4,1,2,1,2,1,5,5,3,4,1,3,2,1,4,5,1,5,5,1,2,3,4,2,1,4,1,4,2,3,3,2,4,1,4,1,4,4,1,5,3,1,5,2,1,1,2,3,3,2,4,1,2,1,5,1,1,2,1,2,1,2,4,5,3,5,5,1,3,4,1,1,3,3,2,2,4,3,1,1,2,4,1,1,1,5,4,2,4,3"

let solve2' = genericSolve 256 input

[<Fact>]
let ``check solve2`` () =
    let actual = solve2'
    actual =! ""