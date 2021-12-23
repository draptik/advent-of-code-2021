module Day4.Helper

open System

let toInt (s: string) =
    s.Trim() |> int
    
let toIntRows (rows: string list) =
    rows
    |> List.map (fun x ->
        x.Split(' ')
        |> List.ofArray
        |> List.filter (fun x -> String.IsNullOrWhiteSpace(x) |> not)
        |> List.map (fun s -> s |> toInt))    