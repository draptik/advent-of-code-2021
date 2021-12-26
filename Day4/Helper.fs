module Day4.Helper

open System

let toInt (s: string) = s.Trim() |> int
    
let isEmpty (s : string) = String.IsNullOrWhiteSpace(s)
let hasValue s = not (isEmpty s)

let toIntRows (rows: string list) =
    rows
    |> List.map (fun row ->
        match row with
        | x when isEmpty x -> None
        | x ->
            x.Split(' ')
            |> List.ofArray
            |> List.filter hasValue
            |> List.map toInt
            |> Some)
    |> List.choose id
