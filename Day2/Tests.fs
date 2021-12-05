module Tests

open System
open Xunit

[<Literal>]
let input = "../../../input.txt"
let inputData = System.IO.File.ReadAllLines(input) |> Array.toList

type Position = {Depth: int; Horizontal: int; Aim: int}

let initPosition = {Depth = 0; Horizontal = 0; Aim = 0}

let getPosition (position:Position) =
    (position.Horizontal, position.Depth)

let getMultiplyResult (position:Position) =
    let (depth, horizontal) = getPosition position
    depth * horizontal

type Command =
    | Forward of int
    | Down of int
    | Up of int
    | Invalid of string

let toCommand (s:string) : Command =
    let split = s.Split ' '
    let direction = split.[0]
    let distance = split.[1] |> int

    if direction = "down" then
        Down distance
    else if direction = "up" then
        Up distance
    else if direction = "forward" then
        Forward distance
    else
        Invalid "Unknown direction"

let move command position =
    match command with
    | Forward distance ->
        let newPosition =
            if position.Horizontal + distance >= 0
            then {position with Depth = position.Depth + distance * position.Aim; Horizontal = position.Horizontal + distance}
            else {position with Depth = position.Depth + distance * position.Aim; Horizontal = position.Horizontal - distance}
        newPosition
    | Down distance ->
        let newPosition =
            if position.Depth + distance >= 0
            then {position with Aim = position.Aim + distance}
            else {position with Aim = position.Aim - distance}
        newPosition
    | Up distance ->
        let newPosition =
            if position.Depth - distance >= 0
            then {position with Aim = position.Aim - distance}
            else {position with Aim = position.Aim + distance}
        newPosition
    | Invalid _ -> position

[<Fact>]
let ``toCommand works`` () =
    let singleInput = "forward 3"
    let actual = toCommand singleInput
    let expected = Forward 3
    Assert.Equal(expected, actual)

[<Fact>]
let ``use demo data`` () =
    let samples = [
        "forward 5"
        "down 5"
        "forward 8"
        "up 3"
        "down 8"
        "forward 2"
    ]

    let actual =
        samples
        |> List.map toCommand
        |> List.fold (fun acc command -> move command acc) initPosition

    let expectedPosition = {Depth = 60; Horizontal = 15; Aim = 0} |> getPosition
    let actualPosition = getPosition actual

    let expectedMultiplied = 900
    let actualMultiplied = getMultiplyResult actual
    Assert.Equal(expectedPosition, actualPosition)
    Assert.Equal(expectedMultiplied, actualMultiplied)

[<Fact>]
let ``day 2 works`` () =
    let actual =
        inputData
        |> List.map toCommand
        |> List.fold (fun acc command -> move command acc) initPosition

    let expectedPosition = {Depth = 984716; Horizontal = 1991; Aim = 0} |> getPosition
    let actualPosition = getPosition actual
    Assert.Equal(expectedPosition, actualPosition)
    
    let expectedMultiplied = 1960569556
    let actualMultiplied = getMultiplyResult actual
    Assert.Equal(expectedMultiplied, actualMultiplied)
