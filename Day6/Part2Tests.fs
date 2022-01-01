module Day6.Part2Tests

open System
open Xunit
open Swensen.Unquote
open Day6.RawData
open Day6.Part2


//[<Fact>]
//let ``get number of fishes after 128 days`` () =
//    let initialStates = rawInitialInternalTimers
//    let numberOfDays = 128
//    let statesAndSpawns' = initialStates |> List.map (fun s -> s,Noop)
//    let statesAndSpawns = getStatesForDays numberOfDays statesAndSpawns'
//    let actual = statesAndSpawns |> getNumberOfFishes
//    let expected = 388_976L //26_984_457_539L
//    actual =! expected


[<Fact>]
let ``get number of fishes after 128 days`` () =
    let initialStates = rawInitialInternalTimers
    let numberOfDays = 128
    let actual = getStatesForDays numberOfDays initialStates |> getNumberOfFishes 
    let expected = 388_976L // 26_984_457_539L
    actual =! expected

[<Fact>]
let ``check getNewFish`` () =
    let actual = getNewFish 3 8
    let expected = [8;8;8]
    actual =! expected
    
[<Fact>]
let ``check getLength`` () =
    let actual = [1L..Int64.MaxValue] |> List.length |> uint64
    actual =! 1UL