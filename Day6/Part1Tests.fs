module Day6.Part1Tests

open System
open Xunit
open Swensen.Unquote

open Day6.RawData
open Day6.Part1

[<Fact>]
let ``check getStatesForNextDay`` () =
    let actual = getStatesForNextDay [0]
    actual =! [6;8]

[<Fact>]
let ``get states after 18 day`` () =
    let initialStates = [3;4;3;1;2]
    let numberOfDays = 18
    let actual = getStatesForDays numberOfDays initialStates 
    let expected18 = [6;0;6;4;5;6;0;1;1;2;6;0;1;1;1;2;2;3;3;4;6;7;8;8;8;8]
    actual =! expected18

[<Fact>]
let ``get number of fishes after 80 days`` () =
    let initialStates = rawInitialInternalTimers
    let numberOfDays = 80
    let actual = getStatesForDays numberOfDays initialStates |> getNumberOfFishes 
    let expected = 5934
    actual =! expected
    