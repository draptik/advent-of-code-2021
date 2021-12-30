module Day6.Part2Tests

open Xunit
open Swensen.Unquote
open Day6.RawData
open Day6.Part2


[<Fact>]
let ``get number of fishes after 128 days`` () =
    let initialStates = rawInitialInternalTimers
    let numberOfDays = 128
    let states = getStatesForDays numberOfDays initialStates
    let actual = states |> getNumberOfFishes
    let expected = 388_976L //26_984_457_539L
    actual =! expected
    