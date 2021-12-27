module Day4.Part2Tests

open Xunit
open Swensen.Unquote

open Day4.Helper
open Day4.RawData
open Day4.Part1
open Day4.Part2

let setupBoards =
    let maxRowsPerBoard = 5
    remaining |> toIntRows |> toBoards maxRowsPerBoard
    
[<Fact>]
let ``part 2 -sample result`` () =
    let boards = setupBoards 
                
    let boardOpt = determineLastWinner None boards draws.Head draws

    match boardOpt with
    | None -> 1 =! 0
    | Some (b,n) ->
        getSumUnmarkedNumbers b =! 148
        getScore b n =! 1924

[<Fact>]
let ``experiments`` () =
    [0..10] |> List.except [0..8] =! [9;10]