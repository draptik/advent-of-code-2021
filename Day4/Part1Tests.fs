module Day4.Part1

open System
open Day4.Helper
open Day4.RawData
open Xunit
open Swensen.Unquote

[<Fact>]
let ``to draws`` () =
    ("1,2,3" |> toDraws) =! [1;2;3]
    
[<Fact>]
let ``check get draws`` () =
    let actual = draws
    let expected = [7; 4; 9; 5; 11; 17; 23; 2; 0; 14; 21; 24; 10; 16; 13; 6; 15; 25; 12; 22; 18; 20;
 8; 19; 3; 26; 1]
    actual =! expected
    
[<Fact>]
let ``check toInt`` () =
    " 42" |> toInt =! 42

[<Fact>]
let ``check toIntRows (single entry)`` () =
    let input = [ "22 13 17 11  0" ]
    let expected = [[22; 13; 17; 11;  0]];
    let actual = input |> toIntRows
    actual =! expected



[<Fact>]
let ``check toIntRows (multiple entries)`` () =
    let input =
        [
            "22 13 17 11  0";
            " 8  2 23  4 24";
            "21  9 14 16  7";
            " 6 10  3 18  5";
            " 1 12 20 15 19"
        ]
    let expected =
        [
            [22; 13; 17; 11;  0];
            [ 8;  2; 23;  4; 24];
            [21;  9; 14; 16;  7];
            [ 6; 10;  3; 18;  5];
            [ 1; 12; 20; 15; 19]
        ]

    let actual = input |> toIntRows
    actual =! expected

let initToBingoCardRow rowIndex (row : int list) : BingoCard =
    row
    |> List.mapi (fun colIndex value ->
        {
            Position = rowIndex, colIndex
            State = NotMarked
            Value = value
        })

[<Fact>]
let ``check initToBingoCardRow`` () =
    let rowIndex = 0
    let row = [13;42]
    let expected =
        [
            {
                Position = 0, 0
                State = NotMarked
                Value = 13
            };
            {
                Position = 0, 1
                State = NotMarked
                Value = 42
            }
        ]
    let actual = row |> initToBingoCardRow rowIndex
    actual =! expected
    
//let toBingoCard (rows : int list list) : Cell list =
//    let result =
//        rows
//        |> initToBingoCardRow
//    
//    result
    
//[<Fact>]
//let ``check toBingoCard`` () =
    
    
    
[<Fact>]
let ``experiments`` () =
    let vals = [1..25]
    let ar = Array2D.zeroCreate<int> 5 5
    1 =! 1