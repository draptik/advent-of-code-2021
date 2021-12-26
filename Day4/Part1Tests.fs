module Day4.Part1Tests

open Day4.Helper
open Day4.RawData
open Day4.Part1
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

[<Fact>]
let ``check toIntRows (multiple entries with empty rows)`` () =
    let input =
        [
            "";
            "22 13 17 11  0";
            " 8  2 23  4 24";
            "21  9 14 16  7";
            " 6 10  3 18  5";
            " 1 12 20 15 19"
            ""
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
    
[<Fact>]
let ``check toBingoCard`` () =
    let input =
        [
            "22 13";
            " 8  2"
        ]
    let expected =
        [
            {
              Position = (0, 0)
              State = NotMarked
              Value = 22
            }
            { Position = (0, 1)
              State = NotMarked
              Value = 13
            }
            { Position = (1, 0)
              State = NotMarked
              Value = 8
            }
            { Position = (1, 1)
              State = NotMarked
              Value = 2
            }
        ]
    let actual = input |> toIntRows |> toBingoCard
    actual =! expected
    
[<Fact>]
let ``check markCellWithValue`` () =
    let input =
        [
            "22 13 17 11  0";
            " 8  2 23  4 24";
            "21  9 14 16  7";
            " 6 10  3 18  5";
            " 1 12 20 15 19"
        ]
    let bingoCard = input |> toIntRows |> toBingoCard
    let actual = bingoCard |> markCellWithValue 22
    let firstCell = actual |> List.find (fun cell -> cell.Position = (0,0))
    
    bingoCard |> areAllStatesUnmarked =! true
    firstCell.State =! Marked
    actual |> areAllStatesUnmarked =! false

[<Fact>]
let ``check hasBingo (row)`` () =
    let input =
        [
            "22 13 17 11  0";
            " 8  2 23  4 24";
            "21  9 14 16  7";
            " 6 10  3 18  5";
            " 1 12 20 15 19"
        ]
    let bingoCard = input |> toIntRows |> toBingoCard
    let actual = bingoCard
                 |> markCellWithValue 22
                 |> markCellWithValue 13
                 |> markCellWithValue 17
                 |> markCellWithValue 11
                 |> markCellWithValue 0
                 |> hasBingo
    actual =! true

[<Fact>]
let ``check hasBingo (column)`` () =
    let input =
        [
            "22 13 17 11  0";
            " 8  2 23  4 24";
            "21  9 14 16  7";
            " 6 10  3 18  5";
            " 1 12 20 15 19"
        ]
    let bingoCard = input |> toIntRows |> toBingoCard
    let actual = bingoCard
                 |> markCellWithValue 22
                 |> markCellWithValue 8
                 |> markCellWithValue 21
                 |> markCellWithValue 6
                 |> markCellWithValue 1
                 |> hasBingo
    actual =! true        

[<Fact>]
let ``import multiple bingo cards`` () =
    let inputs = remaining
    let actual = inputs |> toIntRows |> toBingoCards 5
    let expectedNumberOfBingoCards = 3
    actual.Length =! expectedNumberOfBingoCards
    actual |> List.forall (fun x -> x.Length = 25) =! true
    
[<Fact>]
let ``experiments`` () =
//    let vals = [1..25]
//    let ar = Array2D.zeroCreate<int> 5 5
//    1 =! 1
    let items = [ Some(1); None; Some(8); ]
    items |> List.choose id =! [1;8]
    
    