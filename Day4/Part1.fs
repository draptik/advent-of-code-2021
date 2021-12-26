module Day4.Part1

type CellValue = int
type CellState = Marked | NotMarked
type RowIndex = int
type ColumnIndex = int
type Position = RowIndex * ColumnIndex
type Cell = {
    Position: Position
    State: CellState
    Value: CellValue
}
type BingoCard = Cell list

let initToBingoCardRow rowIndex (row : int list) : BingoCard =
    row
    |> List.mapi (fun colIndex value ->
        {
            Position = rowIndex, colIndex
            State = NotMarked
            Value = value
        })
    
let toBingoCard (rows : int list list) : BingoCard =
    let result =
        rows
        |> List.mapi initToBingoCardRow
        |> List.collect id
    result

let toBingoCards maxRows (rows : int list list) : BingoCard list =
    rows |> List.chunkBySize maxRows |> List.map toBingoCard
    
let markCellWithValue (value : CellValue) (bingoCard : BingoCard) : BingoCard =
    bingoCard
    |> List.map (fun cell ->
        if cell.Value = value then { cell with State = Marked }
        else cell)

let areAllStatesUnmarked cells =
    cells |> List.forall (fun cell -> (cell.State = NotMarked))
let areAllStatesMarked cells =
    cells |> List.forall (fun cell -> (cell.State = Marked))
    
let getCellRow cell =
    let rowIndex, _ = cell.Position
    rowIndex
let getCellColumn cell =
    let _, columnIndex = cell.Position
    columnIndex
    
let hasBingo bingoCard =
    let bingoByFcn f cs =
        cs
        |> List.groupBy f
        |> List.map (fun (_, cells) -> areAllStatesMarked cells)
        |> List.exists (fun x -> x = true)
        
    let anyRowWithBingo = bingoCard |> bingoByFcn getCellRow
    let anyColumnWithBingo = bingoCard |> bingoByFcn getCellColumn
    anyRowWithBingo || anyColumnWithBingo
        
    