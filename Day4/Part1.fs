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
type Board = Cell list
type Score = int
type UnmarkedSum = int

let initToBoardRow rowIndex (row : int list) : Board =
    row
    |> List.mapi (fun colIndex value ->
        {
            Position = rowIndex, colIndex
            State = NotMarked
            Value = value
        })
    
let toBoard (rows : int list list) : Board =
    let result =
        rows
        |> List.mapi initToBoardRow
        |> List.collect id
    result

let toBoards maxRows (rows : int list list) : Board list =
    rows |> List.chunkBySize maxRows |> List.map toBoard
    
let markCellWithValue (value : CellValue) (board : Board) : Board =
    board
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
    
let hasBingo board =
    let bingoByFcn f cs =
        cs
        |> List.groupBy f
        |> List.map (fun (_, cells) -> areAllStatesMarked cells)
        |> List.exists (fun x -> x = true)
        
    let anyRowWithBingo = board |> bingoByFcn getCellRow
    let anyColumnWithBingo = board |> bingoByFcn getCellColumn
    anyRowWithBingo || anyColumnWithBingo
        
let getSumUnmarkedNumbers board : UnmarkedSum =
    board
    |> List.filter (fun cell -> cell.State = NotMarked)
    |> List.sumBy (fun cell -> cell.Value)
    
let getScore board winningDraw : Score =
    let sumUnmarkedNumbers = getSumUnmarkedNumbers board
    sumUnmarkedNumbers * winningDraw
    
let markBoards draw boards : Board list =
    boards
    |> List.map (markCellWithValue draw)

let hasWinner boards =
    boards
    |> List.tryFind hasBingo

let rec determineWinner boards currentDraw remainingDraws =
    match hasWinner boards with
    | Some board ->
        Some (board, currentDraw)
    | None ->
        match remainingDraws with
        | [] ->
            None
        | nextDraw::remainingDraws' ->
            let markedBoards = markBoards nextDraw boards
            determineWinner markedBoards nextDraw remainingDraws'
                    