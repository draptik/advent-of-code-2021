module Day4.Part2

open Day4.RawData
open Day4.Part1

let rec determineLastWinner
    (previousWinner:(Board * Draw) option)
    (remainingBoards:Board list)
    (currentDraw:Draw)
    (remainingDraws:Draws)
    : (Board * Draw) option =
    
    if List.isEmpty remainingBoards then
        match previousWinner with
        | None -> None
        | Some pWinner -> Some pWinner
    else
        match tryGetFirstWinner remainingBoards with
        | Some board ->
            let currentWinner = Some (board, currentDraw)
            let remainingBoards' = remainingBoards |> List.except [board]
            match remainingDraws with
            | [] -> currentWinner
            | currentDraw'::remainingDraws' ->
                let markedBoards = markBoards currentDraw' remainingBoards'
                determineLastWinner currentWinner markedBoards currentDraw' remainingDraws'
        | None ->
            match remainingDraws with
            | [] -> previousWinner
            | currentDraw'::remainingDraws' ->
                let markedBoards = markBoards currentDraw' remainingBoards
                determineLastWinner previousWinner markedBoards currentDraw' remainingDraws'
        
    
