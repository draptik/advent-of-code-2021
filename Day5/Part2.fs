module Day5.Part2

open Day5.Part1

let getOrientedLine' line =
    let x1, y1 = line.Start
    let x2, y2 = line.End
    match (x1, y1, x2, y2) with
    | x1, y1, x2, y2 when x1 = x2 && y1 = y2 -> Other
    | x1, y1, x2, y2 when abs (x1 - x2) = abs (y1 - y2) -> Diagonal line 
    | x1, _, x2, _ when x1 = x2 -> Horizontal line
    | _, y1, _, y2 when y1 = y2 -> Vertical line
    | _ -> Other

let getCoverage' line : Coverage option =
    let sort a b = if a < b then [a..b] else [b..a]
    match line with
    | Horizontal l ->
        let x1, y1 = l.Start 
        let _, y2 = l.End
        let result =
            sort y1 y2 
            |> List.map (fun y -> x1,y)
        Some result    
    | Vertical l ->
        let x1, y1 = l.Start 
        let x2, _ = l.End
        let result =
            sort x1 x2 
            |> List.map (fun x -> x,y1)
        Some result
    | Diagonal l ->
        let x1, y1 = l.Start 
        let x2, y2 = l.End
        let xs = if x1 < x2 then [x1..x2] else [x1.. -1 ..x2]
        let ys = if y1 < y2 then [y1..y2] else [y1.. -1 ..y2]
        let result = List.map2 (fun x y -> x,y) xs ys
        Some result
    | _ -> None
