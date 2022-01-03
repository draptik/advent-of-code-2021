let input = "./Day7/input.txt"
let data = (System.IO.File.ReadAllLines(input)
           |> Array.toList
           |> List.head).Split(',')
           |> List.ofArray
           |> List.map int
//           |> List.sort
//           |> List.distinct

let r = [(List.min data)..(List.max data)]
r |> List.length

