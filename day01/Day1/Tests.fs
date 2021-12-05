module Tests

open System
open Xunit

[<Literal>]
let input = "../../../../input.txt"

[<Fact>]
let ``input has 2000 lines`` () =
    let lines = System.IO.File.ReadAllLines(input)
    Assert.Equal(2000, lines.Length)

