module Day7.Part2

open Day7.Part1

(*  As it turns out, crab submarine engines don't burn fuel at a constant rate.
    Instead, each change of 1 step in horizontal position costs 1 more unit of fuel than the last:
    the first step costs 1, the second step costs 2, the third step costs 3, and so on.
     *)
let calcFuel' (crabPosition:CrabPosition) (targetPosition:TargetPosition) : Fuel =
    let distance = (crabPosition - targetPosition) |> abs
    let result = distance * (distance + 1) / 2
    result

let getFuelUsageAtBestPosition' (crabPositions:CrabPosition list) : Fuel =
    let targetPositions = getTargetPositions crabPositions
    let targetPositionsAndFuelSummaries =
        getTargetPositionsAndFuelSummaries calcFuel' targetPositions crabPositions
    getFuelWithLeastFuelUsage targetPositionsAndFuelSummaries
    