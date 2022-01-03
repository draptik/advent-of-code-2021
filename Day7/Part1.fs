module Day7.Part1

type CrabPosition = int
type TargetPosition = int
type Fuel = int

let getTargetPositions (positions: CrabPosition list) : TargetPosition list =
    let min = positions |> List.min
    let max = positions |> List.max
    [min..max]
    
let calcFuel (crabPosition:CrabPosition) (targetPosition:TargetPosition) : Fuel =
    (crabPosition - targetPosition) |> abs

let calcFuelForCrabsAtPosition
    (f: CrabPosition -> TargetPosition -> Fuel)
    (crabPositions:CrabPosition list)
    (targetPosition:TargetPosition)
    : TargetPosition * Fuel =
    let sumOfFuelUsage =
        crabPositions
        |> List.map (fun crabPosition -> f crabPosition targetPosition)
        |> List.sum
    (targetPosition, sumOfFuelUsage)

let getTargetPositionWithLeastFuelUsage (targetAndFuel:(TargetPosition * Fuel) list) : TargetPosition =
    targetAndFuel
    |> List.minBy snd
    |> fst

let getFuelWithLeastFuelUsage (targetAndFuel:(TargetPosition * Fuel) list) : Fuel =
    targetAndFuel
    |> List.minBy snd
    |> snd

let getTargetPositionsAndFuelSummaries
    (f: CrabPosition -> TargetPosition -> Fuel)
    (targetPositions:TargetPosition list)
    (crabPositions:CrabPosition list)
    : (TargetPosition * Fuel) list =
    targetPositions |> List.map (fun tp -> calcFuelForCrabsAtPosition f crabPositions tp)
    
let getPositionUsingLeastFuel (crabPositions:CrabPosition list) : TargetPosition =
    let targetPositions = getTargetPositions crabPositions
    let targetPositionsAndFuelSummaries =
        getTargetPositionsAndFuelSummaries calcFuel targetPositions crabPositions
    let targetPositionWithLeastFuelUsage =
        getTargetPositionWithLeastFuelUsage targetPositionsAndFuelSummaries
    
    targetPositionWithLeastFuelUsage

let getFuelUsageAtBestPosition (crabPositions:CrabPosition list) : Fuel =
    let targetPositions = getTargetPositions crabPositions
    let targetPositionsAndFuelSummaries =
        getTargetPositionsAndFuelSummaries calcFuel targetPositions crabPositions
    getFuelWithLeastFuelUsage targetPositionsAndFuelSummaries
    