module Day6.Part1

type State = int
type States = State list
let newLanternFishState = 8
let resetLanternFishState = 6
let minimumLanternFishState = 0

type NumberOfNewLanternFish = int

type Action = Spawn | Noop
let nextDay state : State * Action =
    if state = minimumLanternFishState then resetLanternFishState, Spawn
    else (state - 1), Noop

let getStatesForNextDay (states:States) : States =
    let stateAction = states |> List.map nextDay
    let numberOfSpawns = stateAction |> List.filter (fun (_,a) -> a = Spawn) |> List.length
    let newFish = List.replicate numberOfSpawns newLanternFishState
    let states' = (stateAction |> List.map fst) @ newFish     
    states'

let rec getStatesForDays days states : States =
    if days = 0 then
        states
    else
        let days' = days - 1
        let states' = getStatesForNextDay states
        getStatesForDays days' states'
    
let getNumberOfFishes (states:States) : int =
    states |> List.length