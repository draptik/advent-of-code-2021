module Day6.Part2

type State = int
type States = State seq
let newLanternFishState = 8
let resetLanternFishState = 6
let minimumLanternFishState = 0

type NumberOfNewLanternFish = int

type Action = Spawn | Noop

let nextDay state : State * Action =
    if state = minimumLanternFishState then resetLanternFishState, Spawn
    else (state - 1), Noop

let getStateAction states = states |> Seq.map nextDay

(* TODO After profiling, this function takes up all the time *)
let getNumberOfSpawns stateAction =
    stateAction
    |> Seq.filter (fun (_,a) -> a = Spawn)
    |> Seq.length

let getNewFish numberOfSpawns newLanternFishState = Seq.replicate numberOfSpawns newLanternFishState

let getStates stateAction newFish = Seq.concat [ (stateAction |> Seq.map fst) ; newFish ]     

let getStatesForNextDay (states:States) : States =
    let stateAction = getStateAction states
    let numberOfSpawns = getNumberOfSpawns stateAction
    let newFish = getNewFish numberOfSpawns newLanternFishState
    let states' = getStates stateAction newFish     
    states'

let rec getStatesForDays days states : States =
    if days = 0 then
        states
    else
        let days' = days - 1
        let states' = getStatesForNextDay states
        getStatesForDays days' states'
    
let getNumberOfFishes (states:States) : int64 =
    states |> Seq.fold (fun acc _ -> acc + 1L) (0L)