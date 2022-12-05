open System.IO

let lines = File.ReadAllLines "/tmp/aoc/input" |> Array.toList

let rec firstPart input =
    match input with
    | ""::rest -> []
    | a::rest -> a :: firstPart rest
    | [] -> []

let rec secondPart input =
    match input with
    | ""::rest -> rest
    | [] -> []
    | a::rest -> secondPart rest
    
let board = firstPart lines
let movelines = secondPart lines 

board |> List.map (printfn "%A")

let rec splitCrates (line:string) = 
    match line.Length with
    | 3 -> [line]
    | _ -> line.Substring(0,4) :: (splitCrates (line.Substring(4)))

let crateLines = board |> List.map splitCrates

crateLines |> List.map (printfn "%A")

let stacknum = crateLines.Head.Length

let rec splitStacks (crateLines: string list list) =
    match crateLines.Head.Length with
    | 0 -> []
    | _ ->
        let stack = crateLines |> List.map List.head
        let rest = crateLines |> List.map List.tail
        stack :: (splitStacks rest)
        
let stacks = splitStacks crateLines

stacks |> List.map (printfn "%A")

let rec cleanStack (l:string list) =
    match l with
    | "    " :: rest -> cleanStack rest
    | "   " :: rest -> cleanStack rest
    | [] -> []
    | [num] -> [num.Trim ()]
    | i :: rest -> i.Substring(1,1) :: (cleanStack rest)
    
let stacks2 = stacks |> List.map cleanStack

stacks2 |> List.map (printfn "%A")

type Move (count: int, from: int, dest: int) =
    member this.Count = count
    member this.From = from
    member this.Dest = dest
    override this.ToString() =
        $"[Move {count} from {from} to dest {dest}]"
    
let rec parseMove (line:string) =
    let split = line.Split [|' '|]
    let count = split[1] |> int
    let from = split[3] |> int
    let dest = split[5] |> int
    Move(count,from,dest)

let moves = movelines |> List.map parseMove 

moves |> List.map (printfn "{%A}")

type Stacks (stacks:string list list) =
   
    let take (move:Move) : (string list)*Stacks =
        let index = move.From - 1
        let front = stacks |> List.take index
        let target = stacks |> List.skip index |> List.head
        let back = stacks |> List.skip (index + 1)
        let crates = target |> List.take (move.Count)
        let target = target |> List.skip (move.Count)
        let stacks = [front;[target];back] |> List.concat
        (crates,Stacks(stacks))
    
    member this.Put (crates: string list) (move:Move) =
        let index = move.Dest - 1
        let front = stacks |> List.take index
        let target = stacks |> List.skip index |> List.head
        let back = stacks |> List.skip (index + 1)
        let target = [crates;target] |> List.concat
        [front;[target];back] |> List.concat |> Stacks 
     
    member this.applyMove (move:Move) =
        let (crates,stacks) = take move
        // let crates = crates |> List.rev 
        let stacks = stacks.Put crates move  
        printfn $"MR {(crates,stacks)}"
        stacks 
    
    member this.ResultString =
        let tops = stacks |> List.map List.head
        String.concat "" tops
               
    override this.ToString () =
        $"Stacks: {stacks}"
        
let stack3 = stacks2 |> Stacks 

printfn $"{stack3}"

let stack4 = stack3.applyMove moves.Head

let rec applyMoves (stacks:Stacks) (moves:Move list) =
    match moves with
    | [] -> stacks
    | m::rest ->
        let stacks = stacks.applyMove m
        applyMoves stacks rest 

let result = applyMoves stack3 moves 

printfn $"{result}"

printfn $"Result: {result.ResultString}"