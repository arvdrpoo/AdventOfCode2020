type Square =
    | Tree
    | Open

type Slope = {right:int; down:int}
type State = {curr_x:int;curr_y:int;trees:int;slope:Slope}

let parseSquare c =
    match c with
    | '.' -> Open
    | '#' -> Tree
    | _ -> failwith "invalid input"

let folder (prev_state:State) (field: (int * Square[])) =
    let new_x = (prev_state.curr_x + prev_state.slope.right)%(snd field).Length
    let new_y = prev_state.curr_y + prev_state.slope.down

    if ((fst field) = new_y) then
        let new_trees = match (snd field).[new_x] with
                        | Tree -> prev_state.trees + 1
                        | Open -> prev_state.trees
        {curr_x=new_x;curr_y=fst field;trees=new_trees;slope=prev_state.slope}
    else
        prev_state

let treeCount input slope =
    input
    |> Seq.indexed
    |> Seq.fold folder {curr_x=(-slope.right);curr_y=(-slope.down);trees=0;slope=slope}

[<EntryPoint>]
let main argv =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let input = Util.Base.readLines "bigInput.txt"
                |> Seq.map (fun l -> l.ToCharArray() |> Array.map parseSquare)
                //|> Seq.toArray

    stopwatch.Stop()
    printfn "Reading and parsing input took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let slope = {right=3;down=1}
    let state = treeCount input slope

    stopwatch.Stop()
    printfn "Encountered %i trees on the way" state.trees
    printfn "Calculation took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let slopes = [|{right=1;down=1};{right=3;down=1};{right=5;down=1};{right=7;down=1};{right=1;down=2}|]

    let result = slopes
                 |> Array.fold (fun acc slope -> acc*(treeCount input slope).trees) 1

    stopwatch.Stop()
    printfn "The product of all trees encountered on all slopes is %i" result
    printfn "Calculation took %i ms" stopwatch.ElapsedMilliseconds

    0 // return an integer exit code