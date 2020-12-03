type Square =
    | Tree
    | Open

type State = {curr_x:int;trees:int}

let parseSquare c =
    match c with
    | '.' -> Open
    | '#' -> Tree
    | _ -> failwith "invalid input"

let folder (prev_state:State) (field: Square[]) =
    let new_x = (prev_state.curr_x + 3)%field.Length
    let new_trees = match field.[new_x] with
                    | Tree -> prev_state.trees + 1
                    | Open -> prev_state.trees
    {curr_x=new_x;trees=new_trees}

[<EntryPoint>]
let main argv =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let input = Util.Base.readLines "bigInput.txt"
                |> Seq.map (fun l -> l.ToCharArray() |> Array.map parseSquare)
                |> Seq.toArray

    stopwatch.Stop()
    printfn "Reading and parsing input took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let state = input
                |> Array.fold folder {curr_x=(-3);trees=0}

    stopwatch.Stop()
    printfn "Encountered %i trees on the way" state.trees
    printfn "Calculation took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    0 // return an integer exit code