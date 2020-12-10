type operation = Acc | Jmp | Nop
type instruction = {operation: operation;argument:int;index:int}

let parseLine (line:int*string) =
    let index = fst line
    let split= (snd line).Split(" ")
    let operation = match split.[0] with
                    | "acc" -> Acc
                    | "jmp" -> Jmp
                    | "nop" -> Nop
                    | _ -> failwith "unrecognized instruction!"

    let arg = int (split.[1])
    {operation=operation;argument=arg; index=index}

let execute (instructions:instruction[]) =
    let rec loop (acc:int) (index:int) (visitedInstructions:Set<int>) =
        let currentInstruction = instructions.[index]
        printfn "%A" currentInstruction
        if visitedInstructions.Contains(currentInstruction.index) then
            acc
        else
            let newAcc = if currentInstruction.operation = Acc then acc + currentInstruction.argument else acc
            let newIndex = if currentInstruction.operation = Jmp then index + currentInstruction.argument else index + 1
            let newSet = visitedInstructions.Add(index)
            loop newAcc newIndex newSet

    loop 0 0 Set.empty<int>

[<EntryPoint>]
let main argv =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let input = Util.Base.readLines "bigInput.txt"
                |> Seq.indexed
                |> Seq.map parseLine
                |> Seq.toArray

    stopwatch.Stop()
    printfn "Reading and parsing input took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let acc = execute input
    stopwatch.Stop()
    printfn "The accumulater just before starting a loop contains:  %i" acc
    printfn "Calculation took %ims" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()
    0 // return an integer exit code