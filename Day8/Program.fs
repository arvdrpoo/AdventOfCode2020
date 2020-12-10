type operation = Acc | Jmp | Nop
type instruction = {operation: operation;argument:int;index:int}

type result = Loop | Ends
type executionResult = {acc:int; result:result }

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
        if index>=instructions.Length then
            {acc=acc;result=Ends}
        else
            let currentInstruction = instructions.[index]
            if visitedInstructions.Contains(currentInstruction.index) then
                {acc=acc;result=Loop}
            else
                let newAcc = if currentInstruction.operation = Acc then acc + currentInstruction.argument else acc
                let newIndex = if currentInstruction.operation = Jmp then index + currentInstruction.argument else index + 1
                let newSet = visitedInstructions.Add(index)
                loop newAcc newIndex newSet

    loop 0 0 Set.empty<int>

let rec findNextIndexToChange index (instructions:instruction[]) =
    match instructions.[index].operation with
    | Jmp | Nop -> index
    | _ -> findNextIndexToChange (index+1) instructions

let execute2 (instructions:instruction[]) =
    let rec loop indexToChange =
        let newInstructions = Array.copy instructions
        newInstructions.[indexToChange] <- match newInstructions.[indexToChange].operation with
                                           | Jmp -> {newInstructions.[indexToChange] with operation=Nop}
                                           | Nop -> {newInstructions.[indexToChange] with operation=Jmp}
                                           | _ -> newInstructions.[indexToChange]

        let res = execute newInstructions
        match res.result with
        | Ends -> res.acc
        | Loop -> loop (findNextIndexToChange (indexToChange+1) instructions)

    loop (findNextIndexToChange 0 instructions)

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
    printfn "The accumulater just before starting a loop contains:  %i" acc.acc
    printfn "Calculation took %ims" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let terminatingAcc = execute2 input
    stopwatch.Stop()
    printfn "The accumulater just before ending contains:  %i" terminatingAcc
    printfn "Calculation took %ims" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()
    0 // return an integer exit code