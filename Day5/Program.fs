// Learn more about F# at http://fsharp.org

open System

type binarySelector = Vooraan | Achteraan

type seatSpec = {rowSpec:binarySelector list; columnSpec:binarySelector list}
type seat = {row:int;column:int}

let parseLine (line:string) =
     Array.foldBack (fun curr state ->
        match curr with
        | 'F' -> {rowSpec= Vooraan :: state.rowSpec;columnSpec=state.columnSpec}
        | 'B' -> {rowSpec= Achteraan :: state.rowSpec;columnSpec=state.columnSpec}
        | 'L' -> {rowSpec= state.rowSpec;columnSpec= Vooraan :: state.columnSpec}
        | 'R' -> {rowSpec= state.rowSpec;columnSpec= Achteraan :: state.columnSpec}
        | _ -> failwith "unexpected character"
        ) (line.ToCharArray()) {rowSpec=[];columnSpec=[]}

let calculateSeat seat =
    let rec calculate lower upper rowSelectors =
        match rowSelectors with
        | [] -> lower
        | head :: tail ->
            match head with
            | Vooraan -> calculate lower (lower + (upper-lower)/2) tail
            | Achteraan -> calculate (lower+((upper-lower)/2)+1) upper tail

    let row = calculate 0 127 seat.rowSpec
    let col = calculate 0 7 seat.columnSpec
    {row=row;column=col}

let calculateSeatId seat =
    seat.row*8 + seat.column

[<EntryPoint>]
let main argv =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let input = Util.Base.readLines "bigInput.txt"
                |> Seq.map parseLine

    stopwatch.Stop()
    printfn "Reading and parsing input took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let seatIds = input
                |> Seq.map calculateSeat
                |> Seq.map calculateSeatId

    stopwatch.Stop()
    printfn "The highest seat id is %i" (Seq.max seatIds)
    printfn "Calculating part 1 took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let missing = seatIds
                 |> Seq.sort
                 |> Seq.windowed 3
                 |> Seq.fold (fun state curr ->
                    if curr.[0] + 1 <> curr.[1]
                        && curr.[2] - 2 <> curr.[0]
                        then
                            curr.[0]+1
                        else state) -1

    stopwatch.Stop()
    printfn "The missing seat id is %i" missing
    printfn "Calculating part 1 took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    0 // return an integer exit code