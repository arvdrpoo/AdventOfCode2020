// Learn more about F# at http://fsharp.org

let countDistinct (answers:string[]) =
    answers
    |> Array.collect (fun s -> s.ToCharArray())
    |> Array.distinct
    |> Array.length

let countEveryone (answers:string []) =
    let participantCount = answers.Length
    answers
        |> Array.collect (fun s -> s.ToCharArray())
        |> Array.countBy id
        |> Array.sumBy (fun c ->
            if ((snd c)=participantCount) then 1 else 0 )

[<EntryPoint>]
let main argv =

    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let input = Util.Base.readAll "bigInput.txt"
                |> Util.Base.parse (fun l -> l.Split("\r\n")) "\r\n\r\n"

    stopwatch.Stop()
    printfn "Reading and parsing input took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let count = input
                |>Array.map countDistinct
                |> Array.sum

    stopwatch.Stop()
    printfn "The number of questions anyone answered 'yes' to is %i" count
    printfn "Calculating part 1 took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let count2 = input
                 |> Array.map countEveryone
                 |> Array.sum

    stopwatch.Stop()
    printfn "The number of questions everyone answered 'yes' to is %i" count2
    printfn "Calculating part 2 took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()
    0 // return an integer exit code