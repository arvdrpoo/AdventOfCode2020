// Learn more about F# at http://fsharp.org

open System

let countDistinct (answers:char[]) =
    answers
    |> Array.distinct
    |> Array.length

[<EntryPoint>]
let main argv =

    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let input = Util.Base.readAll "bigInput.txt"
                |> Util.Base.parse (fun l -> l.Replace("\r\n","").ToCharArray()) "\r\n\r\n"

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

    0 // return an integer exit code