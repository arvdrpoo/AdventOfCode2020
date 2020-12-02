namespace Util

module Base =
    open System.IO

    let parse parseLine (input:string)=
        input.Split([|"\n\r"|],System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parseLine

    let readLines (filePath:string) = seq{
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

module Say =
    let hello name =
        printfn "Hello %s" name

module Arr =
    let tee a =
        printfn "%A" a
        a