namespace Util

module Base =
    open System.IO

    let parse parseLine (splitString:string) (input:string)=
        input.Split([|splitString|],System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parseLine

    let readLines (filePath:string) = seq{
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let readAll (filePath:string) =
        use sr = new StreamReader (filePath)
        sr.ReadToEnd()

module Say =
    let hello name =
        printfn "Hello %s" name

module Arr =
    let tee a =
        printfn "%A" a
        a