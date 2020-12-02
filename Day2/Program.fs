// Learn more about F# at http://fsharp.org

type PasswordPolicy = { min:int; max:int; character:char }
type PasswordLine = { policy:PasswordPolicy; password:string }

let parseLine (line:string) =
    let split = line.Split [|' '|]
    let minmax = split.[0].Split [|'-'|]
    let min = int minmax.[0]
    let max = int minmax.[1]
    let char = split.[1].[0]
    let password = split.[2]
    {policy={min=min;max=max;character=char}; password=password}

let isValidLine line =
    let matching = line.password.ToCharArray()
                   |> Array.filter (fun c -> c = line.policy.character)
    matching.Length >= line.policy.min && matching.Length <= line.policy.max

[<EntryPoint>]
let main argv =
    let input = Util.Base.readLines "bigInput.txt"
                |> Seq.map parseLine

    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let validLines = input
                    |> Seq.filter isValidLine
                    |> Seq.toList

    printfn "The number of valid lines is %i" validLines.Length
    stopwatch.Stop()
    printfn "Calculation took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()
    0 // return an integer exit code