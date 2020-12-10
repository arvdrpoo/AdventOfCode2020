// Learn more about F# at http://fsharp.org

// Makes a list of tuples from a list
// eg combine a [b;c] -> [(a,b),(a,c)]
let combine item list =
    let rec loop res item list =
        match list with
        | [] -> res
        | head :: tail -> loop ((item,head) :: res) item tail
    loop [] item list

// Makes a list of tuples with every combination in both lists
// eg combineLists [a,b] [c,d] -> [(a;c),(a;d),(b;c),(b,d)]
let combineLists list1 list2 =
    let rec loop res l1 l2 =
        match l1,l2 with
        | [],[] -> res
        | l1head :: l1tail, l2head :: l2tail -> loop (res @ combine l1head l2) l1tail l2tail
        | _ -> failwith "error not same length"
    loop [] list1 list2

let allCombinations list =
    combineLists list list

let isValidNumber preamble =
    let summedPreamble = preamble
                         |> allCombinations
                         |> List.filter (fun a -> (fst a) <> (snd a))
                         |> List.map (fun a -> (fst a) + (snd a))
                         |> List.distinct
                         |> Set.ofList
    summedPreamble.Contains

let findFirstBadNumber preambleLength numbers =
    numbers
    |> Seq.windowed (preambleLength+1)
    |> Seq.find (fun window ->
                    let w = Array.rev window
                    let head = Array.head w
                    let rest = w.[1..]
                    not (isValidNumber (Array.toList rest) head ))
    |> Array.last

let allSubLists l =
    List.scanBack (fun elem acc -> elem::acc) l []

let findContiguousSum (sum:int64) list =
    list
    |> List.rev
    |> allSubLists
    |> List.tryFind (fun l -> l.Length > 1 && (List.sum l) = sum)

[<EntryPoint>]
let main argv =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let inputFile = "bigInput.txt"
    let input = Util.Base.readLines inputFile
                |> Seq.map int64

    let preambleLength = if inputFile = "smallInput.txt" then 5 else 25
    stopwatch.Stop()
    printfn "Reading and parsing input took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let firstBadNumber = findFirstBadNumber preambleLength input
    stopwatch.Stop()
    printfn "The first bad number is %i" firstBadNumber
    printfn "Calculating took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let x = (input
            |> Seq.toList
            |> allSubLists
            |> List.map (fun l -> findContiguousSum firstBadNumber l)
            |> List.find (fun x -> x.IsSome)).Value

    let result = (List.max x) + (List.min x)

    stopwatch.Stop()
    printfn "The contiguous array is %A, the sum of the smallest and largest element is %i" x result
    printfn "Calculating took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()
    0 // return an integer exit code