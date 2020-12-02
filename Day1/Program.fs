// Learn more about F# at http://fsharp.org

open System

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

let combineTriple list1 list2 list3 =
    let rec loop res l1 l2 =
        match l1 with
        | [] -> res
        | head :: tail -> loop (res @ combine head l2) tail l2
    let comb = combineLists list1 list2
    loop [] list3 comb
    |> List.map (fun x -> (fst x, fst (snd x), snd (snd x)))

let allCombinations3 list =
    combineTriple list list list

let mult2Tuple t =
    match t with
    | (a,b) -> a*b

let mult3Tuple t =
    match t with
    | (a,b,c) -> a*b*c

[<EntryPoint>]
let main argv =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let input = Util.Base.readLines "bigInput.txt"
                |> Seq.map int
                |> Seq.toList
    let target = 2020

    stopwatch.Stop()
    printfn "Reading and parsing input took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let result = input
                |> allCombinations
                |> List.filter (fun (a,b) -> a+b = target)
                |> List.head

    let mult = mult2Tuple result

    printfn "Multiplication of %i and %i gives %i" (fst result) (snd result) mult
    stopwatch.Stop()
    printfn "Calculation took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let result2 = input
                 |> allCombinations3
                 |> List.filter (fun (a,b,c) -> a+b+c=target)
                 |> List.head

    let mult2 = mult3Tuple result2
    let m1,m2,m3 = result2
    printfn "Multiplication of %i and %i and %i gives %i" m1 m2 m3 mult2
    stopwatch.Stop()
    printfn "Calculation took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()
    0 // return an integer exit code