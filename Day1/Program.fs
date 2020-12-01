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
let rec combineLists list1 list2 =
    let rec loop res l1 l2 =
        match l1,l2 with
        | [],[] -> res
        | l1head :: l1tail, l2head :: l2tail -> loop (res @ combine l1head l2) l1tail l2tail
        | _ -> failwith "error not same length"
    loop [] list1 list2

[<EntryPoint>]
let main argv =
    let input = Util.Base.readLines "bigInput.txt"
                |> Seq.map (fun a -> a() |> int)

    let target = 2020

    let result = input
                |> Seq.toList
                |> combineLists (Seq.toList input)
                |> List.filter (fun (a,b) -> a+b = target)
                |> List.head

    let mult = (fst result) * (snd result)

    printfn "Multiplication of %i and %i gives %i" (fst result) (snd result) mult
    0 // return an integer exit code