open System
open System.Collections.Generic

type PassportFieldKey = Byr=0 | Iyr=1 | Eyr=2 | Hgt=3 | Hcl=4 | Ecl=5 | Pid=6 | Cid=7

type PassportField = {key:PassportFieldKey; value:string}

type Passport = {fields: PassportField list}

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'T> = Parser of (string list-> ParseResult<'T * string list>)

let parseField (fieldKey:PassportFieldKey) =
    let innerParse (inputArr: string list) =
        match inputArr with
        | [] -> Failure "no more input"
        | kv:: remaining ->
                            if String.IsNullOrWhiteSpace(kv) then
                                Failure "empty input"
                            else
                                let split = kv.Split(":")
                                let key = split.[0]
                                let value = split.[1]
                                if key = fieldKey.ToString().ToLower() then
                                    Success ({key=fieldKey;value=value},remaining)
                                else
                                    let msg = sprintf "Expecting '%s'. Got '%s'" (fieldKey.ToString().ToLower()) key
                                    Failure msg
    Parser innerParse

let parseByr = parseField PassportFieldKey.Byr
let parseIyr = parseField PassportFieldKey.Iyr
let parseEyr = parseField PassportFieldKey.Eyr
let parseHgt = parseField PassportFieldKey.Hgt
let parseHcl = parseField PassportFieldKey.Hcl
let parseEcl = parseField PassportFieldKey.Ecl
let parsePid = parseField PassportFieldKey.Pid
let parseCid = parseField PassportFieldKey.Cid

let run parser input =
    let (Parser innerFn) = parser
    innerFn input

let andThen parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Failure err -> Failure err
        | Success (val1, remaining1) ->
            let result2 = run parser2 remaining1
            match result2 with
            | Failure err -> Failure err
            | Success (val2, remaining2) ->
                let res = {fields=[val1; val2]}
                Success (res,remaining2)
    Parser innerFn

let ( .>>. ) = andThen

let orElse parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Success result -> result1
        | Failure err ->
            let result2 = run parser2 input
            result2

    Parser innerFn

let ( <|> ) = orElse

let choice listOfParsers =
    List.reduce (orElse) listOfParsers

let anyOf listOfPassportFieldKeys =
    listOfPassportFieldKeys
    |> List.map parseField
    |> choice

let enumToList<'a> = (Enum.GetValues(typeof<'a>) :?> ('a [])) |> Array.toList

let anyPassportFieldKey =
    anyOf enumToList<PassportFieldKey>

let parseToEnd parser (input:string list) =
    let rec loop result (toParse:string list) =
        let parsed = run parser toParse
        match parsed with
        | Failure err -> result
        | Success (res, remaining) -> loop {fields=res::result.fields} remaining
    loop {fields=[]} input

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let validateField passportField =
    let key = passportField.key
    let value = passportField.value
    try
        match key with
        | PassportFieldKey.Byr ->
            let v = (int value)
            v>=1920 && v<=2002
        | PassportFieldKey.Iyr ->
            let v = (int value)
            v>=2010 && v<=2020
        | PassportFieldKey.Eyr ->
            let v = (int value)
            v>=2020 && v<=2030
        | PassportFieldKey.Hgt ->
            match value with
            | Regex @"([0-9]+)(cm|in)" [v;unit] ->
                match unit with
                | "cm" ->
                    let vv = int v
                    vv >= 150 && vv <= 193
                | "in" ->
                    let vv = int v
                    vv >= 59 && vv <= 76
                | _ -> false
            | _ -> false
        | PassportFieldKey.Hcl ->
            match value with
            | Regex @"^#[0-9a-f]{6}" []-> true
            | _ -> false
        | PassportFieldKey.Ecl ->
            match value with
            | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
            | _ -> false
        | PassportFieldKey.Pid ->
            let v = int value
            value.Length = 9
        | PassportFieldKey.Cid -> true
        | _ -> false
    with
        | _ -> false

let validatePassport (requiredDict:IDictionary<PassportFieldKey,bool>) passport =
    requiredDict
    |> Seq.fold (fun coll curr ->
        let found = match (Seq.tryFind (fun (f:PassportField) -> f.key = curr.Key) passport.fields) with
                    | Some x -> true
                    | None  -> false
        let res = coll && (found || (not curr.Value))
        res

    ) true

let validatePassport2 (requiredDict:IDictionary<PassportFieldKey,bool>) passport =
    let validFields = passport.fields
                      |> List.filter validateField
    requiredDict
    |> Seq.fold (fun coll curr ->
        let found = match (Seq.tryFind (fun (f:PassportField) -> f.key = curr.Key) validFields) with
                    | Some x -> true
                    | None  -> false
        let res = coll && (found || (not curr.Value))
        res

    ) true

[<EntryPoint>]
let main argv =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let passportLines = Util.Base.readAll "bigInput.txt"
                        |> Util.Base.parse (fun i -> i.Replace("\r\n", " ")) "\r\n\r\n"
                        |> Array.map (fun s -> s.Split(" ")|> Array.toList)
                        |> Array.toList

    stopwatch.Stop()
    printfn "Reading and parsing input took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let passports = passportLines
                 |> List.map (fun line -> parseToEnd anyPassportFieldKey line)

    stopwatch.Stop()
    printfn "Parsing into passports took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let requiredDict = dict[PassportFieldKey.Byr,true;PassportFieldKey.Iyr,true;PassportFieldKey.Eyr,true;PassportFieldKey.Hgt,true;PassportFieldKey.Hcl,true;PassportFieldKey.Ecl,true;PassportFieldKey.Pid,true;PassportFieldKey.Cid,false;]
    let resultList = passports
                     |> List.map (fun pp -> validatePassport requiredDict pp)
    let validCount = resultList
                     |> List.filter id
                     |> List.length
    stopwatch.Stop()
    printfn "The amount of valid passports is %i" validCount
    printfn "Validating passports took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()

    let resultList2 = passports
                      |> List.map (fun pp -> validatePassport2 requiredDict pp)
    let validCount2 = resultList2
                      |> List.filter id
                      |> List.length
    stopwatch.Stop()
    printfn "The amount of valid passports is %i" validCount2
    printfn "Validating passports took %i ms" stopwatch.ElapsedMilliseconds
    stopwatch.Restart()
    0 // return an integer exit code