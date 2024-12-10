module AOC2024.Day7

type Operation = {
    result: int64
    operands: int64[]
}

let (|||) (l:int64) (r:int64) = 
    int64(l.ToString() + r.ToString())
let parseInput (input:seq<string>) =
    input
    |> Seq.map (fun line ->
        let parts = line.Split(':') |> Array.toList
        match parts with
        | [r;o] -> {
            result = int64 r;
            operands = 
                o.Split() 
                |> Array.filter (fun o -> o.Trim() <> "")
                |> Array.map int64}
        | _ -> failwithf "Unexpected input: %s" line)

let rec solvable (operators:List<int64 -> int64 -> int64>) 
    (remaining:List<int64>) (acc:int64) result =
    match remaining with
    | [] -> false
    | _::[] -> acc = result
    | o::os -> 
        (operators |> Seq.tryFind(fun op -> 
            let acc' = if acc = int64 0 then op o os.Head else op acc os.Head
            solvable operators os acc' result)).IsSome

let trySolve ops operators =
    ops |> Seq.map (fun o ->
        let operands = o.operands |> Array.toList
        let result = solvable operators operands (int64 0) o.result
        if result then o.result else int64 0) |> Seq.sum

let part1 input =
    let ops = parseInput input
    let success = trySolve ops [(+); (*)]
    sprintf "Part 1: %d" success

let part2 input =
    let ops = parseInput input
    let success = trySolve ops [(+); (*); (|||)]
    sprintf "Part 2: %d" success

let solve input = 
    (input |> part1, input |> part2)
