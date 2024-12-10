module AOC2024.Day7

// Each line in the input is an "Operation" with a desired result and a list
// of numeric operands
type Operation = {
    result: int64
    operands: int64[]
}

// For part 2, we need an operator to act as the "concatenation" operator.
// This operator is || in the puzzle, but that is an existing operator in
// F#, so I opted to use ||| instead.
let (|||) (l:int64) (r:int64) = 
    int64(l.ToString() + r.ToString())

// Parse the input.  THis will produce a list of operations and throw an
// exception if a line does not contain a colon.
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

// Recursive function that returns a bool indicating whether or not the
// supplied operators and operands can be used to produce the desired result.
// operators - A list of operators to use (these should be real binary
// operators)
// remaining - Initially, this will be the full list of operands.  It will be
// the remaining tail on recursive calls
// acc - An accumulator.  This should be initiliazed as 0 (int64)
// result - The desired result for the operation being processed
let rec solvable (operators:List<int64 -> int64 -> int64>) 
    (remaining:List<int64>) (acc:int64) result =
    match remaining with
    | [] -> false
    | _::[] -> acc = result
    | o::os -> 
        (operators |> Seq.tryFind(fun op -> 
            // If this is th efirst operation (ie. the accumulator is 0), we
            // will apply the operator to the head of the list of operands
            // and the head of the remaining tail; otherwise it will be 
            // applied to the accumulator and the head of the remaining tail 
            // instead.  Example: givin an operator (+) and operands 1 2 3 4,
            // this will be 1 + 2 on the first execution, setting the 
            // accumulator to 3 for future iterations.  On the next pass it 
            // will be 3 (that previous accumulator value) + 3 (the next operand)
            let acc' = if acc = int64 0 then op o os.Head else op acc os.Head
            solvable operators os acc' result)).IsSome

// Given the list of operations from the input, check whether each is solvable.
// If it is, we'll get the result, otherwise we'll get 0, and sum these values
// to get the answer.
let trySolve ops operators =
    ops |> Seq.map (fun o ->
        let operands = o.operands |> Array.toList
        let result = solvable operators operands (int64 0) o.result
        if result then o.result else int64 0) |> Seq.sum

// Part 1 and part 2 are idenctical except that part 2 has an additional 
// operator for concatentation, as seen above.
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
