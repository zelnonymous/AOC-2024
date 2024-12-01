module AOC2024.Day1
open System

// Function to extract digits from a line of text and turn them into
// a list.  Each list should contain two digits, one from each of the
// actual location lists
let extractDigitList line = 
    line
    |> Seq.filter Char.IsDigit
    |> Seq.map (fun c -> int c - int '0')
    |> Seq.toList

// Part 1 solver.  Work in progress.
let part1 input = 
    let extracted = 
        input
        |> Seq.map (fun line -> extractDigitList line)
        |> Seq.filter (fun digits -> digits.Length = 2)
    let firstCol = 
        extracted
        |> Seq.map (fun digits -> digits[0])
        |> Seq.sort
        |> Seq.toList
    let secondCol =
        extracted
        |> Seq.map (fun digits -> digits[1])
        |> Seq.sort
        |> Seq.toList
    let answer = 
        firstCol
        |> List.map (fun digit -> digit)
    sprintf "Part 1: %A" answer

// boilerplate for Part 2.  Will fill this in when I get there.
let part2 input = "Part 2: "

let solve input = (input |> part1, input |> part2)
