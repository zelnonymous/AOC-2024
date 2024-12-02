module AOC2024.Day1
open AOC2024.Utils
open System
open System.Text

// Parse the input using parseDigits and only keep valid entries
// (lines that contain two integers)
let parseInput input =
    input
    |> Seq.map (fun line -> parseDigits line)
    |> Seq.filter (fun digits -> digits.Length = 2)

// Given a sequence of integer lists (as derived by parseInput),
// extract just the column expressed by idx.  To get the first list,
// for example, use idx 0.  Using parseInput first ensures that sequence
// entries all contain exactly two items, so 0 and 1 are safe here
let extractList (lst:seq<List<int>>) = 
    let extractAtIdx idx = 
        lst
        |> Seq.map (fun digits -> digits[idx])
        |> Seq.sort
        |> Seq.toList
    extractAtIdx

// Solver for part 1.  Extract the left and right list, zip them,
// map over and get the difference between the values, and finally sum
// those differences.
let part1 input = 
    let extracted = parseInput input
    let left = extractList extracted 0
    let right = extractList extracted 1
    let answer = 
        List.zip left right 
        |> List.map (fun (left, right) -> abs(left - right))
        |> List.sum
    sprintf "Part 1: %d" answer

// Solver for part 2. Start the same as before, but this time
// we will be mapping over the left list, counting the number of
// matching location IDs in the right list, taking the product of
// that value and the location ID, and finally summing those up.
let part2 input = 
    let extracted = parseInput input
    let left = extractList extracted 0
    let right = extractList extracted 1
    // Helper to get the count of occurrences in the right list
    // for a given location ID
    let countOccurrences locID = 
        right 
        |> List.filter (fun rightID -> rightID = locID)
        |> List.length
    let answer =
        left
        |> List.map (fun leftID ->
            let occurrences = countOccurrences leftID
            leftID * occurrences)
        |> List.sum
    sprintf "Part 2: %d" answer

// Solve part 1 and part 2
let solve input = 
    (input |> part1, input |> part2)
