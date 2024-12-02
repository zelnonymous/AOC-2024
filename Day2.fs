module AOC2024.Day2
open AOC2024.Utils

// Given an optional boolean flag i to indicate
// whether the sequence is incrementing (Some true), decrementing (Some false)
// or not yet known based on the elements inspected so far (None) as well as
// a current value l and a previous value p, check whether the values make
// the report "Unsafe" based on inequality between the values, an appropriate
// difference value, and incrementing or decrementing if known.
let stillSafe i l p = 
    let diff = abs (p - l)
    match i with
    | Some i ->
        l <> p && (i && p < l || not i && p > l) &&
        diff >= 1 && diff <= 3
    | None ->
        l <> p && diff >= 1 && diff <= 3

// Check the safety of a report
// prev - an Option<int> containing the last inspected value if there is one
// inc - an Option<bool> to indicate if we are incrementing, decrementing,
// or unknown
// levels - the current list from the report (or the tail in recursive calls)
let rec checkSafety prev inc levels =
    match (prev, inc, levels) with
    | (None, _, l::ls) -> 
        // We do not know the previous element (ie. we are on the first element)
        // recurse into the tail with the current element as previous
        checkSafety (Some l) inc ls
    | (Some p, None, l::ls) -> 
        // We do not yet know if we are incrementing or decrementing (ie. we are
        // on the second element).  Check if we are still safe with the known
        // values, and if we are, recurse into the tail, with inc set based on
        // whether l is greater than p
        let isStillSafe = stillSafe None l p
        match isStillSafe with
        | true -> checkSafety (Some l) (Some (l > p)) ls
        | false -> false
    | (Some p, Some i, l::ls) -> 
        // Processing the rest of the list.  Check if we are still safe.  If
        // not, return false to indicate the report was unsafe.  If so, 
        // recurse into the remaining tail and continue checking.  
        let isStillSafe = stillSafe (Some i) l p 
        match isStillSafe with
        | true -> checkSafety (Some l) inc ls
        | false -> false
    | (_, _, []) -> 
        // If the tail is empty, we have reached the end of 
        // the list and the report is safe.
        true

// Map the input into integers using the parseDigits utility function from
// Day 1, then iterate the sequence to test each report.  Filter out
// "unsafe" reports and count the length of the safe list
let part1 input = 
    let reports = input |> Seq.map parseDigits
    let safeCount = 
        reports
        |> Seq.filter (fun r -> checkSafety None None r)
        |> Seq.length
    sprintf "Part 1: %d" safeCount 

// Part 2 was a little trickier, but we start the same way.
let part2 input =
    let reports = input |> Seq.map parseDigits
    let safeCount = 
        reports
        |> Seq.filter (fun r -> 
            // If the report is already safe, then we count it.
            // if not, we iterate over it and remove each element, then
            // retest for safety.  If any removal makes the report safe,
            // then List.exists will return true and it will pass the filter.
            // Otherwise, no removals make this report safe and it won't be
            // counted.
            let isSafe = checkSafety None None r
            match isSafe with
            | true -> true
            | false ->
                r
                |> List.mapi (fun idx _ -> List.removeAt idx r)
                |> List.exists (checkSafety None None))
        |> Seq.length
    sprintf "Part 2: %d" safeCount

// Solve part 1 and part 2
let solve input = 
    (input |> part1, input |> part2)
