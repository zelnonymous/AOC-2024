module AOC2024.Day3
open System.Text.RegularExpressions

// Don't judge me for my terrible lexer / parser combo!  
// It's dirty, but I am limited on time today and, given the limited 
// token type list required, it gets the job done.
// useEn - A flag to indicate whether we are using the "enable" grammar (part 2)
// enabled - A flag for whether we are currently in the enabled state. This
// is meaningless if useEn is false
// total - the running total from processed operations
// chars - Initially this will be a full line of text from the input, but on 
// recursive calls, will be the remaining tail of that string to be processed
let rec parseLine useEn enabled total chars =
    match chars with
    | c::cs -> 
        match useEn, enabled, c, cs with
        | true, _, 'd', ['o';'(';')'] -> 
            // If the current character is 'd' and the trailing characters
            // are EXACTLY o(), then this is a do() call at the end of the line,
            // so we should indicate that we are now in enabled state and return
            // the total
            (true, total)
        | true, _, 'd', 'o'::'('::')'::rest -> 
            // Same as above, but there is still tail to process, so instead of
            // returning, we recurse on the remaining tail with 'enabled' set 
            // to true
            parseLine useEn true total rest
        | true, _, 'd', ['o';'n';''';'t'] -> 
            // If the current character is 'd' and the trailing characters are
            // EXACTLY on't(), then this is a don't() call at the end of the
            // line, so we should indicate that we are no longer in enabled state
            // and return the total
            (false, total)
        | true, _, 'd', 'o'::'n'::'''::'t'::'('::')'::rest -> 
            // Same as above, but there is still tail to process, so instead of
            // returning, we recurse on the remaining tail with 'enabled set
            // to false
            parseLine useEn false total rest
        | true, true, 'm', 'u'::'l'::'('::rest | 
            false, _, 'm', 'u'::'l'::'('::rest -> 
            // If either useEn is true AND we are enabled state, or useEn is
            // false and we are in any state, this is a mutiply operation we 
            // should process
            let tstr = rest |> List.map string |> List.reduce (+)
            // Starting from the beginning of the tail, this is only a valid
            // operation of there are 1 to 3 digits, followed immediately by
            // a comma, and then by another 1 to 3 digits with a closing paren
            // at the end
            let mcheck = Regex.Match (tstr, "^([0-9]{1,3}),([0-9]{1,3})\)")
            match mcheck.Success, mcheck.Groups.Count with
            | true, 3 -> 
                // This is a valid operation.  Extract the numbers from the
                // capture groups, convert to ints, get the product and sum
                // with the current total, then continue to process the
                // tail
                let oper1 = int (mcheck.Groups[1].Value)
                let oper2 = int (mcheck.Groups[2].Value)
                parseLine useEn enabled (oper1 * oper2 + total) rest
            | _, _ -> 
                // Not a valid operation. Continue processing the remaining tail.
                parseLine useEn enabled total rest
        | _ -> 
            // No operation detected.  Continue processing the remaining tail.
            parseLine useEn enabled total cs
    | [] -> 
        // We reached the end of the line.  Return the current enabled state and
        // to use for summing and / or processing more lines.
        (enabled, total)

// For part 1, we are just running the parser with useEn turned off and 
// summing the totals to get the grand total.
let part1 input = 
    let total =
        input
        |> Seq.map (fun line ->
            let (_, result) = 
                parseLine false true 0 (Seq.toList line)
            result)
        |> Seq.sum
    sprintf "Part 1: %d" total

// For part 2, we need to set useEn to true, but we also need to carry over the
// last enable setting as we move from line to line
let part2 input = 
    let asList = input |> Seq.toList
    // Initially, we we call this with enabled set true (the starting state),
    // total as 0, and the entire input.  As each line of input is processed,
    // we will call it recursively, carrying over the enabled state and total
    // from the line that was just handled for processing the next line.
    let rec parseInput enabled total lines =
        match lines with
        | l::ls -> 
            let line = Seq.toList l
            let (en, res) = parseLine true enabled total line
            parseInput en res ls
        | [] -> total
    let total = parseInput true 0 asList
    sprintf "Part 2: %d" total

let solve input = 
    (input |> part1, input |> part2)
