module AOC2024.Day5

// Given the puzzle input, this will parse the rules section.
// Each rule is represented by a tuple of strings with the first item
// representing the left hand side of the rule and the second representing the 
// right. Once the parser encounters a line that does not contain "|", it 
// returns a tuple containing the rules tuple and the tail of the input
// for further processing
let rec parseRules rules (input:List<string>) =
    match input with
    | r::rs -> 
        let parts = r.Split("|")
        match parts.Length with
        | 2 -> 
            let newRules = rules |> List.append [(parts[0], parts[1])]
            parseRules newRules rs
        | _ -> (rules, rs)
    | [] -> (rules, [])

// Takes the list of rules and a list of pages. Returns the list of 
// violated rules (or an empty list if none)
let getViolatedRules rules pages =
    rules
    |> List.filter (fun (l,r) ->
        let lpage = pages |> List.tryFindIndex (fun p -> p = l)
        let rpage = pages |> List.tryFindIndex (fun p -> p = r)
        match lpage, rpage with
        | None, _ 
        | _, None -> false
        | lp, rp -> lp >= rp)

// Takes the list of rules, the remaining raw input (comma separated pages),
// Whether we are looking for "correct" entries (true if we want the page lists
// that contain no rules violations or false otherwise), and a list (initially
// empty and will populate with matching page lists on recursion).
// Splits each page list on the commas and returns the filtered list of page
// lists
let rec getPageLists rules (input:List<string>) correct list =
    match input with
    | p::ps -> 
        let plist = p.Split(",") |> Array.toList
        let violations = getViolatedRules rules plist
        match violations, correct with
        | _::_, true | [], false -> getPageLists rules ps correct list 
        | [], true | _::_, false ->
            let newList = list |> List.append [plist]
            getPageLists rules ps correct newList 
    | [] -> list

// This is for part two. Given a single rule violation, swap the positions of
// the offending rules.
let applySwap newList (l, r) =
    let lidx = newList |> List.tryFindIndex (fun p -> p = l)
    let ridx = newList |> List.tryFindIndex (fun p -> p = r)
    match lidx, ridx with
    | None, _
    | _, None-> failwithf "A violated rule doesn't match the list!"
    | Some li, Some ri -> 
        let leftPage = newList[li]
        let rightPage = newList[ri]
        newList |> List.mapi (fun i p ->
            if i = ri then leftPage elif i = li then rightPage else p)


// Get the middle page number from each page list as an int
// (A list with an even number of elements will throw here)
let getMidPages (lists:List<List<string>>) =
    lists
    |> List.map (fun l -> 
        match (l.Length % 2) with
        | 0 -> failwithf "Even number of elements in a list: %A" l
        | _ -> l[l.Length/2])
    |> List.map int

// For part 1, we get parse the rules and the pages with no rule violations
// from the raw input, then we sum the middle pages to get the answer
let part1 input =
    let (rules, rest) = parseRules [] (input |> Seq.toList)
    let correctLists = getPageLists rules rest true []
    let midPages = getMidPages correctLists
    let answer = midPages |> List.sum
    sprintf "Part 1: %d" answer

// For part 2, I'm certain there is a more efficient way to do this.
// We start off similar to part one, but after getting the
// rules list, we get the page lists that do violate at least one rule
let part2 input =
    let (rules, rest) = parseRules [] (input |> Seq.toList)
    let incorrectLists = getPageLists rules rest false []
    // This gets called recursively until the order of the page list meets all
    // the rules. On each cycle, we call get violations.  If there are not any,
    // this list is ordered correctly and is returned.  If there are, we get
    // the first violation from the list, swap the left and right element
    // described in the rule, and cycle again so that if the swap addressed
    // any other violations or created any new ones, we can deal with those.
    // This continues until there are no violated rules
    let rec fixList newList =
        let violations = getViolatedRules rules newList
        match violations with
        | [] -> newList
        | v::_ -> 
            let corrected = applySwap newList v
            fixList corrected
    let fixedLists = incorrectLists |> List.map (fun l -> fixList l)
    // Same as part 1.  Get the mid pages from the corrected lists and sum them
    let midPages = getMidPages fixedLists
    let answer = midPages |> List.sum
    sprintf "Part 2: %d" answer

let solve input =
    (input |> part1, input |> part2)
