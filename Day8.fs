module AOC2024.Day8
// This is going to transform the input grid into a list of tower positions 
// grouped by frequency character.  Given towers of frequency A at 1,5 and 6,6
// and a tower of frequency B at 2,2, the result of the transformation will be
// a list of tuples that looks like this:
// ('A' [(1, 5); (6,6)])
// ('B' [(2,2)]
let getTowers grid =
    grid
    |> List.mapi (fun y line ->
        line 
        |> List.indexed 
        |> List.filter (fun (_,c ) -> c <> '.')
        |> List.map (fun (x,c) -> (c, (y, x))))
    |> List.collect id
    |> List.groupBy fst
    |> List.map (fun (c, t) -> 
        (c, t |> List.map(fun (_,(y,x)) -> (y,x))))

// Once we have the towers, we need to generate "pair" combinations such
// that every tower of a frequency is paired with every other tower of that 
// same frequency. We will be lookin for antinodes going in both directions,
// so towers will technically be paired twice, tower 1 with tower 2 and 
// tower 2 with tower 1.
let pairTowers towers = 
    towers 
    |> List.map (fun t1 -> 
        towers |> List.map (fun t2 -> (t1, t2)))
    |> List.collect id
    |> List.filter (fun (t1,t2) -> t1 <> t2)

// Utility function to get dy and dx representing the delta between two points
let getDelta (y1,x1) (y2,x2) = (y1-y2, x1-x2)

// Given a maximum Y (lines in the grid), maximum X (characters in each line),
// and our list of tower pairs, get the next coordinate in line based on the
// delta and filter to only those that are in bounds.
// Given a tower 1 at 5,3 and tower 2 at 3,2, the delta 2,1, so the next point 
// in sequence would be 3-2, 2-1: 1,1
// Going the opposite direction with tower 1 at 3,2 and tower 2 at 5,3, the 
// delta is -2,-1, so the next point in sequence would be 5-(-2),3-(-1), or 7,4.
let getAntiNodes maxY maxX pairs = 
    pairs |> List.map (fun (c,pair) -> (c, pair 
        |> List.map (fun ((t1y, t1x),(t2y, t2x)) -> 
            let (dy, dx) = getDelta (t1y, t1x) (t2y, t2x)
            (t2y - dy, t2x - dx))
        |> List.filter (fun (ay, ax) -> 
            ay >= 0 && ay <= maxY && ax >= 0 && ax <= maxX)))

// For part 2, the antinodes keep going with the same delta between antinodes
// regardless of distance from towers, so instead of just producing one 
// antinode from each pair, we recursively iterate and produce notes until we 
// reach out of bounds on the grid
let getAntiNodesWHarmonic maxY maxX pairs = 
    let rec getNextAntinode maxY maxX (y,x) (dy,dx) result =
        let newY = y-dy
        let newX = x-dx
        if (newY < 0 || newY > maxY || newX < 0 || newX > maxX) then
            result
        else
            let newResult = result |> List.append [(newY,newX)]
            getNextAntinode maxY maxX (newY,newX) (dy,dx) newResult 
    pairs 
    |> List.map (fun (c,pair) -> (c, pair 
        |> List.collect (fun ((t1y, t1x),(t2y, t2x)) -> 
            let (dy, dx) = getDelta (t1y, t1x) (t2y, t2x)
            let initAntinodes = [(t1y,t1x);(t2y,t2x)]
            getNextAntinode maxY maxX (t2y,t2x) (dy,dx) initAntinodes)))

// Use our utility function to produce the grid as a List<List<char>>, then
// return that in a tuple along with the maximum Y and maximum X for the grid
let initializeGrid input = 
    let grid = input |> Utils.getGrid
    let maxY = grid.Length - 1
    let maxX = 
        match grid with
        | [] -> failwithf "Invalid input"
        | _::_ -> grid[0].Length - 1
    (grid |> getTowers, maxY, maxX)

// For part 1, we'll get the towers and find the antinodes using our funcations
// above.  For the result, we don't care about the separate characters, just
// the count of unique antinode positions, so we fold the list down to just
// the coordinates and use distinct to eliminate duplicates (antinodes that are
// valid for multiple frequencies).
let part1 input =
    let towers, maxY, maxX = input |> initializeGrid 
    let antiNodes = towers |> List.map (fun (c,t) -> 
        (c, pairTowers t)) |> getAntiNodes maxY maxX
    let answer = 
        antiNodes 
        |> List.fold (fun acc (_, ans) ->
            acc |> List.append ans) []
        |> List.distinct
        |> List.length
    sprintf "Part 1: %d" answer

// Part 2 is the same, except we use our getAntiNodesWHarmonic function
// to get continued antinodes on a vector until oob is reached
let part2 input =
    let towers, maxY, maxX = input |> initializeGrid 
    let antiNodes = towers |> List.map (fun (c,t) -> 
        (c, pairTowers t)) |> getAntiNodesWHarmonic maxY maxX
    let answer = 
        antiNodes 
        |> List.fold (fun acc (_, ans) ->
            acc |> List.append ans) []
        |> List.distinct
        |> List.length
    sprintf "Part 2: %d" answer

let solve input =
    (input |> part1, input |> part2)

