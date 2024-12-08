module AOC2024.Day6

// The guard faces (and moves) in cardinal directions.
type Direction =
    | Up
    | Left
    | Right
    | Down

// This is used during initiaization to determine which direction the gaurd
// is facing based on the direction they are facing.  This may always be up,
// but it wasn't specified in the puzzle details, so added detection anyway
let getDirection c =
    match c with
    | '^' -> Some Up
    | '<' -> Some Left
    | '>' -> Some Right
    | 'v' -> Some Down
    | _ -> None

// Recursive function to get the guard's starting position and direction 
let rec getGaurdStart y rest =
    match rest with
    | l::ls ->
        let x = l |> List.tryFindIndex (fun c -> 
            match (getDirection c) with 
            | Some _ -> true
            | None ->  false)
        match x with
        | Some x -> 
            let d = getDirection l[x]
            match d with
            | Some d -> (y, x, d)
            | None -> failwithf "Unable to determine direction!"
        | None -> getGaurdStart (y+1) ls
    | [] -> failwithf "Unable to find gaurd!"

// Given a position and a direction, get the next position the gaurd would be in
let planPosition (y, x, d) =
    match d with
    | Up -> (y-1, x)
    | Down -> (y+1, x)
    | Left -> (y, x-1)
    | Right -> (y, x+1)

// Determine the real next position the gaurd will move to and the direction he
// will be facing based on obstacle encounters and turning.  This could happen
// multiple times before finding a valid position in this scenario:
// ....
// ..>#
// ..#.
let rec getNextPos (grid:List<List<char>>) y x d =
    let py, px = planPosition (y, x, d)
    match grid.[py].[px] with
    | '#' ->
        match d with
        | Up -> getNextPos grid y x Right
        | Right -> getNextPos grid y x Down
        | Down -> getNextPos grid y x Left
        | Left -> getNextPos grid y x Up
    | _ -> (py, px, d)

// Walk the gaurd until either a loop is detected or they are out of bounds
let rec walk walked (y, x, d) (grid:List<List<char>>) =
    match grid with
    | [] -> 
        // Empty grid.  This should not happen
        (walked, false)
    | _::_ -> 
        // If the current position and direction are present in the walked
        // list, the gaurd walked this path in this direction before and is in 
        //a loop.
        let looped = walked |> List.tryFind (fun (py, px, pd) ->
            py = y && px = x && pd = d)
        match looped with
        | Some _ -> 
            // Desribe the loop we detected and return the walked position list
            // as of the time of detection and 'true' for looped
            printfn "Loop detected at %d %d" y x
            (walked, true)
        | None -> 
            // We are not in a loop. Add the gaurd's current position and
            // direction to the walked list
            let nw = List.append walked [(y,x,d)]
            // Get the next planned position based on the current position and
            // direction.  If it is out of bounds, the gaurd will leave the
            // grid this move
            let npy, npx = planPosition (y, x, d)
            let oob =
                npy < 0 || npy > grid.Length - 1 ||
                npx < 0 || npx > grid[0].Length - 1
            match oob with
            | true ->  (nw, false)
            | false ->
                // The gaurd remains in bounds, but may be facing an obstacle
                // or corner.  Find the next position and walk there.
                let (ny, nx, nd) = getNextPos grid y x d
                walk nw (ny, nx, nd) grid

// Initalization function to get the grid as a List<List<char>>, find the 
// gaurd starting parameters. Returns the grid and the gaurd details.
let initialize input = 
    let grid = input |> Utils.getGrid
    let gaurd = getGaurdStart 0 grid
    (grid, gaurd)

// Given the grid and a set of coordinates, return a new grid with the
// an obstacle inserted at the specified position
let insertObstacle grid y x =
    grid
    |> List.mapi (fun y' l -> 
        l |> List.mapi (fun x' c -> if x' = x && y' = y then '#' else c))

// For part one, we shouldn't be in a loop, so we ignore that.
// We just walk the gaurd until it's out of bounds, then count the steps to
// get there.
let part1 input =
    let (grid, gaurd) = initialize input
    let (walked, _) = walk [] gaurd grid
    let answer = 
        walked 
        |> List.map (fun (x,y,_) -> (x,y)) 
        |> Seq.distinct
        |> Seq.length
    sprintf "Part 1: %d" answer

// For part 2, we start out the same and based on the positions the gaurd
// walked in part 1, we try inserting an obstacle at each point along the path
// and test to see if he is in a loop.  We only care to test each coordinate
// once, so we filter this to distinct positions
let part2 input =
    let (grid, gaurd) = initialize input
    let (walked, _) = walk [] gaurd grid
    let (gx, gy, _) = gaurd
    let unique = 
        walked 
        |> List.filter (fun (y,x,_) -> not (x = gx && y = gy))
        |> Seq.distinctBy (fun (y,x,_) -> (y,x))
        |> Seq.toList
    let rec testObstacle idx grid mutations gaurd prev (result:List<bool>) =
        match mutations with
        | (y,x,d)::ms ->
            printfn "Obstacle at %d %d (Mutation #%d)" y x idx
            let mutated = insertObstacle grid y x
            match prev with
            | None -> 
                let (_,looped) = walk [] gaurd mutated
                System.Console.ReadLine |> ignore
                let newResult = List.append result [looped]
                testObstacle (idx+1) grid ms gaurd (Some gaurd) newResult
            | Some (py,px,pd) ->
                let (_,looped) = walk [] (py,px,pd) mutated
                let newResult = List.append result [looped]
                testObstacle (idx+1) grid ms gaurd (Some (y,x,d)) newResult
        | [] -> result
    printfn "Testing %d mutations..." (unique |> List.length)
    let mutations = 
        testObstacle 1 grid unique gaurd None []
        |> List.filter (fun looped -> looped)
    
    let answer =
        mutations
        |> List.length
    sprintf "Part 2: %d" answer

let solve input =
    (input |> part1, input |> part2)
