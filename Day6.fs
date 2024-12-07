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

// Recursive function to get the guards starting position and direction 
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

// Once the gaurd's position and direction are detected, we replace them with
// a . like other non-occupied spaces. In hindsight, this is probably not
// necessary since we only explicitly check for walls when validating moves.
let removeGaurdChar grid (gy, gx, _) =
    grid
    |> List.mapi (fun y l ->
       l |> List.mapi (fun x c ->
        if y = gy && x = gx then '.' else c))


// Given a current position and direction, get the next position
let getNextPos (y, x, d) =
    match d with
    | Up -> (y-1, x)
    | Down -> (y+1, x)
    | Left -> (y, x-1)
    | Right -> (y, x+1)

// Determine the direction the gaurd will be facing for the next move.
// If they would move to a position occupied by a wall, turn right
let getNextDirection (grid:List<List<char>>) y x d =
    match grid.[y].[x] with
    | '#' ->
        match d with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up
    | _ -> d

// Walk the gaurd until either a loop is detected or they are out of bounds
let rec walk walked (y, x, d) (grid:List<List<char>>) =
    match grid with
    | _::_ -> 
        // If the current position and direction are present in the walked
        // list, we walked this path before and are in a loop.
        let looped = walked |> List.tryFind (fun (py, px, pd) ->
            py = y && px = x && pd = d)
        match looped with
        | Some _ -> 
            // Desribe the loop we detected and return the walked position list
            // as of the time of detection and 'true' for looped
            let descript = 
                match d with
                | Up -> "Up"
                | Right -> "Right"
                | Down -> "Down"
                | Left -> "Left"
            printfn "Loop detected at %d %d (Direction %s)" y x descript
            (walked, true)
        | None -> 
            // We are not in a loop. Add our current position and direction to
            // the walked list
            let nw = List.append walked [(y,x,d)]
            // Get the next planned position based on our current position and
            // direction.  This may be a wall or out of bounds, so we'll need to 
            // do some checks.
            let npy, npx = getNextPos (y, x, d)
            // Check for out of bounds first
            let oob =
                npy < 0 || npy > grid.Length - 1 ||
                npx < 0 || npx > grid[0].Length - 1
            match oob with
            | true ->  (nw, false)
            | false ->
                // The gaurd is not out of bounds.  Determine if we will walk
                // to the planned space or turn becuase of an obstacle
                let nd = getNextDirection grid npy npx d
                // Get the next position again in case we turned and are
                // walking in a different direction now
                let ny, nx = getNextPos (y, x, nd)
                walk nw (ny, nx, nd) grid
    | [] -> 
        // Empty grid.  This should not happen
        (walked, false)

// Initalization function to get the grid as a List<List<char>>, find the 
// gaurd starting parameters, and remove him from the grid. Returns the
// 'cleaned' grid and the gaurd details.
let initialize input = 
    let grid = input |> Utils.getGrid
    let gaurd = getGaurdStart 0 grid
    let cleaned = removeGaurdChar grid gaurd
    (cleaned, gaurd)

// Given the grid and a set of coordinates, return a new grid with the
// an obstacle inserted at the specified position
let insertObstacle grid y x =
    grid
    |> List.mapi (fun y' l -> 
        l |> List.mapi (fun x' c ->
            if x' = x && y' = y then '#' else c))

// For part one, we shouldn't be in a loop, so we ignore that.
// We just walk the gaurd until it's out of bounds, then count the steps to
// get there.
let part1 input =
    let (grid, gaurd) = initialize input
    let (walked, _) = walk [] gaurd grid
    let answer = 
        walked 
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
    let unique = 
        walked 
        |> List.map (fun (x,y,_) -> (x,y)) 
        |> Seq.distinct 
        |> Seq.toList
    printfn "Testing %d mutations..." (unique |> List.length)
    let answer =
        unique
        |> List.mapi (fun i (y,x) ->
            let mutated = insertObstacle grid y x
            let (_, looped) = walk [] gaurd mutated
            if looped then
                printfn "Obstacle at %d %d (Mutation #%d)" y x i
                looped
            else
                looped)
        |> List.filter (fun l -> l)
        |> List.length
    sprintf "Part 2: %d" answer

let solve input =
    (input |> part1, input |> part2)
