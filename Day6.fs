module AOC2024.Day6

type Direction =
    | Up
    | Left
    | Right
    | Down

let getDirection c =
    match c with
    | '^' -> Some Up
    | '<' -> Some Left
    | '>' -> Some Right
    | 'v' -> Some Down
    | _ -> None

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

let removeGaurdChar grid (gy, gx, _) =
    grid
    |> List.mapi (fun y l ->
       l |> List.mapi (fun x c ->
        if y = gy && x = gx then '.' else c))

let getNextPos (y, x, d) =
    match d with
    | Up -> (y-1, x)
    | Down -> (y+1, x)
    | Left -> (y, x-1)
    | Right -> (y, x+1)

let getNextDirection (grid:List<List<char>>) y x d =
    match grid.[y].[x] with
    | '#' ->
        match d with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up
    | _ -> d

let rec walk walked (y, x, d) (grid:List<List<char>>) =
    match grid with
    | _::_ -> 
        let nw = List.append walked [(y,x)]
        let npy, npx = getNextPos (y, x, d)
        let oob =
            npy < 0 || npy > grid.Length - 1 ||
            npx < 0 || npx > grid[0].Length - 1
        match oob with
        | true ->  nw
        | false ->
            let nd = getNextDirection grid npy npx d
            let ny, nx = getNextPos (y, x, nd)
            walk nw (ny, nx, nd) grid
    | [] -> walked

let part1 input =
    let grid = input |> Utils.getGrid
    let gaurd = getGaurdStart 0 grid
    let cleaned = removeGaurdChar grid gaurd
    let walked = walk [] gaurd cleaned
    let answer = 
        walked 
        |> Seq.distinct
        |> Seq.length
    sprintf "Part 1: %d" answer

let part2 input =
    sprintf "Part 2: "

let solve input =
    (input |> part1, input |> part2)
