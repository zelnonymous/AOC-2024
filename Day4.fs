module AOC2024.Day4
open AOC2024.Utils

// An enum for directions in which we can cast a ray to get a word
// from the wordsearch, including the diagonals
type Direction =
    | Up
    | UpLeft
    | Left
    | DownLeft
    | Down
    | DownRight
    | Right
    | UpRight

// A recursive function to retrieve a word from the grid.  If at any point
// a character would be requested from outside the bounds of the grid, the
// current buffer is returned as the found word
// grid - List<List<char>>, where the outer list is from the lines of
// text top to bottom and the inner is the list of chars in that line
// buffer - A string buffer for capturing the word. Initially this should be
// an empty string and will contain the full word at the end of recursion
// (y, x) - A tuple of coordinates to inspect.  Initially, this will be the 
// origin from which the word is being built, but on recursive calls will
// be the position of the next char in the sequence
// d - The direction to move while collecting characters
// len - The length of the word to extract
let rec castRay (grid:List<List<char>>) (buffer:string) (y, x) (d:Direction) len = 
    match grid, d with
    | [], _ -> buffer
    | _::_, _ -> 
        // Out of bounds detection
        let oob = 
            y < 0 || y > grid.Length - 1 || 
            x < 0 || x > grid[0].Length - 1
        match oob, buffer.Length + 1 = len with
        | true, _ -> buffer
        | false, true -> buffer + (string(grid[y][x]))
        | false, false -> 
            // Get the x and y coordinates for the next char based on
            // direction
            let newX = 
                match d with
                | Left | UpLeft | DownLeft -> x - 1
                | DownRight | Right | UpRight -> x + 1
                | _ -> x
            let newY =
                match d with
                | UpLeft | Up | UpRight -> y - 1
                | DownLeft | Down | DownRight -> y + 1
                | _ -> y
            let newStr = buffer + string(grid[y][x])
            castRay grid newStr (newY, newX) d len

// For part 1, filter the grid to the coordinates of every instance of
// 'X', then cast a 4 character ray from that position in each direction.
// Each ray that matches the word "XMAS" counts as a match.
let part1 input =
    let grid = input |> getGrid
    let cnts = grid |> List.mapi (fun y line ->
        line
        |> List.mapi (fun x c -> (x, c))
        |> List.filter (fun (_, c) -> c = 'X')
        |> List.fold (fun a (x, _) ->
            let words = 
                [castRay grid "" (y, x) Up 4;
                castRay grid "" (y, x) UpLeft 4;
                castRay grid "" (y, x) Left 4;
                castRay grid "" (y, x) DownLeft 4;
                castRay grid "" (y, x) Down 4;
                castRay grid "" (y, x) DownRight 4;
                castRay grid "" (y, x) Right 4;
                castRay grid "" (y, x) UpRight 4]
            a + (words |> List.filter (fun w -> w = "XMAS") |> List.length)
        ) 0)
    sprintf "Part 1: %d" (List.sum cnts)

// For part 2, I took a similar approach, but I filter the grid to 
// occurrences of 'M' instead, then cast one ray up to 3 chars in each
// of the diagonal directions as well as the corresponding ray that would
// form one of the 'X-MAS' patterns.  For example, if starting from an 'M',
// We match 'MAS' down and to the right, then we want to also cast a ray of
// three characters at x+2 down and to the left, and a another up and to the
// right from y+2 (I do discrete matches for each of these patterns).
//
// For diagnostic purposes, I include a string descriptor of the ray directions.
// Example: "DRDL" means down to the right and down to the left

let part2 input =
    let grid = input |> getGrid
    let cnts = grid |> List.mapi (fun y line ->
        line
        |> List.mapi (fun x c -> (x, c))
        |> List.filter (fun (_, c) -> c = 'M')
        |> List.fold (fun a (x, _) ->
            let wordPairs = 
                // M.M
                // .A.
                // S.S
                [(castRay grid "" (y, x) DownRight 3, 
                castRay grid "" (y, x+2) DownLeft 3,
                "DRDL");
                
                // S.S
                // .A.
                // M.M
                (castRay grid "" (y, x) UpRight 3,
                castRay grid "" (y, x+2) UpLeft 3,
                "URUL");


                // M.S
                // .A.
                // M.S
                (castRay grid "" (y, x) DownRight 3,
                castRay grid "" (y+2, x) UpRight 3,
                "DRUR");

                // S.M
                // .A.
                // S.M
                (castRay grid "" (y, x) DownLeft 3,
                castRay grid "" (y+2, x) UpLeft 3,
                "DLUL")]

            // If both rays in a pair match 'MAS', we found an 'X-MAS'
            let matches = wordPairs |> List.filter (fun (l1, l2, _) ->
                l1 = "MAS" && l2 = "MAS")
            a + (matches |> List.length)
        ) 0)
    printfn "%A" cnts
    sprintf "Part 2: %d" (List.sum cnts)

let solve input =
    (input |> part1, input |> part2)
