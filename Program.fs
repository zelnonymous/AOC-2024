module AOC2024.AOC
open System.IO

// Make sure all of the DayX modules are included in the fsproj file!

// Set this to the solver for the day you want to solve
// TODO: Select day from an argument or make a TUI menu to select a day
let solver = Day3.solve

// Set this to the input file you want to solve for
// TODO: Select day from an argument or make a TUI menu to select a day
let inputFile = "Day3.txt"

let cwd = Directory.GetCurrentDirectory()

// Read sequence of lines from file
let input = 
    Path.Combine(cwd, "inputs", inputFile) 
    |> Utils.ReadInput
    
// Solve both parts.  The solver produces a tuple of strings
let (part1, part2) = solver input

// Print the solution
printfn "%s" part1
printfn "%s" part2
