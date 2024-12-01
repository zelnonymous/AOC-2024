open System.IO

let inputFile = "day1.txt"
let cwd = Directory.GetCurrentDirectory()
let input = Path.Combine(cwd, inputFile) |> File.ReadLines


