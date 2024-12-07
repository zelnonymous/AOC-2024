module AOC2024.Utils
open System
open System.IO
open System.Text

// Function to read lines of text out of a file as a sequence
let ReadInput fname = File.ReadLines fname

// Function to extract a list of integers integers separated by non-digit 
// characters from a string and turn them into a list.  
let parseDigits line =
    let buffer = StringBuilder()
    let result = ResizeArray<int>()
    let flushBuffer () =
        if buffer.Length > 0 then
            result.Add(int (buffer.ToString()))
            buffer.Clear() |> ignore
    line |> Seq.iter (fun c ->
        if Char.IsDigit c then 
            buffer.Append(c) |> ignore
        else 
            flushBuffer())
    flushBuffer()
    result |> Seq.toList


// $ead the entire input into memory to form the grid as a two
// dimensional list of chars
let getGrid (input:seq<string>) = 
    input 
    |> Seq.toList 
    |> List.map (fun s -> s.ToCharArray() |> Array.toList) 
