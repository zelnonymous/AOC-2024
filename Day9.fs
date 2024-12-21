module AOC2024.Day9
    
let validateInput (input:List<string>) =
    match (input |> List.length) with
    | 1 -> input[0].ToCharArray() 
        |> Array.map (fun c -> int64 c - int64 '0') 
        |> Array.toList
    | _ -> failwithf "Invalid input!"

let rec parseInput (id:int64) (layout:List<int64>) isFree (rest:List<int64>) =
    match rest with
    | d::ds ->
        let addition = 
            if isFree then (int64 -1) else id
            |> List.replicate (int d)
        let newLayout = addition |> List.append layout
        let newId = if isFree then id else id + (int64 1)
        parseInput newId newLayout (not isFree) ds
    | [] -> layout

let swap layout idx dest = 
    layout 
    |> List.mapi (fun i p ->
        if i = idx then 
            layout[dest] 
        elif i = dest then 
            layout[idx] 
        else p)

let rec defrag idx (layout:List<int64>) = 
    let dest = layout |> List.findIndex (fun pos -> pos = -1)
    if idx = -1 || idx > layout.Length - 1 then
        layout
    else 
        if int64 layout[idx] = int64 -1 then
            defrag (idx - 1) layout
        elif dest > idx then
            layout 
        else 
            defrag (idx - 1) (swap layout idx dest)


let calcChecksum (defragged:List<int64>) =
    defragged 
    |> List.mapi (fun i v -> 
        let i64 = int64 i
        if v = (int64 -1) then (int64 0) else v * i64)
    |> List.sum
    

let part1 (input:List<int64>) =
    let layout = input |> parseInput (int64 0) [] false
    let dl = layout |> defrag (layout.Length - 1)
    let checksum = calcChecksum dl
    sprintf "Part 1: %d" checksum

let part2 input =
    sprintf "Part 2: "

let solve input =
    let input = validateInput (input |> Seq.toList)
    (input |> part1, input |> part2)
