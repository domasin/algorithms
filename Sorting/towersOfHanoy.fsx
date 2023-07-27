let move start target (pegs: int list list) =
    printfn "move %i %i %A" start target pegs
    let disk = pegs[start] |> List.head

    pegs
    |> List.mapi (fun i p ->
        match i with
        | i when i = start -> p |> List.removeAt 0
        | i when i = target ->
            match p with
            | [] -> p |> List.append [ disk ]
            | hd :: tl when hd > disk -> p |> List.append [ disk ]
            | _ -> failwith "lower disk exists"
        | _ -> p)

let rec solve n start target (pegs: int list list) = 
    match n with
    | 1 -> pegs |> move start target
    | _ -> 
        let spare = 
            match start + target with
            | 1 -> 2
            | 2 -> 1
            | _ -> 0
        pegs
        |> solve (n-1) start spare
        |> move start target
        |> solve (n-1) spare target

[[1;2;3;4;5];[];[]] 
|> solve 5 0 1

[[1;2;3;4;5];[];[]] 
|> move 0 2
// |> move 0 2 // invalid move
|> move 0 1
|> move 2 1

// let move numDisks start target (pegs:int list list) =
//     let pegsStart = (start,pegs[start])
//     let pegsTarget = (target,pegs[target])
//     let other = 
//         match start + target with
//         | 3 -> 3
//         | 4 -> 2
//         | _ -> 1
//     let pegsOther = (other, pegs[other])
//     match numDisks with
//     | 1 -> 
        