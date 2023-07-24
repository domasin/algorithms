let rec fibNaive n = 
    match n with
    | 0 | 1 -> n
    | _ -> (fibNaive (n-1)) + (fibNaive (n-2))

let fibTailRecursive n = 

    let rec fibAux acc1 acc2 n = 
        printfn "fibAux %A %A %A" acc1 acc2 n
        match n with
        | 0 -> 
            printfn "= %i" acc1
            acc1
        | _ -> fibAux (acc1+acc2) acc1 (n-1)

    fibAux 0 1 n

[0..7]
|> List.map fibTailRecursive