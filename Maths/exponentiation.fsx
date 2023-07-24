let rec expn x n = 
    match n with
    | 0. -> 1.
    | _ -> 
        match n > 0. with
        | true ->
            match n % 2. = 0. with
            | true -> 
                let y = expn x (n/2.)
                y * y
            | false -> 
                x * (expn x (n-1.))
        | false -> 
            1./(expn x -n)

expn 2. 4.