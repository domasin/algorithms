let rec pal acc (s:string) =
    match s.Length with
    | 0 | 1 -> acc
    | l -> 
        match s[0] = s[l-1] with
        | false -> false
        | _ -> pal true s[1..l-2]