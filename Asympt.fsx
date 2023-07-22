let quadratic n = 
    (n |> float) ** 2. |> int

let quadraticTime n = 
    let quad = (n |> float) ** 2. |> int
    System.Threading.Thread.Sleep(quad)
    // printfn "%i" n
    quad

let logaritimicTime n = 
    let quad = System.Math.Log2(n) |> int
    System.Threading.Thread.Sleep(quad)
    // printfn "%i" n
    quad

// #time
// let x, y, z=
//     [1..10..100]
//     |> List.map (fun n ->
//         // let timeAverage = 
//         //     [1..10]
//         //     |> List.map (fun t -> 
//         let stopWatch = System.Diagnostics.Stopwatch.StartNew()
//         let quad = quadraticTime n
//         stopWatch.Stop()
//         let elapsed = stopWatch.Elapsed.TotalMilliseconds
//             //     elapsed |> float
//             // )
//             // |> List.average
//         (n, quad, elapsed))
//     |> List.unzip3
// #time

// [(1,2,3);(4,5,6)] |> List.unzip3

