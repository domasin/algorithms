let rec sseq1 acc el xs =
    match xs with
    | [] -> acc |> List.rev |> List.map List.rev
    | hd :: tl ->
        match acc with
        | [] -> sseq1 [ [ hd; el ]; [ el ] ] el tl
        | hd' :: tl' -> sseq1 ((hd :: hd') :: acc) el tl

let rec sseq2 acc xs =
    match xs with
    | [] -> acc |> List.rev |> List.concat
    | [ hd ] -> sseq2 ([ [ hd ] ] :: acc) []
    | hd :: tl ->
        let hdSseq = sseq1 [] hd tl
        sseq2 (hdSseq :: acc) tl

let sseq xs =
    sseq2 [] xs

[(1,13);(2,-3);(3,-25);(4,20);(5,-3);(6,-16);
(7,-23);(8,18);(9,20);(10,-7);(11,12);(12,-5);
(13,-22);(14,15);(15,-4);(16,7)]
|> sseq
// |> fun xs -> xs[0..3]
|> List.map (fun xs -> 
    let min,_ = xs[0]
    let max,_ = xs[xs.Length-1]
    let sum = xs |> List.sumBy snd
    (min,max,sum))
|> List.maxBy (fun (_,_,sum) -> sum)

-infinity + 1.

let maxRightSeq xs =
    // printfn "curVal | prvInd | prvSum | maxInd | maxSum "

    xs
    |> List.fold
        (fun (prvInd, prvSum, maxInd, maxSum) curVal ->
            // printf " %5.0f | %6i | %6.0f | %6i | %6.0f " 
            //     curVal prvInd prvSum maxInd maxSum |> ignore
            let cumSum = prvSum + curVal
            let curInd = prvInd + 1

            // printfn "--> cumSum = %.0f" cumSum

            match prvInd, cumSum > maxSum with
            | -1,_ -> (curInd, cumSum, curInd, cumSum)
            | _,true -> (curInd, cumSum, curInd, cumSum)
            | _ -> (curInd, cumSum, maxInd, maxSum))
        (-1, 0., 0, 0.)
    |> fun (_, _, maxInd, maxSum)  -> (maxInd, maxSum) 


[0.;1.;2.;-3.;-4.] |> maxRightSeq

[0.;-25.;20.;6.;-16.]
|> maxRightSeq

let maxCrossingSubSeq low mid high (xs:float list) = 
    let lxs =  xs[low..mid]
    let rxs = xs[mid+1..high]
    
    let maxLeft, leftsum = 
        lxs 
        |> List.rev
        |> maxRightSeq
        |> fun (maxIndex,maxSum) -> 
            mid-maxIndex, maxSum

    let maxRight, rightSum = 
        rxs 
        |> maxRightSeq
        |> fun (maxIndex,maxSum) -> 
            mid+1+maxIndex, maxSum

    maxLeft,maxRight,rightSum+leftsum 



[0.;-25.;20.;3.;-16.;0.;1.;2.;-3.;4.]
|> maxCrossingSubSeq 1 5 9

// let maxSeq acc xs = 
//     match xs with
//     | [] -> []
//     | hd::tl -> 

// let leftSum acc low mid xs = 


// Max crossing subsequence
// let maxCrsSseq low mid high xs = 
//     match xs with
//     | [] -> (0,0,0)
//     | _ -> 

