module Newton

open Point

let createNewtonFunc (points: Point[]) : (decimal -> decimal) =
    let rec computeDividedDifferences (pts: Point list) =
        let rec buildColumn prevColumn =
            match prevColumn with
            | [] | [_] -> []  
            | _ ->
                let nextColumn =
                    prevColumn
                    |> List.pairwise
                    |> List.mapi (fun i (current, next) ->
                        let xi = pts.[i].X
                        let xj = pts.[i + 1 + (pts.Length - prevColumn.Length)].X  
                        (next - current) / (xj - xi))
                nextColumn :: buildColumn nextColumn

        let yValues = pts |> List.map (fun p -> p.Y)
        let allColumns = yValues :: buildColumn yValues
        allColumns |> List.map List.head  

    let pointList = Array.toList points
    let coefficients = computeDividedDifferences pointList 

    let interpolate (x: decimal) : decimal =
        let result =
            coefficients
            |> List.mapi (fun i coef ->
                let product =
                    [0 .. i - 1]
                    |> List.fold (fun acc j -> acc * (x - pointList.[j].X)) 1m
                coef * product)
            |> List.sum
        result

    interpolate