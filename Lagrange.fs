module Lagrange

open Point

let createLagrangeFunction (points: Point[]) : (decimal -> decimal) =
    fun x ->
        let n = points.Length - 1
        let rec sum i acc =
            if i > n then acc
            else
                let xi, yi = points.[i].X, points.[i].Y
                let li = 
                    let rec product j accProd =
                        if j > n then accProd
                        elif i = j then product (j + 1) accProd
                        else
                            let xj, _ = points.[j].X, points.[j].Y
                            product (j + 1) (accProd * (x - xj) / (xi - xj))
                    product 0 1.0m
                sum (i + 1) (acc + yi * li)
        sum 0 0.0m
