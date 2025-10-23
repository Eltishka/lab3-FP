module Linear

open Point

type Section = {
    A: decimal
    B: decimal
    LastPoint: Point
}

type InterpolatedFunction = {
    Sections: Section[]
}

let private addSection lastPoint point1 interFunc =
    let a = (point1.Y - lastPoint.Y) / (point1.X - lastPoint.X)
    let b = lastPoint.Y - lastPoint.X * a  // Упрощённая формула
    let newSection = {
        A = a
        B = b
        LastPoint = point1
    }
    {
        Sections = Array.append interFunc.Sections [| newSection |]
    }

let empty () =
    {
        Sections = Array.empty
    }

let newPoint point interFunc =
    let lastPoint = interFunc.Sections.[interFunc.Sections.Length - 1].LastPoint
    addSection lastPoint point interFunc

let interpolatePoint x interFunc =
    let rec loop i seg =
        if i < 0 then seg
        else
            if interFunc.Sections.[i].LastPoint.X <= x then
                interFunc.Sections.[i]
            else
                loop (i - 1) interFunc.Sections.[i]
    let seg = loop (interFunc.Sections.Length - 1) interFunc.Sections.[0]
    seg.A * x + seg.B

let createLinearFunc (points: Point[]) =

    let rec build i inter =
        if i >= points.Length then
            inter
        else
            let newInter = addSection points.[i - 1] points.[i] inter
            build (i + 1) newInter

    let interpolated = build 1 (empty())
    fun x -> interpolatePoint x interpolated