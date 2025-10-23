module Streams

open System
open System.Globalization
open Point

let stdinLines () : seq<string> =
    Seq.unfold (fun () ->
        let line = Console.ReadLine()
        if isNull line then None else Some (line, ())
    ) ()

let tryParsePoint (line: string) : Point option =
    let seps = [| ';'; '\t'; ' '; ',' |]
    let parts =
        line.Split(seps, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.Trim())
    if parts.Length >= 2 then
        let style = NumberStyles.Float
        let culture = CultureInfo.InvariantCulture
        match Decimal.TryParse(parts.[0], style, culture),
                Decimal.TryParse(parts.[1], style, culture) with
        | (true, x), (true, y) -> Some { X = x; Y = y }
        | _ -> None
    else None


let stdinPoints () : seq<Point> =
    stdinLines ()
    |> Seq.choose tryParsePoint


let slidingWindows (w: int) (points: seq<Point>) : seq<Point list> =
    let addKeep (buf: Point list) (p: Point) =
        let buf' = buf @ [p]
        let len = List.length buf'
        if len <= w then buf' else buf' |> List.skip (len - w)
    points |> Seq.scan addKeep [] |> Seq.tail

let genPoints (step: decimal) (start: decimal): seq<decimal> = 
    Seq.unfold (fun x -> Some(x, x + step)) start
