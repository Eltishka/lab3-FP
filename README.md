# Лабораторная работа 3

**Выполнил:** Кузнецов Кирилл Андреевич, P3330


## Требования=

- Реализация интерполяции линейной, Ньютона, Лагража
- "Оконный" режим
- Выбор алгоритма, шага, количества точек окна через ввод/вывод
  
## Ключевые элементы реализации


Streams.fs
```fsharp
type Point = {
    X: decimal 
    Y: decimal 
}
```

```fsharp

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

```

Program.fs
```fsharp

type Algorithm = 
    | Linear
    | Lagrange 
    | Newton

type CommandLineOptions = {
    Algorithms: Algorithm list
    Step: decimal
    WindowSize: int
}

type PointWithAlgo = {
    Point: Point
    Algorithm: Algorithm
}

let parseAlgorithm = function
    | "--linear" -> Some Linear
    | "--newton" -> Some Newton  
    | "--lagrange" -> Some Lagrange
    | _ -> None

let parseArguments (argv: string[]) =
    let rec parse args currentOptions=
        match args with
        | [] -> currentOptions
        | "--step" :: stepValue :: rest ->
            match Decimal.TryParse(stepValue) with
            | true, step -> parse rest { currentOptions with Step = step }
            | false, _ -> 
                printfn "invalid step %s" stepValue
                parse rest currentOptions
        | "-n" :: nValue :: rest ->
            match Int32.TryParse(nValue) with
            | true, n -> parse rest { currentOptions with WindowSize = n }
            | false, _ ->
                printfn "invalid window size %s" nValue
                parse rest currentOptions
        | arg :: rest ->
            match parseAlgorithm arg with
            | Some alg -> 
                parse rest { currentOptions with Algorithms = alg :: currentOptions.Algorithms }
            | None ->
                printfn "unknown argument %s" arg
                parse rest currentOptions

    let defaultOptions = {
        Algorithms = []
        Step = 0.1m
        WindowSize = 2
    }
    
    parse  (List.ofArray argv)  defaultOptions

let algorithmFunc = function
    | Linear -> createLinearFunc
    | Lagrange -> createLagrangeFunction
    | Newton -> createNewtonFunc


let runAlg (algs: Algorithm list) (step: decimal) (points: seq<Point>) (n: int) : seq<PointWithAlgo[]> =
    let rec runAllAlgs (algs: Algorithm[]) x (window: Point[]) i acc = 
        if i >= algs.Length then
            acc
        else 
            let f = (algorithmFunc algs.[i]) window
            runAllAlgs algs x window (i + 1) (Array.append acc [| {Point = {X = x; Y = f x}; Algorithm = algs.[i]} |])
        
    let windows = points |> slidingWindows n |> Seq.filter(fun window -> List.length window = n) |> Seq.cache
    let start = (points |> Seq.head).X
    let seqPoints = genPoints step start |> Seq.cache

    let rec processWindows (remainingWindows: seq<Point list>) (lastX: decimal) (acc: seq<PointWithAlgo[]>) =
        match remainingWindows |> Seq.tryHead with
        | None -> acc
        | Some window ->
            let last = (List.last window).X
            let windowArray = List.toArray window
            let algsArray = List.toArray algs
            
            let pointsInWindow = 
                seqPoints
                |> Seq.takeWhile (fun x -> x <= last)
                |> Seq.skipWhile (fun x -> x <= lastX)
                

            let windowResults = 
                pointsInWindow
                |> Seq.map (fun x -> 
                    runAllAlgs algsArray x windowArray 0 [||] 
                ) 

            processWindows 
                (remainingWindows |> Seq.tail) 
                (pointsInWindow |> Seq.last) 
                (Seq.append acc windowResults)
                
    
    processWindows windows (start - step) Seq.empty

   

let runAllAlgorithms (options: CommandLineOptions) (points: seq<Point>) : seq<PointWithAlgo> =
    runAlg options.Algorithms options.Step points options.WindowSize |> Seq.collect id

[<EntryPoint>]
let main argv = 
    let options = parseArguments argv
    
    if List.isEmpty options.Algorithms then
        printfn "Wrong algo"
        1
    else
        let points = stdinPoints()
        
        let results = runAllAlgorithms options points
        results |> Seq.iter (fun el -> printfn "%A: %f %f" el.Algorithm el.Point.X el.Point.Y)
        0
```
А также Newton.fs, Lagrange.fs, Linear.fs
## Выводы

1. Последовательности отлично подходят для работы с вводом/выводом (понятно, поток)
2. Пайпы имба

