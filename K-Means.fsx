#load "packages/FsLab/FsLab.fsx"

open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle
open XPlot.Plotly
let rgen = System.Random(14234)
let rfloat = rgen.NextDouble
type Point<'s> = 
    {X: 's; Y: 's; Z: 's}
    static member Zero = {X = 0.; Y = 0.; Z = 0.}
type Cluster = 
    {Points: Point<float> list; Centroid: Point<float>}
let distance (p1: Point<float>) (p2: Point<float>) =
    sqrt(pown (p1.X - p2.X) 2 + pown (p1.Y - p2.Y) 2 + pown (p1.Z - p2.Z) 2)
let initClusters points k = 
    let ks = points |> List.take k 
    [for point in points |> List.except ks -> ks |> List.minBy (distance point), point]
    |> List.groupBy fst
    |> List.map(fun (key, pairs) -> {Points = pairs |> List.map snd; Centroid = key})

let points = 
    [for _ in 1..rgen.Next(100) -> {X = rfloat() * 10.; Y = rfloat() * 10.; Z = rfloat()  * 10.}]

let clusters = initClusters points 5

points 
|> List.mapi (fun i p -> 
    Scatter3d(
        x = [|p.X|],
        y = [|p.Y|],
        z = [|p.Z|],
        text = [|string i|],
        mode = "markers",
         marker =
            Marker(
                color = sprintf "rgb(%d, %d, %d)" (rgen.Next(0,255)) (rgen.Next(0,255)) (rgen.Next(0,255)),
                size = 10.,
                symbol = "circle"
            )
    )
)
|> Plotly.Plot
|> Plotly.WithWidth 1000
|> Plotly.WithHeight 1000

let colors = 
    [|"red"; "green"; "blue"; "orange"; "magenta"|]

clusters
|> List.mapi (fun i cluster -> 
    Scatter3d(
        x = (cluster.Points |> List.map (fun point -> point.X) |> Array.ofList),
        y = (cluster.Points |> List.map (fun point -> point.Y) |> Array.ofList),
        z = (cluster.Points |> List.map (fun point -> point.Z) |> Array.ofList),
        mode = "markers",
        marker =
            Marker(
                color = colors.[i], //sprintf "rgb(%d, %d, %d)" ((int32 cluster.Centroid.X)) (int32 cluster.Centroid.Y) (int32 cluster.Centroid.Z),
                opacity = 0.8,
                size = 10.,
                symbol = "circle"
            )
    )
)
|> List.append (
    clusters 
    |> List.mapi(fun i cluster ->
        Scatter3d(
            x = [|cluster.Centroid.X|],
            y = [|cluster.Centroid.Y|],
            z = [|cluster.Centroid.Z|],
            mode = "markers",
            marker =
                Marker(
                    color = colors.[i],//sprintf "rgb(%d, %d, %d)" (int32 cluster.Centroid.X) (int32 cluster.Centroid.Y) (int32 cluster.Centroid.Z),
                    //size = (cluster.Points |> List.maxBy (distance cluster.Centroid) |> distance Point<float>.Zero) * 10.,
                    size = 15.,
                    symbol = "circle",
                    opacity = 0.5
                )
        )
    )
)
|> Plotly.Plot
|> Plotly.WithWidth 1000
|> Plotly.WithHeight 1000

