namespace Turtle
open FSharp.Core.Operators.Checked
open Chiron

[<Measure>] type ml

type Distance = Distance of uint32

type Angle = Angle of uint16
with static member ToUInt16 (Angle v) = v

type Rotation = Rotation of int16

type PenState =
    | Up
    | Down
with
    static member ToJson (x:PenState) = 
        match x with
        | Up -> "up"
        | Down -> "down"
        |> Json.Optic.set Json.String_
    static member FromJson (_:PenState) = json {
        match! Json.Optic.get Json.String_ with
        | "up" -> return Up
        | "down" -> return Down
        | x -> return! Json.error (sprintf "Unrecognized value: %s" x)
    }

type Position =
    { X: uint32
      Y: uint32 }
with
    static member ToJson (x:Position) = json {
        do! Json.write "x" x.X
        do! Json.write "y" x.Y
    }
    static member FromJson (_:Position) = json {
        let! x = Json.read "x"
        let! y = Json.read "y"
        return { X = x; Y = y }
    }

type Command =
    | PenDown
    | PenUp
    | Move of Distance
    | MoveTo of Position
    | Turn of Rotation

type State = 
    { Pen: PenState
      InkSpent: float<ml>
      Position: Position
      Angle: Angle }
with
    static member ToJson (x:State) = json {
        do! Json.write "pen" x.Pen
        do! Json.write "ink" (float x.InkSpent)
        do! Json.write "position" x.Position
        do! Json.write "angle" (Angle.ToUInt16 x.Angle)
    }
    static member FromJson (_:State) = json {
        let! pen = Json.read "pen"
        let! ink = Json.read "ink"
        let! position = Json.read "position"
        let! angle = Json.read "angle"
        return { Pen = pen
                 InkSpent = (LanguagePrimitives.FloatWithMeasure ink)
                 Position = position
                 Angle = Angle angle }
    }

module Ink =
    let update (Distance distance) (current:float<ml>) =
        current + (float distance * 0.1<ml>)

module Angle =
    let toRadians (Angle angle) =
        System.Math.PI * (float angle) /180.

    let update (Rotation change) (Angle current) =
        match (int current) + (int change) with
        | a when a < 0 -> 360 - abs(a % 360)
        | a when a > 360 -> a % 360
        | a -> a
        |> uint16
        |> Angle

module Position =
    let update (angle: Angle, Distance distance) (current: Position) =
        { Y = (float current.Y) + ((float distance) * sin (Angle.toRadians angle)) |> round |> uint32
          X = (float current.X) + ((float distance) * cos (Angle.toRadians angle)) |> round |> uint32 }

    let getDistance (a: Position) (b: Position) =
        let dx = float (int a.X - int b.X) |> abs
        let dy = float (int a.Y - int b.Y) |> abs
        float (dx ** 2. + dy ** 2.) |> sqrt |> round |> int |> uint32 |> Distance

    let fromString (s: string) =
        let parts = 
            match s with
            | null | "" -> Result.Error "Postion not specified"
            | s -> s.Split '-' |> Array.map System.UInt32.TryParse |> Ok
        match parts with
        | Ok [| true,x; true,y |] -> Ok { X = x; Y = y }
        | Ok _ -> Result.Error "Invalid format"
        | Result.Error err -> Result.Error err

module State =
    let init () : State =
        { Pen = Up
          InkSpent = 0.<ml>
          Position = { X = 0u; Y = 0u }
          Angle = Angle 0us }

    let update (cmd:Command) (state:State) : State =
        match cmd, state.Pen with
        | PenUp, _ ->
            { state with Pen = Up }
        | PenDown, _ ->
            { state with Pen = Down }
        | Move distance, Down ->
            { state with
                InkSpent = state.InkSpent |> Ink.update distance
                Position = state.Position |> Position.update (state.Angle, distance) }
        | MoveTo position, Up ->
            { state with Position = position }
        | MoveTo position, Down ->
            { state with
                InkSpent = state.InkSpent |> Ink.update (state.Position |> Position.getDistance position)
                Position = position }
        | Move distance, Up ->
            { state with Position = state.Position |> Position.update (state.Angle, distance) }
        | Turn rotation, _ ->
            { state with Angle = state.Angle |> Angle.update rotation }

module HttpClient =
    open System.Net.Http

    let sendMsg (http: HttpClient) (baseAddress: System.Uri) (mkMsg: unit->HttpRequestMessage) =
        async {
            use msg = mkMsg ()
            msg.RequestUri <- System.Uri(baseAddress, string msg.RequestUri)
            return! http.SendAsync msg |> Async.AwaitTask
        }

module HttpResponseMessage =
    open System.Net.Http

    let mapContent (map:string*HttpResponseMessage->_) (result:Async<HttpResponseMessage>) : Async<_> =
        async {
            use! r = result 
            let! body = 
                if isNull r.Content then Async.result "" 
                else r.Content.ReadAsStringAsync() |> Async.AwaitTask
            return (body,r) |> map
        }

module Client =
    open System
    open System.Net.Http
    [<Struct>]
    type SendMessage = SendMessage of ((unit->HttpRequestMessage) -> Async<HttpResponseMessage>)

    [<RequireQualifiedAccess>]
    module RequestBuilders =
        let private buildMsg (path:string) (httpMethod:HttpMethod) (content:string option) =
            let uri = Uri(path, UriKind.Relative)
            let msg = new HttpRequestMessage (httpMethod, uri)
            match content with
            | Some json ->
                msg.Content <- new StringContent(json, System.Text.Encoding.UTF8, "application/json")
            | None ->
                ()
            msg
                
        let init =
            let apiPath = "/init"
            fun _ -> buildMsg apiPath HttpMethod.Get None

        let update (cmd:Command) (state:State) =
            let apiPath = 
                match cmd with 
                | PenUp -> "/pen-up"
                | PenDown -> "/pen-down"
                | MoveTo pos -> sprintf "/move-to/%d-%d" pos.X pos.Y
                | Move (Distance d) -> sprintf "/move/%d" d
                | Turn (Rotation r) -> sprintf "/turn/%d" r
            let data =
                state |> Json.serialize |> Json.format
            fun _ -> buildMsg apiPath HttpMethod.Post (Some data)
    

    [<RequireQualifiedAccess>]
    module ResponseMappers =
        open System.Net
        let state : (string * HttpResponseMessage -> State) = function
        | (json:string, r:HttpResponseMessage) when r.StatusCode = HttpStatusCode.OK ->
            json |> Json.parse |> Json.deserialize
        | x -> failwithf "Unexpected response: %A" x

    let init (SendMessage send) =
        RequestBuilders.init
        |> send
        |> HttpResponseMessage.mapContent ResponseMappers.state
    
    let update (SendMessage send) cmd state =
        RequestBuilders.update cmd state
        |> send
        |> HttpResponseMessage.mapContent ResponseMappers.state