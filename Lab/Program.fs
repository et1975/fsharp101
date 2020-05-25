namespace Turtle

open System
open Suave
open Suave.Filters
open Suave.Operators
open Chiron
open FSharp.Core

module Server =
    let inline okJson (o:^T) =
        Suave.Writers.setHeader "Content-Type" "application/json; charset=utf-8" 
        >=> Successful.OK (o |> Json.serialize |> Json.format)          
        
    let inline badRequest err =
        RequestErrors.BAD_REQUEST ("invalid request: " + err)

    module Result =
        let iter f (r:Result<_,string>) =
            match r with
            | Ok v -> f v
            | Error err -> badRequest err

    let init (ctx: HttpContext) =
        async {
            let response = State.init()
            return! okJson response ctx
        }

    let update msg =
        fun (ctx: HttpContext) ->
            async {
                match ctx.request.rawForm |> Text.Encoding.UTF8.GetString |> Json.tryParse with
                | Choice1Of2 json ->
                    match json |> Json.tryDeserialize with
                    | Choice1Of2 (state: State) ->
                        let response = state |> State.update msg
                        return! okJson response ctx
                    | Choice2Of2 err ->
                        return! badRequest err ctx
                | Choice2Of2 err ->
                    return! badRequest err ctx
            }
            
    let log (ctx:HttpContext) =
        async {
            printfn "%s - [%s] - %s %s %d" (ctx.clientIpTrustProxy.ToString())
                                           (DateTimeOffset.UtcNow.ToString()) 
                                           (ctx.request.method.ToString()) 
                                           (ctx.request.url.PathAndQuery) 
                                           ctx.response.status.code
            return Some ctx
        }
            
    let routes =
        choose [
            HEAD >=> path "/" >=> Successful.NO_CONTENT // Used as liveness probe by integration tests only
            choose [
                GET >=> choose [
                    path "/init" >=> init
                ]
                POST >=> choose [
                    path "/pen-up" >=> update PenUp
                    path "/pen-down" >=> update PenDown
                    pathScan "/move-to/%s" (Position.fromString >> Result.iter (MoveTo >> update))
                    pathScan "/move/%u" (Distance >> Move >> update)
                    pathScan "/turn/%d" (Rotation >> Turn >> update)
                ]
            ]
            RequestErrors.NOT_FOUND "not found"
        ] >=> log

module Program =
    [<EntryPoint>]
    let main _ =
        let port =
            Environment.GetEnvironmentVariable "PORT" |> function
            | null -> 8080
            | s -> s |> int
        startWebServer { defaultConfig with hideHeader=true; bindings=[HttpBinding.createSimple HTTP "0.0.0.0" port] } Server.routes
        0 // return an integer exit code