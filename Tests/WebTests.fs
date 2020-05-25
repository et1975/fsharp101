module WebTests

open System
open System.Net.Http
open System.Threading.Tasks
open System.Threading
open Suave
open Chiron
open Xunit
open Swensen.Unquote
open Turtle

module Http =
    open System.Net

    let waitFor method period url =
        async {
            use client = new HttpClient()

            let rec poll sleep =
                async {
                    if sleep then do! Async.Sleep period

                    use request =
                        new HttpRequestMessage(Method = method, RequestUri = url)

                    let! response =
                        client.SendAsync request
                        |> Async.AwaitTask
                        |> Async.Catch
                    match response with
                    | Choice1Of2 r when r.StatusCode = HttpStatusCode.NoContent -> ()
                    | Choice1Of2 r -> return! poll true
                    | Choice2Of2 ex -> return! poll true
                }

            return! poll false
        }

type LoggingHttpClient(innerHandler, log: string -> unit) =
    inherit DelegatingHandler(innerHandler)

    member private __.BaseImpl(request, cancellationToken) =
        base.SendAsync(request, cancellationToken)
        |> Async.AwaitTask

    override this.SendAsync(request: HttpRequestMessage, cancellationToken: CancellationToken): Task<HttpResponseMessage> =
        async {
            if isNull request.Content then
                sprintf "Request: %A" request |> log
            else
                let! content =
                    request.Content.ReadAsStringAsync()
                    |> Async.AwaitTask
                sprintf "Request: %A, Content: %s" request content
                |> log

            let! response = this.BaseImpl(request, cancellationToken)

            if isNull response.Content then
                sprintf "Response: %A" response |> log
            else
                let! content =
                    response.Content.ReadAsStringAsync()
                    |> Async.AwaitTask
                sprintf "Response: %A, Content: %s" response content
                |> log

            return response
        }
        |> Async.StartAsTask

module List =
    let rec asyncFold (f: 'state -> 'item -> Async<'state>) (state: 'state) (items: 'item list) =
        async {
            match items with
            | h :: tail ->
                let! state = f state h
                return! asyncFold f state tail
            | [] -> return state
        }

module Serialization =
    open FsCheck
    open FsCheck.Xunit

    type Generators() =
        static member Floats() =
            Arb.Default.UInt32()
            |> Arb.convert (float >> fun x -> x / 10.) (fun x -> uint32 x * 10u)

    [<Property(Arbitrary = [| typeof<Generators> |])>]
    let ``json roundtrips`` (original: State) =
        Json.serialize original
        |> Json.format
        |> Json.parse
        |> Json.deserialize
        =! original


type WithLogger(output: Xunit.Abstractions.ITestOutputHelper) =
    let httpClient =
        let log text = output.WriteLine text
        new HttpClient(new LoggingHttpClient(new HttpClientHandler(), log) :> HttpMessageHandler)

    [<Fact>]
    member __.Square() =
        async {
            // configure and start the server
            let cts = new CancellationTokenSource()
            let port = 9080

            let conf =
                { defaultConfig with
                      hideHeader = true
                      bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" port ] }

            let _, server = startWebServerAsync conf Server.routes
            Async.Start(server, cts.Token)

            let address =
                sprintf "http://localhost:%d/" port |> Uri

            do! address |> Http.waitFor HttpMethod.Head 10_000

            // configure the send function
            let sendMsg =
                HttpClient.sendMsg httpClient address
                |> Client.SendMessage

            // make the calls
            let! initial = Client.init sendMsg
            return! (initial,
                     [ MoveTo { X = 20u; Y = 20u }
                       PenDown
                       Move (Distance 10u)
                       Turn (Rotation 90s)
                       Move (Distance 10u)
                       Turn (Rotation 90s)
                       Move (Distance 10u)
                       Turn (Rotation 90s)
                       Move (Distance 10u)
                       PenUp ])
                    ||> List.asyncFold (fun s cmd -> Client.update sendMsg cmd s)
        }
        |> Async.RunSynchronously
        =! { Pen = Up
             Position = { X = 20u; Y = 20u }
             InkSpent = 4.<ml>
             Angle = Angle 270us }
