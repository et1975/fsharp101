module Tests

open Xunit
open Swensen.Unquote
open Turtle

[<Fact>]
let ``Moves X, Stays on Y`` () =
    { X = 20u; Y = 20u } |> Position.update (Angle 0us, Distance 10u) =! { X = 30u; Y = 20u }
    { X = 20u; Y = 20u } |> Position.update (Angle 180us, Distance 10u) =! { X = 10u; Y = 20u }

[<Fact>]
let ``Moves Y, Stays on X`` () =
    { X = 20u; Y = 20u } |> Position.update (Angle 90us, Distance 10u) =! { X = 20u; Y = 30u }
    { X = 20u; Y = 20u } |> Position.update (Angle 270us, Distance 10u) =! { X = 20u; Y = 10u }

[<Fact>]
let Square () =
    (State.init (),
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
    ||> List.fold (fun s cmd -> s |> State.update cmd)
    =! { Pen = Up
         Position = { X = 20u; Y = 20u }
         InkSpent = 4.<ml>
         Angle = Angle 270us }
