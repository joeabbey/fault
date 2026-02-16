module Sample

open System
open System.IO
open System.Collections.Generic

let greet name =
    sprintf "Hello, %s" name

let add x y = x + y

let private helper msg =
    printfn "%s" msg

member this.Process(items) =
    items |> List.map (fun x -> x * 2)
