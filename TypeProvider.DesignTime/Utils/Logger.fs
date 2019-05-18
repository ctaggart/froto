[<RequireQualifiedAccess>]
module Froto.TypeProvider.Core.Logger

open System
open System.Reflection
open System.IO
open Printf

open Froto.TypeProvider.Runtime

#if DEBUG

let private filename =
    Assembly.GetExecutingAssembly().Location
    |> Path.GetDirectoryName
    </> "log"
    </> (sprintf "%s-type-provider.log" (DateTime.Now.ToString("yyyy-MM-dd-hh-mm-ss-fff")))

let private stream =
    let folder = Path.GetDirectoryName filename
    if not <| Directory.Exists folder then Directory.CreateDirectory folder |> ignore
    new StreamWriter(filename, true, AutoFlush = true)

let private write (line: string) = stream.WriteLine(line)

#else

let write (line: string) = ()

#endif

let log fmt = kprintf write fmt