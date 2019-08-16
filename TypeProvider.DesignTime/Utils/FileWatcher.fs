[<RequireQualifiedAccess>]
module Froto.TypeProvider.Utils.FileWatcher

open System
open System.IO

type private State =
    { LastFileWriteTime: DateTime
      Updated: DateTime }

let watch changesOnly filePath onChanged =
    let getLastWrite() = File.GetLastWriteTime filePath
    let state = ref { LastFileWriteTime = getLastWrite(); Updated = DateTime.Now }

    let changed (_: FileSystemEventArgs) =
        let curr = getLastWrite()
        if curr <> (!state).LastFileWriteTime && DateTime.Now - (!state).Updated > TimeSpan.FromMilliseconds 500. then
          onChanged()
          state := { LastFileWriteTime = curr; Updated = DateTime.Now }

    let watcher = new FileSystemWatcher(Path.GetDirectoryName filePath, Path.GetFileName filePath)
    watcher.NotifyFilter <- NotifyFilters.CreationTime ||| NotifyFilters.LastWrite ||| NotifyFilters.Size
    watcher.Changed.Add changed
    if not changesOnly then
        watcher.Deleted.Add changed
        watcher.Renamed.Add changed
    watcher.EnableRaisingEvents <- true
    watcher :> IDisposable