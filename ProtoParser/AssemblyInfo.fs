namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.1.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0")>]
[<assembly: AssemblyInformationalVersionAttribute("{\"buildVersion\":\"0.1.0-ci1406081724\",\"buildDate\":\"2014-06-08T17:24:41Z\",\"gitCommit\":\"88f3a3037ae263ca7b7193d62cdb3e4f249ca542\"}")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.0"
