﻿namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.8.6.1")>]
[<assembly: AssemblyFileVersionAttribute("0.8.6.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.8.6.1"
