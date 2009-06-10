namespace FParsec

open System.Reflection

[<assembly: AssemblyTitle("FParsec.dll")>]
[<assembly: AssemblyDescription("FParsec.dll")>]
#if DEBUG
    [<assembly: AssemblyConfiguration("Debug Build")>]
#else
    [<assembly: AssemblyConfiguration("Release Build")>]
#endif
[<assembly: AssemblyProduct("FParsec")>]
[<assembly: AssemblyCopyright("Copyright \169 Stephan Tolksdorf 2007-2009")>]
[<assembly: AssemblyVersion("0.8.0.0")>]
[<assembly: AssemblyFileVersion("0.8.0.0")>]
do ()