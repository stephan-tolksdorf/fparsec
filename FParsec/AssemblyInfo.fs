namespace FParsec

open System.Reflection
open System.Runtime.InteropServices

[<assembly: ComVisible(false)>]

[<assembly: AssemblyTitle("FParsec.dll")>]
[<assembly: AssemblyDescription("FParsec.dll")>]
#if DEBUG
    #if LOW_TRUST
        [<assembly: AssemblyConfiguration("Debug Build (--define:LOW_TRUST)")>]
    #else
        [<assembly: AssemblyConfiguration("Debug Build")>]
    #endif
#else
    #if LOW_TRUST
        [<assembly: AssemblyConfiguration("Release Build (--define:LOW_TRUST)")>]
    #else
        [<assembly: AssemblyConfiguration("Release Build")>]
    #endif
#endif
[<assembly: AssemblyProduct("FParsec")>]
[<assembly: AssemblyCopyright("Copyright \169 Stephan Tolksdorf 2007-2009")>]
[<assembly: AssemblyVersion("0.8.0.0")>]
[<assembly: AssemblyFileVersion("0.8.0.0")>]
do ()