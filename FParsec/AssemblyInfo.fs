namespace FParsec

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly: ComVisible(false)>]

#if LOW_TRUST
 #if CLR4
  #if SILVERLIGHT
  #else
    [<assembly: System.Security.AllowPartiallyTrustedCallers>]
    [<assembly: System.Security.SecurityTransparent>]
  #endif
 #endif
#endif

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
[<assembly: AssemblyCopyright("Copyright \169 Stephan Tolksdorf 2007-2011")>]
[<assembly: AssemblyVersion("0.9.1.1")>]
[<assembly: AssemblyFileVersion("0.9.1.1")>]
[<assembly: InternalsVisibleTo("test_fparsec")>]
do ()