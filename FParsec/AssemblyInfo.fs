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

[<assembly: AssemblyProduct(FParsec.CommonAssemblyInfo.Product)>]
[<assembly: AssemblyCopyright(FParsec.CommonAssemblyInfo.Copyright)>]
[<assembly: AssemblyVersion(FParsec.CommonAssemblyInfo.Version)>]
[<assembly: AssemblyFileVersion(FParsec.CommonAssemblyInfo.FileVersion)>]
[<assembly: AssemblyConfiguration(FParsec.CommonAssemblyInfo.Configuration)>]

[<assembly: InternalsVisibleTo(FParsec.CommonAssemblyInfo.TestAssemblyName)>]
do ()