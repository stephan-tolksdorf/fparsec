using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: ComVisible(false)]

#if LOW_TRUST && CLR4 && !SILVERLIGHT
    [assembly: System.Security.AllowPartiallyTrustedCallers]
    [assembly: System.Security.SecurityTransparent]
#endif

[assembly: AssemblyTitle      ("FParsecCS.dll")]
[assembly: AssemblyDescription("FParsecCS.dll")]

[assembly: AssemblyProduct      (FParsec.CommonAssemblyInfo.Product)]
[assembly: AssemblyCopyright    (FParsec.CommonAssemblyInfo.Copyright)]
[assembly: AssemblyVersion      (FParsec.CommonAssemblyInfo.Version)]
[assembly: AssemblyFileVersion  (FParsec.CommonAssemblyInfo.FileVersion)]
[assembly: AssemblyConfiguration(FParsec.CommonAssemblyInfo.Configuration)]

[assembly: InternalsVisibleTo (FParsec.CommonAssemblyInfo.FParsecAssemblyName)]
[assembly: InternalsVisibleTo (FParsec.CommonAssemblyInfo.TestAssemblyName)]


namespace FParsec {

internal static partial class CommonAssemblyInfo {
    public const string Product     = "FParsec";
    public const string Copyright   = "Copyright © Stephan Tolksdorf 2007-2013";
    public const string Version     = "1.0.0.0";
    public const string FileVersion = "1.0.1.0";
    
#if !NUGET // the NuGet package build script sets these attributes
    public const string Configuration =
    #if DEBUG
        #if LOW_TRUST
            "Debug Build (/define:LOW_TRUST)";
        #else
            "Debug Build";
        #endif
    #else
        #if LOW_TRUST
            "Release Build (/define:LOW_TRUST)";
        #else
            "Release Build";
        #endif
    #endif
    public const string FParsecAssemblyName = "FParsec";
    public const string TestAssemblyName = "test_fparsec";
#endif
};

}
