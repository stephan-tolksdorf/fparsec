using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: ComVisible(false)]

#if LOW_TRUST && CLR4
    [assembly: System.Security.AllowPartiallyTrustedCallers]
    [assembly: System.Security.SecurityTransparent]
#endif

[assembly: AssemblyTitle      ("FParsecCS.dll")]
[assembly: AssemblyDescription("FParsecCS.dll")]

[assembly: AssemblyProduct      (FParsec.CommonAssemblyInfo.Product)]
[assembly: AssemblyCopyright    (FParsec.CommonAssemblyInfo.Copyright)]
[assembly: AssemblyVersion      (FParsec.CommonAssemblyInfo.Version)]
[assembly: AssemblyFileVersion  (FParsec.CommonAssemblyInfo.FileVersion)]
[assembly: AssemblyConfiguration(FParsec.CommonAssemblyInfo.CSConfiguration)]

[assembly: InternalsVisibleTo (FParsec.CommonAssemblyInfo.FParsecAssemblyName + FParsec.CommonAssemblyInfo.StrongNamePublicKey)]
[assembly: InternalsVisibleTo (FParsec.CommonAssemblyInfo.TestAssemblyName + FParsec.CommonAssemblyInfo.StrongNamePublicKey)]

namespace FParsec {

internal static partial class CommonAssemblyInfo {
    public const string Product     = "FParsec";
    public const string Copyright   = "Copyright © Stephan Tolksdorf 2007-2015";
    public const string Version     = "1.0.0.0";
    public const string FileVersion = "1.0.3.0";

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
    public const string CSConfiguration = Configuration;
    public const string FSConfiguration = Configuration;
    public const string FParsecAssemblyName = "FParsec";
    public const string TestAssemblyName = "Test";

#if STRONG_NAME
    public const string StrongNamePublicKey =
        ", PublicKey=002400000480000094000000060200000024000052534131000400000100010077c6be48a40f5b" +
        "194ec9f992e5b512bbbba33e211354d9ee50c3214decddad8356470a9a19a9ee84637cbd6ff690" +
        "9527d3973741dbe0a69b1461eeae774af9a78de45618ffd6fe7c7d52e0441b92f3bc7e8fb5757f" +
        "b8b1611a0b6b8c9f9ef64edcf51d44218ae040f3015373fd261d30f8e1f5a1f914fd9ebcde7d7e" +
        "f42dbaa5";
    
    public const string PublicKeyToken =
        ", PublicKeyToken=40ccfc0a09edbb5d";
#else
    public const string StrongNamePublicKey = "";
    public const string PublicKeyToken = "";
#endif
#endif
};

}
