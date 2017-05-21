using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: ComVisible(false)]

#if LOW_TRUST
    [assembly: System.Security.AllowPartiallyTrustedCallers]
    [assembly: System.Security.SecurityTransparent]
#endif

[assembly: InternalsVisibleTo ("FParsec" + FParsec.CommonAssemblyInfo.StrongNamePublicKey)]
[assembly: InternalsVisibleTo (FParsec.CommonAssemblyInfo.TestAssemblyName + FParsec.CommonAssemblyInfo.StrongNamePublicKey)]

namespace FParsec {

internal static partial class CommonAssemblyInfo {
    public const string TestAssemblyName = "Test";

#if STRONG_NAME
    public const string StrongNamePublicKey =
        ", PublicKey=002400000480000094000000060200000024000052534131000400000100010077c6be48a40f5b" +
        "194ec9f992e5b512bbbba33e211354d9ee50c3214decddad8356470a9a19a9ee84637cbd6ff690" +
        "9527d3973741dbe0a69b1461eeae774af9a78de45618ffd6fe7c7d52e0441b92f3bc7e8fb5757f" +
        "b8b1611a0b6b8c9f9ef64edcf51d44218ae040f3015373fd261d30f8e1f5a1f914fd9ebcde7d7e" +
        "f42dbaa5";
#else
    public const string StrongNamePublicKey = "";
#endif
};

}
