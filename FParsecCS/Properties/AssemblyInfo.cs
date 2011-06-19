using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: ComVisible(false)]

#if LOW_TRUST && CLR4 && !SILVERLIGHT
    [assembly: System.Security.AllowPartiallyTrustedCallers]
    [assembly: System.Security.SecurityTransparent]
#endif

[assembly: AssemblyTitle("FParsecCS.dll")]
[assembly: AssemblyDescription("FParsecCS.dll")]
#if DEBUG
    #if LOW_TRUST
        [assembly: AssemblyConfiguration("Debug Build (/define:LOW_TRUST)")]
    #elif UNALIGNED_READS
        [assembly: AssemblyConfiguration("Debug Build (/define:UNALIGNED_READS)")]
    #else
        [assembly: AssemblyConfiguration("Debug Build")]
    #endif
#else
    #if LOW_TRUST
        [assembly: AssemblyConfiguration("Release Build (/define:LOW_TRUST)")]
    #elif UNALIGNED_READS
        [assembly: AssemblyConfiguration("Release Build (/define:UNALIGNED_READS)")]
    #else
        [assembly: AssemblyConfiguration("Release Build")]
    #endif
#endif
[assembly: AssemblyProduct("FParsec")]
[assembly: AssemblyCopyright("Copyright © Stephan Tolksdorf 2007-2011")]

[assembly: AssemblyVersion("0.9.1.1")]
[assembly: AssemblyFileVersion("0.9.1.1")]

[assembly: InternalsVisibleTo("FParsec")]
[assembly: InternalsVisibleTo("test_fparsec")]

