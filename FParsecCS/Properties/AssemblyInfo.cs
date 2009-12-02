using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: ComVisible(false)]

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
[assembly: AssemblyCopyright("Copyright © Stephan Tolksdorf 2007-2009")]

[assembly: AssemblyVersion("0.8.0.0")]
[assembly: AssemblyFileVersion("0.8.0.0")]

[assembly: InternalsVisibleTo("FParsec")]
[assembly: InternalsVisibleTo("test_fparsec")]

