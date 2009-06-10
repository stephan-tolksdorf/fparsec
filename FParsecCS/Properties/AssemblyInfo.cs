using System.Reflection;
using System.Runtime.CompilerServices;

[assembly: AssemblyTitle("FParsecCS.dll")]
[assembly: AssemblyDescription("FParsecCS.dll")]
#if DEBUG
    #if UNALIGNED_READS
        [assembly: AssemblyConfiguration("Debug Build (/define:UNALIGNED_READS)")]
    #else
        [assembly: AssemblyConfiguration("Debug Build")]
    #endif
#else
    #if UNALIGNED_READS
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

