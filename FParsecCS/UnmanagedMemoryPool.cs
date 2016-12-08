// Copyright (c) Stephan Tolksdorf 2010
// License: Simplified BSD License. See accompanying documentation.

#if !LOW_TRUST

using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;

namespace FParsec {

/// <summary>
/// Allocates and keeps references to chunks of unmanaged memory that we
/// intend to keep around for the lifetime of the AppDomain.
/// </summary>
internal sealed class UnmanagedMemoryPool {
    private static List<IntPtr> Handles = new List<IntPtr>();

    static public IntPtr Allocate(int size) {
        lock (Handles) {
            var h = Marshal.AllocHGlobal(size);
            Handles.Add(h);
            return h;
        }
    }

    // implementation of a "static finalizer"
    private UnmanagedMemoryPool() { }
    private static readonly UnmanagedMemoryPool Instance = new UnmanagedMemoryPool();
    ~UnmanagedMemoryPool() {
        var hs = Handles;
        Handles = null;
        foreach (var h in hs)
            Marshal.FreeHGlobal(h);
    }
}

}

#endif