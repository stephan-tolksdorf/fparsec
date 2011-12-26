// Copyright (c) Stephan Tolksdorf 2010
// License: Simplified BSD License. See accompanying documentation.

using System;
using System.Collections;
using System.Collections.Generic;

using Microsoft.FSharp.Core;

namespace FParsec {

internal static class FastGenericEqualityERComparer<T> {
    // if T is a reference type, accessing the field requires a hash table lookup
    public static EqualityComparer<T> Instance = FastGenericEqualityERComparer.Create<T>();

    /// <summary>For reference types it's faster to call Instance.Equals directly
    /// (due to limitations of the inliner of the .NET JIT.)</summary>
    public static bool Equals(T left, T right) {
        return Instance.Equals(left, right);
    }
}

internal static class FastGenericEqualityERComparer {
    public static EqualityComparer<T> Create<T>() {
        var t = typeof(T);
        if (t.IsArray) return new ArrayStructuralEqualityERComparer<T>();
        if (typeof(IStructuralEquatable).IsAssignableFrom(t)) {
            var gct = t.IsValueType ? typeof(StructStructuralEqualityERComparer<>)
                                    : typeof(ClassStructuralEqualityERComparer<>);
            var ct = gct.MakeGenericType(t);
        #if LOW_TRUST
            return (EqualityComparer<T>)Activator.CreateInstance(ct);
        #else
            return (EqualityComparer<T>)System.Runtime.Serialization.FormatterServices.GetUninitializedObject(ct);
        #endif
        }
        return EqualityComparer<T>.Default;
    }

    private class ClassStructuralEqualityERComparer<T> : EqualityComparer<T> where T : class, IStructuralEquatable {
        public override bool Equals(T x, T y) {
            return    (object)x == (object)y
                   || ((object)x != null && x.Equals(y, LanguagePrimitives.GenericEqualityERComparer));
        }

        public override int GetHashCode(T obj) {
            if ((object)obj == null) throw new ArgumentNullException("obj");
            return obj.GetHashCode(LanguagePrimitives.GenericEqualityERComparer);
        }
    }

    private class StructStructuralEqualityERComparer<T> : EqualityComparer<T> where T : struct, IStructuralEquatable {
        public override bool Equals(T x, T y) {
            return x.Equals(y, LanguagePrimitives.GenericEqualityERComparer);
        }

        public override int GetHashCode(T obj) {
            return obj.GetHashCode(LanguagePrimitives.GenericEqualityERComparer);
        }
    }

    /// <summary>Forwards all work to F#'s GenericEqualityERComparer.</summary>
    private class ArrayStructuralEqualityERComparer<T> : EqualityComparer<T> {
        public override bool Equals(T x, T y) {
            return (object)x == (object)y || LanguagePrimitives.GenericEqualityERComparer.Equals(x, y);
        }

        public override int GetHashCode(T obj) {
            if ((object)obj == null) throw new ArgumentNullException("obj");
            return LanguagePrimitives.GenericEqualityERComparer.GetHashCode(obj);
        }
    }
}

}