// Copyright (c) Stephan Tolksdorf 2010
// License: Simplified BSD License. See accompanying documentation.

using System;
using System.Diagnostics;
using System.Collections.Generic;

namespace FParsec {

[DebuggerDisplay("{ErrorMessageList.GetDebuggerDisplay(this),nq}"),
 DebuggerTypeProxy(typeof(ErrorMessageList.DebugView))]
public sealed class ErrorMessageList : IEquatable<ErrorMessageList> {
    public readonly ErrorMessage Head;
    public readonly ErrorMessageList Tail;

    public ErrorMessageList(ErrorMessage head, ErrorMessageList tail) {
        var throwNullReferenceExceptionIfHeadIsNull = head.Type;
        Head = head;
        Tail = tail;
    }

    public ErrorMessageList(ErrorMessage message) {
        var throwNullReferenceExceptionIfMessageIsNull = message.Type;
        Head = message;
    }

    public ErrorMessageList(ErrorMessage message1, ErrorMessage message2) {
        var throwNullReferenceExceptionIfMessage1IsNull = message1.Type;
        Head = message1;
        Tail = new ErrorMessageList(message2);
    }

    public static ErrorMessageList Merge(ErrorMessageList list1, ErrorMessageList list2) {
        if ((object)list1 == null) return list2;
        return MergeContinue(list1, list2);
    }
    private static ErrorMessageList MergeContinue(ErrorMessageList list1, ErrorMessageList list2) {
        while ((object)list2 != null) {
            list1 = new ErrorMessageList(list2.Head, list1);
            list2 = list2.Tail;
        }
        return list1;
    }

    public static HashSet<ErrorMessage> ToHashSet(ErrorMessageList messages)  {
        var msgs = messages;
        var set = new HashSet<ErrorMessage>();
        for (; (object)msgs != null; msgs = msgs.Tail) {
            var msg = msgs.Head;
            Debug.Assert(msg.Type >= 0);
            if (msg.Type <= ErrorMessageType.Message && string.IsNullOrEmpty(msg.String)) continue;
            set.Add(msg);
        }
        return set;
    }

    public static ErrorMessage[] ToSortedArray(ErrorMessageList messages) {
        var set = ToHashSet(messages);
        var array = new ErrorMessage[set.Count];
        set.CopyTo(array);
        Array.Sort(array, ErrorMessage.Comparer);
        return array;
    }

    public override bool Equals(object obj) { return Equals(obj as ErrorMessageList); }

    public bool Equals(ErrorMessageList other) {
        return    (object)this == (object)other
               || (   (object)other != null
                   && ToHashSet(this).SetEquals(ToHashSet(other)));
    }

    public static bool operator==(ErrorMessageList left, ErrorMessageList right) {
        return    (object)left == (object)right
               || (   (object)left != null
                   && (object)right != null
                   && ToHashSet(left).SetEquals(ToHashSet(right)));
    }
    public static bool operator!=(ErrorMessageList left, ErrorMessageList right) { return !(left == right); }

    public override int GetHashCode() {
        var set = ToHashSet(this);
        var h = 0;
        foreach (var msg in set)
            h ^= msg.GetHashCode();
        return h;
    }

    internal static string GetDebuggerDisplay(ErrorMessageList list) {
        var es = ErrorMessageList.ToSortedArray(list);
        switch (es.Length) {
        case 0:  return "[]";
        case 1:  return "[" + es[0].GetDebuggerDisplay() + "]";
        case 2:  return "[" + es[0].GetDebuggerDisplay() + "; " + es[1].GetDebuggerDisplay() + "]";
        case 3:  return "[" + es[0].GetDebuggerDisplay() + "; " + es[1].GetDebuggerDisplay() + "; " + es[2].GetDebuggerDisplay() + "]";
        default: return "[" + es[0].GetDebuggerDisplay() + "; " + es[1].GetDebuggerDisplay() + "; " + es[2].GetDebuggerDisplay() + "; ...]";
        }
    }

    internal class DebugView {
        //[DebuggerBrowsable(DebuggerBrowsableState.Never)]
        private ErrorMessageList List;

        public DebugView(ErrorMessageList list) { List = list; }

        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
        public ErrorMessage[] Items { get { return ErrorMessageList.ToSortedArray(List); } }
    }
}

}