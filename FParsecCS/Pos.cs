// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

using System;

namespace FParsec {

public class Pos : IEquatable<Pos>, IComparable, IComparable<Pos> {
    public long   Index      { get; private set;}
    public long   Line       { get; private set;}
    public long   Column     { get; private set;}
    public string StreamName { get; private set;}

    public Pos(string streamName, long index, long line, long column) {
        this.StreamName = streamName; this.Index = index; this.Line = line; this.Column = column;
    }

    public override string ToString() {
        return (String.IsNullOrEmpty(StreamName) ? "(Ln: " : "(\"" + StreamName + "\", Ln: ")
               + Line.ToString() + ", Col: " + Column.ToString() + ")";
    }

    public override bool Equals(object other) {
        return Equals(other as Pos);
    }
    public bool Equals(Pos other) {
        return (object)other != null && Index == other.Index && Line == other.Line && Column == other.Column && StreamName == other.StreamName;
    }
    public override int GetHashCode() {
        return Index.GetHashCode();
    }
    public static bool operator==(Pos pos1, Pos pos2) { return  pos1.Equals(pos2); }
    public static bool operator!=(Pos pos1, Pos pos2) { return !pos1.Equals(pos2); }

    public int CompareTo(Pos other) {
        if ((object)this == (object)other) return 0;
        if ((object)other == null) return 1;
        int r = StreamName.CompareTo(other.StreamName);
        if (r != 0) return r;
        r = Line.CompareTo(other.Line);
        if (r != 0) return r;
        r = Column.CompareTo(other.Column);
        if (r != 0) return r;
        return Index.CompareTo(other.Index);
    }
    int IComparable.CompareTo(object value) {
        Pos pos = value as Pos;
        if ((object)pos != null) return CompareTo(pos);
        if (value == null) return 1;
        throw new ArgumentException("value", "Object must be of type Pos.");
    }
}

}
