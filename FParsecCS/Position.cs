// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

using System;

namespace FParsec {

public sealed class Position : IEquatable<Position>, IComparable, IComparable<Position> {
    public long   Index      { get; private set; }
    public long   Line       { get; private set; }
    public long   Column     { get; private set; }
    public string StreamName { get; private set; }

    public Position(string streamName, long index, long line, long column) {
        StreamName = streamName; Index = index; Line = line; Column = column;
    }

    public override string ToString() {
        return (String.IsNullOrEmpty(StreamName) ? "(Ln: " : "(\"" + StreamName + "\", Ln: ")
               + Line.ToString() + ", Col: " + Column.ToString() + ")";
    }

    public override bool Equals(object obj) {
        return Equals(obj as Position);
    }
    public bool Equals(Position other) {
        return (object)other != null && Index == other.Index && Line == other.Line && Column == other.Column && StreamName == other.StreamName;
    }
    public override int GetHashCode() {
        return Index.GetHashCode();
    }
    public static bool operator==(Position left, Position right) { return  left.Equals(right); }
    public static bool operator!=(Position left, Position right) { return !left.Equals(right); }

    public int CompareTo(Position other) {
        if ((object)this == (object)other) return 0;
        if ((object)other == null) return 1;
        int r = String.CompareOrdinal(StreamName, other.StreamName);
        if (r != 0) return r;
        r = Line.CompareTo(other.Line);
        if (r != 0) return r;
        r = Column.CompareTo(other.Column);
        if (r != 0) return r;
        return Index.CompareTo(other.Index);
    }
    int IComparable.CompareTo(object value) {
        Position position = value as Position;
        if ((object)position != null) return CompareTo(position);
        if (value == null) return 1;
        throw new ArgumentException("value", "Object must be of type Position.");
    }
}

[Obsolete("FParsec.State.Pos has been renamed to FParsec.State.Position.", true)]
public sealed class Pos {}

}
