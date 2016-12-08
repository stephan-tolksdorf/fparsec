// Copyright (c) Stephan Tolksdorf 2010
// License: Simplified BSD License. See accompanying documentation.

using System;
using System.Diagnostics;
using System.Collections.Generic;

using Microsoft.FSharp.Core;

namespace FParsec {

public enum ErrorMessageType {
    Expected,
    ExpectedString,
    ExpectedCaseInsensitiveString,
    Unexpected,
    UnexpectedString,
    UnexpectedCaseInsensitiveString,
    Message,
    NestedError,
    CompoundError,
    Other
}

[DebuggerDisplay("{GetDebuggerDisplay(),nq}")]
public class ErrorMessage : IEquatable<ErrorMessage> {
    public readonly ErrorMessageType Type;

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    internal string String;

    internal ErrorMessage(ErrorMessageType messageType) {
        Type = messageType;
    }

    public class Expected : ErrorMessage {
        public string Label { get { return String; } }
        public Expected(string labelForExpectedInput) : base(ErrorMessageType.Expected) {
            String = labelForExpectedInput;
        }
    }

    public class ExpectedString : ErrorMessage {
        public new string String { get { return String; } }
        public ExpectedString(string expectedString) : base(ErrorMessageType.ExpectedString) {
            base.String = expectedString;
        }
    }

    public class ExpectedCaseInsensitiveString : ErrorMessage {
        public string CaseInsensitiveString { get { return String; } }
        public ExpectedCaseInsensitiveString(string expectedCaseInsensitiveString) : base(ErrorMessageType.ExpectedCaseInsensitiveString) {
            String = expectedCaseInsensitiveString;
        }
    }

    public class Unexpected : ErrorMessage {
        public string Label { get { return String; } }
        public Unexpected(string labelForUnexpectedInput) : base(ErrorMessageType.Unexpected) {
            String = labelForUnexpectedInput;
        }
    }

    public class UnexpectedString : ErrorMessage {
        public new string String { get { return String; } }
        public UnexpectedString(string unexpectedString) : base(ErrorMessageType.UnexpectedString) {
            base.String = unexpectedString;
        }
    }

    public class UnexpectedCaseInsensitiveString : ErrorMessage {
        public string CaseInsensitiveString { get { return String; } }
        public UnexpectedCaseInsensitiveString(string unexpectedCaseInsensitiveString) : base(ErrorMessageType.UnexpectedCaseInsensitiveString) {
            String = unexpectedCaseInsensitiveString;
        }
    }

    public class Message : ErrorMessage {
        public new string String { get { return String; } }
        public Message(string message) : base(ErrorMessageType.Message) {
            base.String = message;
        }
    }

    public class NestedError : ErrorMessage {
        public Position Position { get; private set; }
        public object UserState { get; private set; }
        public ErrorMessageList Messages { get; private set; }

        public NestedError(Position position, object userState, ErrorMessageList messages) : base(ErrorMessageType.NestedError) {
            Position = position;
            UserState = userState;
            Messages = messages;
        }
    }

    public class CompoundError : ErrorMessage {
        public string LabelOfCompound { get { return String; } }

        public Position NestedErrorPosition { get; private set; }
        public object NestedErrorUserState { get; private set; }
        public ErrorMessageList NestedErrorMessages { get; private set; }

        public CompoundError(string labelOfCompound,
                             Position nestedErrorPosition,
                             object nestedErrorUserState,
                             ErrorMessageList nestedErrorMessages) : base(ErrorMessageType.CompoundError)
        {
            String = labelOfCompound;
            NestedErrorPosition = nestedErrorPosition;
            NestedErrorUserState = nestedErrorUserState;
            NestedErrorMessages = nestedErrorMessages;
        }
    }

    public class Other : ErrorMessage {
        public object Data { get; private set; }
        public Other(object data) : base(ErrorMessageType.Other) {
            Data = data;
        }
    }

    public override bool Equals(object obj) { return Equals(obj as ErrorMessage); }

    public bool Equals(ErrorMessage other) {
        return    (object)this == (object)other
               || (   (object)other != null
                   && Type == other.Type
                   && (Type > ErrorMessageType.Message
                       ? EqualsHelper(other)
                       : String == other.String));
    }

    public static bool operator==(ErrorMessage left, ErrorMessage right) {
        return    (object)left == (object)right
               || (   (object)left != null
                   && (object)right != null
                   && left.Type == right.Type
                   && (left.Type > ErrorMessageType.Message
                       ? left.EqualsHelper(right)
                       : left.String == right.String));
    }
    public static bool operator!=(ErrorMessage left, ErrorMessage right) { return !(left == right); }

    private bool EqualsHelper(ErrorMessage other) {
        Debug.Assert(Type == other.Type
                     && Type > ErrorMessageType.Message);
        if (Type == ErrorMessageType.NestedError) {
            var ne1 = (NestedError)this;
            var ne2 = (NestedError)other;
            return    ne1.Position  == ne2.Position
                   && ne1.Messages  == ne2.Messages
                   && LanguagePrimitives.GenericEqualityERComparer.Equals(ne1.UserState, ne2.UserState);
        } else if (Type == ErrorMessageType.CompoundError) {
            if (String != other.String) return false;
            var ce1 = (CompoundError)this;
            var ce2 = (CompoundError)other;
            return    ce1.NestedErrorPosition  == ce2.NestedErrorPosition
                   && ce1.NestedErrorMessages  == ce2.NestedErrorMessages
                   && LanguagePrimitives.GenericEqualityERComparer.Equals(ce1.NestedErrorUserState, ce2.NestedErrorUserState);
        } else { // ErrorMessageType == ErrorMessageType.Other
            Debug.Assert(Type == ErrorMessageType.Other);
            return ((Other)this).Data == ((Other)other).Data;
        }
    }

    public override int GetHashCode() {
        return (int)Type ^ (String == null ? 0 : String.GetHashCode());
    }

    private class ErrorMessageComparer : Comparer<ErrorMessage> {
        public override int Compare(ErrorMessage x, ErrorMessage y) {
            if (x == null || y == null) {
                return x == null && y == null ? 0 : (x == null ? -1 : 1);
            }
            int d = (int)x.Type - (int)y.Type;
            if (d != 0) return d;
            var type = x.Type;
            if (type <= ErrorMessageType.Message) {
                Debug.Assert(type >= 0);
                return String.CompareOrdinal(x.String, y.String);
            } else if (type == ErrorMessageType.NestedError) {
                var ne1 = (NestedError)x;
                var ne2 = (NestedError)y;
                var c = Position.Compare(ne1.Position, ne2.Position);
                if (c != 0) return c;
                var msgs1 = ErrorMessageList.ToSortedArray(ne1.Messages);
                var msgs2 = ErrorMessageList.ToSortedArray(ne2.Messages);
                int n = Math.Min(msgs1.Length, msgs2.Length);
                for (int i = 0; i < n; ++i) {
                    c = Compare(msgs1[i], msgs2[i]);
                    if (c != 0) return c;
                }
                return msgs1.Length - msgs2.Length;
            } else if (type == ErrorMessageType.CompoundError) {
                var c = String.CompareOrdinal(x.String, y.String);
                if (c != 0) return c;
                var ce1 = (CompoundError)x;
                var ce2 = (CompoundError)y;
                c = Position.Compare(ce1.NestedErrorPosition, ce2.NestedErrorPosition);
                if (c != 0) return c;
                var msgs1 = ErrorMessageList.ToSortedArray(ce1.NestedErrorMessages);
                var msgs2 = ErrorMessageList.ToSortedArray(ce2.NestedErrorMessages);
                int n = Math.Min(msgs1.Length, msgs2.Length);
                for (int i = 0; i < n; ++i) {
                    c = Compare(msgs1[i], msgs2[i]);
                    if (c != 0) return c;
                }
                return msgs1.Length - msgs2.Length;
            } else {
                Debug.Assert(type == ErrorMessageType.Other);
                return 0;
            }
        }
	}

    internal static Comparer<ErrorMessage> Comparer = new ErrorMessageComparer();
    internal static ErrorMessage[] EmptyArray = new ErrorMessage[0];

    internal string GetDebuggerDisplay() {
        switch (Type) {
            case ErrorMessageType.Expected:
                return String == null
                       ? "Expected(null)"
                       : Text.DoubleQuote("Expected(", String, ")");
            case ErrorMessageType.ExpectedString:
                return String == null
                       ? "ExpectedString(null)"
                       : Text.DoubleQuote("ExpectedString(", String, ")");
            case ErrorMessageType.ExpectedCaseInsensitiveString:
                return String == null
                       ? "ExpectedCaseInsensitiveString(null)"
                       : Text.DoubleQuote("ExpectedCaseInsensitiveString(", String, ")");
            case ErrorMessageType.Unexpected:
                return String == null
                       ? "Unexpected(null)"
                       : Text.DoubleQuote("Unexpected(", String, ")");
            case ErrorMessageType.UnexpectedString:
                return String == null
                       ? "UnexpectedString(null)"
                       : Text.DoubleQuote("UnexpectedString(", String, ")");
            case ErrorMessageType.UnexpectedCaseInsensitiveString:
                return String == null
                       ? "UnexpectedCaseInsensitiveString(null)"
                       : Text.DoubleQuote("UnexpectedCaseInsensitiveString(", String, ")");
            case ErrorMessageType.Message:
                return String == null
                       ? "Message(null)"
                       : Text.DoubleQuote("Message(", String, ")");
            case ErrorMessageType.NestedError: {
                var ne = (NestedError)this;
                var pos = ne.Position == null ? "null" : ne.Position.ToString();
                var msgs = ErrorMessageList.GetDebuggerDisplay(ne.Messages);
                return "NestedError(" + pos + ", ..., " + msgs +  ")";
            }
            case ErrorMessageType.CompoundError: {
                var ce = (CompoundError)this;
                var label = ce.String == null ? "null" : Text.Escape(ce.String, "", "\"", "\"", "", '"');
                var pos = ce.NestedErrorPosition == null ? "" : ce.NestedErrorPosition.ToString();
                var msgs = ErrorMessageList.GetDebuggerDisplay(ce.NestedErrorMessages);
                return "CompoundError(" + label +  ", " + pos + ", ..., " + msgs +  ")";
            }
            case ErrorMessageType.Other: {
                var oe = (Other)this;
                return oe.Data == null ? "Other(null)" : "Other(" + oe.ToString() + ")";
            }
            default:
                throw new InvalidOperationException();
        }
    }
}


}