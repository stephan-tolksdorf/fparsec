// Copyright (c) Stephan Tolksdorf 2010-2011
// License: Simplified BSD License. See accompanying documentation.

using System;

namespace FParsec {

internal static class Errors {
    static private ErrorMessageList Expected(string str) {
        return new ErrorMessageList(new ErrorMessage.Expected(str));
    }

    static private ErrorMessageList Unexpected(string str) {
        return new ErrorMessageList(new ErrorMessage.Unexpected(str));
    }

    static private ErrorMessageList Message(string str) {
        return new ErrorMessageList(new ErrorMessage.Message(str));
    }

    public static readonly ErrorMessageList ExpectedEndOfInput           = Expected(Strings.EndOfInput);
    public static readonly ErrorMessageList UnexpectedEndOfInput         = Unexpected(Strings.EndOfInput);

    public static readonly ErrorMessageList ExpectedAnyChar              = Expected(Strings.AnyChar);
    public static readonly ErrorMessageList ExpectedWhitespace           = Expected(Strings.Whitespace);
    public static readonly ErrorMessageList ExpectedAsciiUppercaseLetter = Expected(Strings.AsciiUppercaseLetter);
    public static readonly ErrorMessageList ExpectedAsciiLowercaseLetter = Expected(Strings.AsciiLowercaseLetter);
    public static readonly ErrorMessageList ExpectedAsciiLetter          = Expected(Strings.AsciiLetter);
    public static readonly ErrorMessageList ExpectedUppercaseLetter      = Expected(Strings.UppercaseLetter);
    public static readonly ErrorMessageList ExpectedLowercaseLetter      = Expected(Strings.LowercaseLetter);
    public static readonly ErrorMessageList ExpectedLetter               = Expected(Strings.Letter);
    public static readonly ErrorMessageList ExpectedBinaryDigit          = Expected(Strings.BinaryDigit);
    public static readonly ErrorMessageList ExpectedOctalDigit           = Expected(Strings.OctalDigit);
    public static readonly ErrorMessageList ExpectedDecimalDigit         = Expected(Strings.DecimalDigit);
    public static readonly ErrorMessageList ExpectedHexadecimalDigit     = Expected(Strings.HexadecimalDigit);

    public static readonly ErrorMessageList ExpectedNewline   = Expected(Strings.Newline);
    public static readonly ErrorMessageList UnexpectedNewline = Unexpected(Strings.Newline);

    public static readonly ErrorMessageList ExpectedTab     = Expected(Strings.Tab);

    public static readonly ErrorMessageList ExpectedFloatingPointNumber = Expected(Strings.FloatingPointNumber);

    public static readonly ErrorMessageList ExpectedInt64  = Expected(Strings.Int64);
    public static readonly ErrorMessageList ExpectedInt32  = Expected(Strings.Int32);
    public static readonly ErrorMessageList ExpectedInt16  = Expected(Strings.Int16);
    public static readonly ErrorMessageList ExpectedInt8   = Expected(Strings.Int8);
    public static readonly ErrorMessageList ExpectedUInt64 = Expected(Strings.UInt64);
    public static readonly ErrorMessageList ExpectedUInt32 = Expected(Strings.UInt32);
    public static readonly ErrorMessageList ExpectedUInt16 = Expected(Strings.UInt16);
    public static readonly ErrorMessageList ExpectedUInt8  = Expected(Strings.UInt8);

    public static readonly ErrorMessageList ExpectedPrefixOperator  = Expected(Strings.PrefixOperator);
    public static readonly ErrorMessageList ExpectedInfixOperator   = Expected(Strings.InfixOperator);
    public static readonly ErrorMessageList ExpectedPostfixOperator = Expected(Strings.PostfixOperator);
    public static readonly ErrorMessageList ExpectedInfixOrPostfixOperator = ErrorMessageList.Merge(ExpectedInfixOperator, ExpectedPostfixOperator);

    public static readonly ErrorMessageList NumberOutsideOfDoubleRange = Message(Strings.NumberOutsideOfDoubleRange);
    public static readonly ErrorMessageList NumberOutsideOfInt64Range  = Message(Strings.NumberOutsideOfInt64Range);
    public static readonly ErrorMessageList NumberOutsideOfInt32Range  = Message(Strings.NumberOutsideOfInt32Range);
    public static readonly ErrorMessageList NumberOutsideOfInt16Range  = Message(Strings.NumberOutsideOfInt16Range);
    public static readonly ErrorMessageList NumberOutsideOfInt8Range   = Message(Strings.NumberOutsideOfInt8Range);
    public static readonly ErrorMessageList NumberOutsideOfUInt64Range = Message(Strings.NumberOutsideOfUInt64Range);
    public static readonly ErrorMessageList NumberOutsideOfUInt32Range = Message(Strings.NumberOutsideOfUInt32Range);
    public static readonly ErrorMessageList NumberOutsideOfUInt16Range = Message(Strings.NumberOutsideOfUInt16Range);
    public static readonly ErrorMessageList NumberOutsideOfUInt8Range  = Message(Strings.NumberOutsideOfUInt8Range);


    public static ErrorMessageList ExpectedAnyCharIn(string chars) {
        return Expected(Strings.AnyCharIn(chars));
    }

    public static ErrorMessageList ExpectedAnyCharNotIn(string chars) {
        return Expected(Strings.AnyCharNotIn(chars));
    }

    public static ErrorMessageList ExpectedStringMatchingRegex(string regexPattern) {
        return Expected(Strings.StringMatchingRegex(regexPattern));
    }

    public static ErrorMessageList ExpectedAnySequenceOfNChars(int n) {
        return Expected(Strings.ExpectedAnySequenceOfNChars(n));
    }

    public static ErrorMessageList CouldNotFindString(string str) {
        return Message(Strings.CouldNotFindString(str));
    }

    public static ErrorMessageList CouldNotFindCaseInsensitiveString(string str) {
        return Message(Strings.CouldNotFindCaseInsensitiveString(str));
    }

    public static ErrorMessageList OperatorsConflict<T,W,U>(Position position1, Operator<T,W,U> operator1,
                                                            Position position2, Operator<T,W,U> operator2)
    {
        return Message(Strings.OperatorsConflict(position1, operator1, position2, operator2));
    }

    public static ErrorMessageList UnexpectedNonPrefixOperator<T,W,U>(Operator<T,W,U> op) {
        return new ErrorMessageList(
                   ExpectedPrefixOperator.Head,
                   new ErrorMessage.Unexpected(Strings.OperatorToString(op)));
    }

    public static ErrorMessageList MissingTernary2ndString<T,W,U>(Position position1, Position position2, Operator<T,W,U> op) {
        return new ErrorMessageList(
                   new ErrorMessage.ExpectedString(op.TernaryRightString),
                   new ErrorMessage.Message(Strings.OperatorStringIsRightPartOfTernaryOperator<T,W,U>(position1, position2, op)));
    }
}

namespace Internal { // the internal namespace contains internal types that must be public for inlining reasons
    public static class ParserCombinatorInInfiniteLoopHelper {
        public static Exception CreateException(string combinatorName, CharStream stream) {
            return new InvalidOperationException(stream.Position.ToString() + ": The combinator '" + combinatorName  + "' was applied to a parser that succeeds without consuming input and without changing the parser state in any other way. (If no exception had been raised, the combinator likely would have entered an infinite loop.)");
        }
    }
}

}
