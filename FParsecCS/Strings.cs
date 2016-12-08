// Copyright (c) Stephan Tolksdorf 2010-2011
// License: Simplified BSD License. See accompanying documentation.

using System;

namespace FParsec {

internal static class Strings {

    static internal string Quote(string stringToQuote) {
        return Text.SingleQuote(stringToQuote);
    }
    static internal string Quote(string prefix, string stringToQuote, string postfix) {
        return Text.SingleQuote(prefix, stringToQuote, postfix);
    }

    static internal string AsciiQuote(string prefix, string stringToQuote, string postfix) {
        return Text.AsciiEscape(stringToQuote, prefix, "'", "'", postfix, '\'');
    }

    static internal string QuoteCaseInsensitive(string caseInsensitiveStringToQuote) {
        return Quote("", caseInsensitiveStringToQuote, " (case-insensitive)");
    }

    static private string OrdinalEnding(int value) {
        if (value < 1) throw new ArgumentOutOfRangeException("value", "The value must be greater than 0.");
        var n100 = value%100;
        var n10 = value%10;
        if (n100 < 11 || n100 > 13) {
            if (n10 == 1) return "st";
            if (n10 == 2) return "nd";
            if (n10 == 3) return "rd";
        }
        return "th";
    }

    public static readonly string EndOfInput           = "end of input";
    public static readonly string AnyChar              = "any char";
    public static readonly string Whitespace           = "whitespace";
    public static readonly string AsciiUppercaseLetter = "Ascii uppercase letter";
    public static readonly string AsciiLowercaseLetter = "Ascii lowercase letter";
    public static readonly string AsciiLetter          = "Ascii letter";
    public static readonly string UppercaseLetter      = "uppercase letter";
    public static readonly string LowercaseLetter      = "lowercase letter";
    public static readonly string Letter               = "letter";
    public static readonly string BinaryDigit          = "binary digit";
    public static readonly string OctalDigit           = "octal digit";
    public static readonly string DecimalDigit         = "decimal digit";
    public static readonly string HexadecimalDigit     = "hexadecimal digit";
    public static readonly string Newline              = "newline";
    public static readonly string Tab                  = "tab";
    public static readonly string FloatingPointNumber  = "floating-point number";
    public static readonly string Int64                = "integer number (64-bit, signed)";
    public static readonly string Int32                = "integer number (32-bit, signed)";
    public static readonly string Int16                = "integer number (16-bit, signed)";
    public static readonly string Int8                 = "integer number (8-bit, signed)";
    public static readonly string UInt64               = "integer number (64-bit, unsigned)";
    public static readonly string UInt32               = "integer number (32-bit, unsigned)";
    public static readonly string UInt16               = "integer number (16-bit, unsigned)";
    public static readonly string UInt8                = "integer number (8-bit, unsigned)";

    public static readonly string Identifier           = "identifier";
    public static readonly string IdentifierContainsInvalidCharacterAtIndicatedPosition = "The identifier contains an invalid character at the indicated position.";


    public static readonly string NumberOutsideOfDoubleRange  = "This number is outside the allowable range for double precision floating-pointer numbers.";

    public static readonly string NumberOutsideOfInt64Range  = "This number is outside the allowable range for signed 64-bit integers.";
    public static readonly string NumberOutsideOfInt32Range  = "This number is outside the allowable range for signed 32-bit integers.";
    public static readonly string NumberOutsideOfInt16Range  = "This number is outside the allowable range for signed 16-bit integers.";
    public static readonly string NumberOutsideOfInt8Range   = "This number is outside the allowable range for signed 8-bit integers.";

    public static readonly string NumberOutsideOfUInt64Range = "This number is outside the allowable range for unsigned 64-bit integers.";
    public static readonly string NumberOutsideOfUInt32Range = "This number is outside the allowable range for unsigned 32-bit integers.";
    public static readonly string NumberOutsideOfUInt16Range = "This number is outside the allowable range for unsigned 16-bit integers.";
    public static readonly string NumberOutsideOfUInt8Range  = "This number is outside the allowable range for unsigned 8-bit integers.";

    public static readonly string InfixOperator   = "infix operator";
    public static readonly string TernaryOperator = "ternary operator";
    public static readonly string PrefixOperator  = "prefix operator";
    public static readonly string PostfixOperator = "postfix operator";

    private static readonly string AnyCharIn1 = "any char in ";
    private static readonly string AnyCharIn2 =                 "";

    private static readonly string AnyCharNotIn1 = "any char not in ";
    private static readonly string AnyCharNotIn2 =                     "";

    private static readonly string AnySequenceOfNChars1 = "any sequence of ";
    private static readonly string AnySequenceOfNChars2 =                     " chars";

    private static readonly string CouldNotFindString1 = "Could not find the string ";
    private static readonly string CouldNotFindString2 =                               ".";

    private static readonly string CouldNotFindCaseInsensitiveString1 = "Could not find the case-insensitive string ";
    private static readonly string CouldNotFindCaseInsensitiveString2 =                                               ".";

    private static readonly string StringMatchingRegex1 = "string matching the regex ";
    private static readonly string StringMatchingRegex2 =                               "";

    private static readonly string ErrorPositionStreamNameFormat = " {0}:";
    private static readonly string ErrorPositionUnaccountedNewlinesFormat = " (+{0})";
    private static readonly string ErrorPositionUtf16ColumnFormat = " (UTF16-Col: {0})";
    private static readonly string ErrorPositionFormat = "Error in{0} Ln: {1}{2} Col: {3}{4}";
                                                         // 0: ErrorPositionStreamName or ""
                                                         // 1: line
                                                         // 2: ErrorPositionUnaccountedNewlines or ""
                                                         // 3: column
                                                         // 4: ErrorPositionUtf16Col

    public static string ErrorPosition(Position position) {
        var name = string.IsNullOrEmpty(position.StreamName) ? "" : string.Format(ErrorPositionStreamNameFormat, position.StreamName);
        return string.Format(ErrorPositionFormat, name, position.Line, "", position.Column, "");
    }

    public static string ErrorPosition(Position position, int unaccountedNewlines, long column, long utf16Column) {
        var name = string.IsNullOrEmpty(position.StreamName) ? "" : string.Format(ErrorPositionStreamNameFormat, position.StreamName);
        var nlCorrection = unaccountedNewlines == 0 ? "" : string.Format(ErrorPositionUnaccountedNewlinesFormat, unaccountedNewlines);
        var utf16Col = column == utf16Column ? "" : string.Format(ErrorPositionUtf16ColumnFormat, utf16Column);
        return string.Format(ErrorPositionFormat, name, position.Line, nlCorrection, column, utf16Col);
    }

    public static readonly string Note = "Note: ";
    public static readonly string Expecting = "Expecting: ";
    public static readonly string Unexpected = "Unexpected: ";
    public static readonly string Comma = ", ";
    public static readonly string Or = " or ";
    public static readonly string And = " and ";
    private static readonly string CompoundCouldNotBeParsedBecauseFormat = "{0} could not be parsed because: ";

    public static string CompoundCouldNotBeParsedBecause(string compoundLabel) {
        return string.Format(CompoundCouldNotBeParsedBecauseFormat, compoundLabel);
    }

    public static readonly string ParserBacktrackedAfter = "The parser backtracked after: ";
    public static readonly string OtherErrors = "Other error messages: ";
    public static readonly string UnknownErrors = "Unknown Error(s)";
    public static readonly string Utf16ColumnCountOnlyCountsEachTabAs1Char = " The UTF-16 column count only counts each tab as 1 char.";
    public static readonly string ExactPositionBetweenCaretsDependsOnDisplayUnicodeCapabilities = "The exact error position between the two ^ depends on the unicode capabilities of the display.";
    public static readonly string ErrorOccurredAtEndOfInputStream    = "The error occurred at the end of the input stream.";
    public static readonly string ErrorOccurredOnAnEmptyLine         = "The error occurred on an empty line.";
    public static readonly string ErrorOccurredAtEndOfLine           = "The error occurred at the end of the line.";
    public static readonly string ErrorOccurredAtSecondCharInNewline = "The error occured at the 2nd char in the newline char sequence '\r\n'.";

    private static readonly string NonAssociative = "non-associative";
    private static readonly string LeftAssociative = "left-associative";
    private static readonly string RightAssociative = "right-associative";

    private static readonly string OperatorToStringFormat = "{0} {1} (precedence: {2}{3}{4})";
                                                            // 0: InfixOperator/TernaryOperator/...
                                                            // 1: operator strings
                                                            // 2: precedence
                                                            // 3: Comma if 4 is not empty, otherwise empty
                                                            // 4: LeftAssociative/RightAssociative/... or empty if operator is an associative prefix or postfix operator

                                                                   // It would be more precise to write "UTF-16 colum" here,
                                                                   // but that would probably only confuse users in most situations.
    private static readonly string RelativePositionOnTheSameLine   = "on the same line at column {0}";
    private static readonly string RelativePositionOnPreviousLine  = "on the previous line column {0}";
    private static readonly string RelativePositionOnLineAbove     = "{0} lines above column {1}";
    private static readonly string RelativePositionOnDifferentLine = "at (Ln: {0}, Col: {1} )";
    private static readonly string RelativePositionInDifferentFile = "at ({0}, Ln: {1}, Col: {2})";

    private static readonly string OperatorsConflictsFormat = "The {1} conflicts with the {0} {2}.";
                                                               // 0: previous operator
                                                               // 1: current operator
                                                               // 2: relative position of previous operator

    private static readonly string OperatorStringIsRightPartOfTernaryOperatorFormat = "{0} is the right part of the ternary operator {1}. The left part is {2}.";


    private static readonly string ColumnCountAssumesTabStopDistanceOfNChars1 = "The column count assumes a tab stop distance of ";
    private static readonly string ColumnCountAssumesTabStopDistanceOfNChars2 =                                                     " chars.";

    private static readonly string ErrorOccurredAtNthCharInCombiningCharacterSequence1 = "The error occurred at the ";
    private static readonly string ErrorOccurredAtNthCharInCombiningCharacterSequence2 =                               " char in the combining character sequence ";
    private static readonly string ErrorOccurredAtNthCharInCombiningCharacterSequence3 =                                                                             ".";

    private static readonly string InputContainsAtLeastNUnaccountedNewlines1         = "The input contains at least ";
    private static readonly string InputContainsAtLeastNUnaccountedNewlines2Singular =                                 " newline in the input that wasn't properly registered in the parser stream state.";
    private static readonly string InputContainsAtLeastNUnaccountedNewlines2Plural   =                                 " newlines in the input that weren't properly registered in the parser stream state.";

    private static readonly string ErrorOccurredAtBeginningOfSurrogatePair1 = "The error occurred at the beginning of the surrogate pair ";
    private static readonly string ErrorOccurredAtBeginningOfSurrogatePair2 =                                                               ".";

    private static readonly string ErrorOccurredAtSecondCharInSurrogatePair1 = "The error occurred at the second char in the surrogate pair ";
    private static readonly string ErrorOccurredAtSecondCharInSurrogatePair2 =                                                                 ".";

    private static readonly string CharAtErrorPositionIsIsolatedHighSurrogate1 = "The char at the error position ('";
    private static readonly string CharAtErrorPositionIsIsolatedHighSurrogate2 =                                     "') is an isolated high surrogate.";

    private static readonly string CharAtErrorPositionIsIsolatedLowSurrogate1 = "The char at the error position ('";
    private static readonly string CharAtErrorPositionIsIsolatedLowSurrogate2 =                                      "') is an isolated low surrogate.";

    private static readonly string CharBeforeErrorPositionIsIsolatedHighSurrogate1 = "The char before the error position ('";
    private static readonly string CharBeforeErrorPositionIsIsolatedHighSurrogate2 =                                         "') is an isolated high surrogate.";

    private static readonly string CharBeforeErrorPositionIsIsolatedLowSurrogate1 = "The char before the error position ('";
    private static readonly string CharBeforeErrorPositionIsIsolatedLowSurrogate2 =                                         "') is an isolated low surrogate.";


    public static string AnyCharIn(string chars) {
        //return Quote(Strings.AnyCharIn1, chars, Strings.AnyCharIn2);
        return Strings.AnyCharIn1 + "‘" + chars + "’" + Strings.AnyCharIn2; // Review: Should we use different quotes if the string contains ‘ or ’ chars?
    }

    public static string AnyCharNotIn(string chars) {
        //return Quote(Strings.AnyCharNotIn1, chars, Strings.AnyCharNotIn2);
        return Strings.AnyCharNotIn1 + "‘" + chars + "’" + Strings.AnyCharNotIn2;
    }

    public static string StringMatchingRegex(string regexPattern) {
        return Quote(Strings.StringMatchingRegex1, regexPattern, Strings.StringMatchingRegex2);
    }

    public static string ExpectedAnySequenceOfNChars(int n) {
        return Strings.AnySequenceOfNChars1 + n.ToString() + Strings.AnySequenceOfNChars2;
    }

    public static string CouldNotFindString(string str) {
        return Quote(Strings.CouldNotFindString1, str, Strings.CouldNotFindString2);
    }

    public static string CouldNotFindCaseInsensitiveString(string str) {
        return Quote(Strings.CouldNotFindCaseInsensitiveString1, str, Strings.CouldNotFindCaseInsensitiveString2);
    }

    internal static string OperatorToString<T,W,U>(Operator<T,W,U> op) {
        var type = op.Type == OperatorType.Infix ? (op.IsTernary ? TernaryOperator : InfixOperator) :
                   op.Type == OperatorType.Prefix ? PrefixOperator : PostfixOperator;
        var opString = op.IsTernary ? Quote(Quote("", op.String, " "), op.TernaryRightString, "") : Quote(op.String);
        var comma = op.Type != OperatorType.Infix && op.IsAssociative ? "" : Comma;
        var assoc = op.Type != OperatorType.Infix
                    ? (op.IsAssociative ? "" : NonAssociative)
                    : (op.Associativity == Associativity.Left ? LeftAssociative :
                       op.Associativity == Associativity.Right ? RightAssociative : NonAssociative);
        return String.Format(OperatorToStringFormat, type, opString, op.Precedence, comma, assoc);
    }

    private static string RelativePosition(Position previousPosition, Position currentPosition) {
        if (previousPosition.StreamName == currentPosition.StreamName) {
            if (previousPosition.Line == currentPosition.Line)
                return String.Format(RelativePositionOnTheSameLine, previousPosition.Column);
            long diff = currentPosition.Line - previousPosition.Line;
            if (diff == 1)
                return String.Format(RelativePositionOnPreviousLine, previousPosition.Column);
            if (diff <= 3)
                return String.Format(RelativePositionOnLineAbove, diff, previousPosition.Column);
            return String.Format(RelativePositionOnDifferentLine, previousPosition.Line, previousPosition.Column);
        }
        return String.Format(RelativePositionInDifferentFile, Quote(previousPosition.StreamName), previousPosition.Line, previousPosition.Column);
    }

    public static string OperatorsConflict<T,W,U>(Position previousPosition, Operator<T,W,U> previousOperator,
                                                  Position currentPosition, Operator<T,W,U> currentOperator)
    {
        var prevOpString = OperatorToString(previousOperator);
        var currentOpString = OperatorToString(currentOperator);
        var relativePosition = RelativePosition(previousPosition, currentPosition);
        return String.Format(OperatorsConflictsFormat, prevOpString, currentOpString, relativePosition);
    }

    public static string OperatorStringIsRightPartOfTernaryOperator<T,W,U>(Position position1, Position position2, Operator<T,W,U> op) {
        return String.Format(OperatorStringIsRightPartOfTernaryOperatorFormat,
                             Quote(op.TernaryRightString),
                             Quote(Quote("", op.String, " "), op.TernaryRightString, ""),
                             RelativePosition(position1, position2));
    }

    public static string ColumnCountAssumesTabStopDistanceOfNChars(int n) {
        return ColumnCountAssumesTabStopDistanceOfNChars1 + n.ToString() + ColumnCountAssumesTabStopDistanceOfNChars2;
    }

    public static string ErrorOccurredAtNthCharInCombiningCharacterSequence(int n, string textElement) {
        return AsciiQuote(ErrorOccurredAtNthCharInCombiningCharacterSequence1 + n.ToString() + OrdinalEnding(n) + ErrorOccurredAtNthCharInCombiningCharacterSequence2,
                          textElement,
                          ErrorOccurredAtNthCharInCombiningCharacterSequence3);
    }

    public static string InputContainsAtLeastNUnaccountedNewlines(int n) {
        return InputContainsAtLeastNUnaccountedNewlines1 + n.ToString() + (n == 1 ? InputContainsAtLeastNUnaccountedNewlines2Singular
                                                                                  : InputContainsAtLeastNUnaccountedNewlines2Plural);
    }

    public static string ErrorOccurredAtBeginningOfSurrogatePair(string surrogatePair) {
        return AsciiQuote(ErrorOccurredAtBeginningOfSurrogatePair1, surrogatePair, ErrorOccurredAtBeginningOfSurrogatePair2);
    }


    public static string ErrorOccurredAtSecondCharInSurrogatePair(string surrogatePair) {
        return AsciiQuote(ErrorOccurredAtSecondCharInSurrogatePair1, surrogatePair, ErrorOccurredAtSecondCharInSurrogatePair2);
    }


    public static string CharAtErrorPositionIsIsolatedHighSurrogate(char ch) {
        return CharAtErrorPositionIsIsolatedHighSurrogate1 + Text.HexEscape(ch) + CharAtErrorPositionIsIsolatedHighSurrogate2;
    }

    public static string CharAtErrorPositionIsIsolatedLowSurrogate(char ch) {
        return CharAtErrorPositionIsIsolatedLowSurrogate1 + Text.HexEscape(ch) + CharAtErrorPositionIsIsolatedLowSurrogate2;
    }

    public static string CharBeforeErrorPositionIsIsolatedHighSurrogate(char ch) {
        return CharBeforeErrorPositionIsIsolatedHighSurrogate1 + Text.HexEscape(ch) + CharBeforeErrorPositionIsIsolatedHighSurrogate2;
    }

    public static string CharBeforeErrorPositionIsIsolatedLowSurrogate(char ch) {
        return CharBeforeErrorPositionIsIsolatedLowSurrogate1 + Text.HexEscape(ch) + CharBeforeErrorPositionIsIsolatedLowSurrogate2;
    }


}

}

