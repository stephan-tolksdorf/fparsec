// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

module FParsec.CharParsers

open System.Text.RegularExpressions

open Error
open Primitives


// ========================
// Running parsers on input
// ========================

/// Values of this type are returned by the runParser functions (not by `Parser<_,_>` functions).
type ParserResult<'Result,'UserState> =
     /// Success(result, userState, endPos) holds the result and the user state returned by a successful parser,
     /// together with the position where the parser stopped.
     | Success of 'Result * 'UserState * Position
     /// Failure(errorAsString, error, suserState) holds the parser error and the user state returned by a failing parser,
     /// together with a string representation of the parser error.
     | Failure of string * ParserError * 'UserState

// Note that `ParserResult` and `ParserError` values contain no references to the input
// `CharStream` or to any `State<_>` value with a nested `CharStream` reference.
// This is important because most of the `runParser` functions dispose the `CharStream`
// used as the input source before they return the `ParserResult` value.

/// `runParser p ustate streamName charStream` runs the parser `p` on the content of `charStream`,
/// starting with the initial user state `ustate`. The `streamName` is used in error messages
/// to describe the source of the input (e.g. a file path) and may be empty.
/// The parser's `Reply` is captured and returned as a `ParserResult` value.
/// This function *does not* dispose the `CharStream` after use.
val runParser: Parser<'a,'u> -> 'u -> streamName: string -> CharStream -> ParserResult<'a,'u>

/// `runParserOnString p ustate streamName str` runs the parser `p` directly on the content of the string `str`,
/// starting with the initial user state `ustate`. The `streamName` is used in error messages to describe
/// the source of the input (e.g. a file path) and may be empty.
/// The parser's `Reply` is captured and returned as a `ParserResult` value.
val runParserOnString: Parser<'a,'u> -> 'u -> streamName: string -> string -> ParserResult<'a,'u>

/// `runParserOnSubstring p ustate streamName str index count` runs the parser `p` directly on the content
/// of the string `str` between the indices `index` (inclusive) and `index + count` (exclusive),
/// starting with the initial user state `ustate`. The `streamName` is used in error messages to describe
/// the source of the input (e.g. a file path) and may be empty.
/// The parser's `Reply` is captured and returned as a `ParserResult` value.
val runParserOnSubstring: Parser<'a,'u> -> 'u -> streamName: string -> string -> int -> int -> ParserResult<'a,'u>

/// `runParserOnStream p ustate streamName stream encoding` runs the parser `p` on the content of
/// the `System.IO.Stream` `stream`, starting with the initial user state `ustate`. The `streamName`
/// is used in error messages to describe the source of the input (e.g. a file path) and may be empty.
/// In case no unicode byte order marks are found, the stream data is assumed to be encoded with the given `encoding`.
/// The parser's `Reply` is captured and returned as a `ParserResult` value.
val runParserOnStream:    Parser<'a,'u> -> 'u -> streamName: string -> System.IO.Stream -> System.Text.Encoding -> ParserResult<'a,'u>

/// `runParserOnFile p ustate path encoding` runs the parser `p` on the content of the file
/// at the given `path`, starting with the initial user state `ustate`.
/// In case no unicode byte order marks are found, the file data is assumed to be encoded with the given `encoding`.
/// The parser's `Reply` is captured and returned as a `ParserResult` value.
val runParserOnFile: Parser<'a,'u> -> 'u -> path: string -> System.Text.Encoding -> ParserResult<'a,'u>

/// `runParserOnSubStream p stateBegin stateEnd` runs the parser `p` on the `CharStream` content
/// between the position of `stateBegin` (inclusive) and `stateEnd` (exclusive). The substream appears
/// to the parser as a new and independent stream, although the position information
/// (index, column and line count etc.) reflects the position within the original stream.
/// The parser's `Reply` is captured and returned as a `ParserResult` value.
///
/// If the underlying `CharStream` has more than one block, `runParserOnSubStream` will copy
/// the content of the substream before applying the parser.
val runParserOnSubstream: Parser<'a,'u2> -> 'u2
                          -> stateAtFirstChar: State<'u> -> stateAfterLastChar: State<'u>
                          -> ParserResult<'a,'u2>

/// `run parser str` is equivalent to `runParserOnString parser () "" str`.
/// This function is mainly meant for testing purposes or interactive usage.
val run: Parser<'Result, unit> -> string -> ParserResult<'Result,unit>


// =======
// Parsers
// =======


// -------------------------------------------------------------
// Reading the input stream position and handling the user state
// -------------------------------------------------------------

/// The parser `getPosition` returns the current position in the input Stream.
/// `getPosition` is equivalent to `fun state -> Reply(state.Position, state)`.
val getPosition: Parser<Position,'u>

[<System.Obsolete("FParsec.CharParsers.getPos has been renamed to FParsec.CharParsers.getPosition.")>]
val getPos: Parser<Position,'u>

/// The parser `getUserState` returns the current user state.
/// `getUserState` is equivalent to `fun state -> Reply(state.UserState, state)`.
val getUserState: Parser<'u,'u>

/// The parser `setUserState u` sets the user state to `u`.
/// `setUserState u` is equivalent to `fun state -> Reply((), state.WithUserState(u))`.
val setUserState: 'u -> Parser<unit,'u>

/// `updateUserState f` is equivalent to `fun state -> Reply((), state.WithUserState(f state.UserState))`.
val updateUserState: ('u -> 'u) -> Parser<unit,'u>

/// The parser `userStateSatisfies f` succeeds if `f` returns `true`
/// when applied to the current user state, otherwise it fails.
val userStateSatisfies: ('u -> bool) -> Parser<unit,'u>


// --------------------
// Parsing single chars
// --------------------

/// `pchar c` parses the char `c` and returns `c`.
/// If `c = '\r'` or `c = '\n'` then `pchar c` will parse any one newline ("\n", "\r\n" or "\r") and return `c`.
val pchar:    char -> Parser<char,'u>

/// `skipChar c` is an optimized implementation of `pchar c |>> ignore`.
val skipChar: char -> Parser<unit,'u>

/// `charReturn c x` is an optimized implementation of `pchar c >>% x`.
val charReturn: char -> 'a -> Parser<'a,'u>

/// `anyChar` parses any single char or newline ("\n", "\r\n" or "\r").
/// Returns the parsed char, or '\n' in case a newline was parsed.
val anyChar: Parser<char,'u>

/// `skipAnyChar` is an optimized implementation of `anyChar |>> ignore`.
val skipAnyChar: Parser<unit,'u>


/// `satisfy f` parses any one char or newline for which the predicate function `f` returns `true`.
/// It returns the parsed char.
/// Any newline ("\n", "\r\n" or "\r") is converted to the single char '\n'.
/// Thus, to accept a newline `f '\n'` must return `true`. `f` will never be called
/// with '\r' and `satisfy f` will never return '\r'.
val satisfy:      (char -> bool)           -> Parser<char,'u>

/// `skipSatisfy f` is an optimized implementation of `satisfy f |>> ignore`.
val skipSatisfy:  (char -> bool)           -> Parser<unit,'u>

/// `satisfy f label` is an optimized implementation of `satisfy f <?> label`.
val satisfyL:     (char -> bool) -> string -> Parser<char,'u>

/// `skipSatisfyL f label` is an optimized implementation of `skipSatisfy f <?> label`.
val skipSatisfyL: (char -> bool) -> string -> Parser<unit,'u>


/// `anyOf str` parses any char contained in the string `str`. It returns the parsed char.
/// If `str` contains the char '\n', `anyOf str` parses any newline ("\n", "\r\n" or "\r")
/// and returns it as '\n'. (Note that it does not make a difference whether or not
/// `str` contains '\r'; `anyOf str` will never return '\r'.)
val anyOf: string -> Parser<char,'u>

/// `skipAnyOf str` is an optimized implementation of `anyOf str |>> ignore`.
val skipAnyOf:  string -> Parser<unit,'u>

/// `noneOf str` parses any char not contained in the string `str`. It returns the parsed char.
/// If `str` does not contain the char '\n', `noneOf str` parses any newline ("\n", "\r\n" or "\r")
/// and returns it as  as '\n'. (Note that it does not make a difference whether or not
/// `str` contains '\r'; `noneOf str` will never return '\r'.)
val noneOf:     string -> Parser<char,'u>

/// `skipNoneOf s` is an optimized implementation of `noneOf s |>> ignore`.
val skipNoneOf: string -> Parser<unit,'u>


/// Parses any char in the range 'A' - 'Z'. Returns the parsed char.
val asciiUpper: Parser<char,'u>

/// Parses any char in the range 'a' - 'z'. Returns the parsed char.
val asciiLower: Parser<char,'u>

/// Parses any char in the range 'a' - 'z' and 'A' - 'Z'. Returns the parsed char.
val asciiLetter: Parser<char,'u>

/// Parses any unicode uppercase letter char identified by `System.Char.IsUpper`.
/// Returns the parsed char.
val upper: Parser<char,'u>

/// Parses any unicode lowercase letter char identified by `System.Char.IsLower`.
/// Returns the parsed char.
val lower: Parser<char,'u>

/// Parses any unicode letter char identified by `System.Char.IsLetter`.
/// Returns the parsed char.
val letter: Parser<char,'u>

/// Parses any char in the range '0' - '9'. Returns the parsed char.
val digit: Parser<char,'u>

/// Parses any char in the range '0' - '9', 'a' - 'f' and 'A' - 'F'. Returns the parsed char.
val hex: Parser<char,'u>

/// Parses any char in the range '0' - '7'. Returns the parsed char.
val octal: Parser<char,'u>

// predicate functions corresponding to the above parsers

/// `isAnyOf str` returns a predicate function.
/// When this predicate function is applied to a char, it returns `true` if and only if the char is contained in `str`.
val isAnyOf: string -> (char -> bool)
/// `isNoneOf str` returns a predicate function.
/// When this predicate function is applied to a char, it returns `true` if and only if the char is not contained in `str`.
val isNoneOf: string -> (char -> bool)
/// Returns `true` for any char in the range 'A' - 'Z' and `false` for all other chars.
val inline isAsciiUpper:    char -> bool
/// Returns `true` for any char in the range 'a' - 'z' and `false` for all other chars.
val inline isAsciiLower:    char -> bool
/// Returns `true` for any char in the range 'a' - 'z', 'A' - 'Z' and `false` for all other chars.
val inline isAsciiLetter:   char -> bool
/// `isUpper` is equivalent to `System.Char.IsUpper`.
val inline isUpper:         char -> bool
/// `isLower` is equivalent to `System.Char.IsLower`.
val inline isLower:         char -> bool
/// `isLetter` is equivalent to `System.Char.IsLetter`.
val inline isLetter:        char -> bool
/// Returns `true` for any char in the range '0' - '9' and `false` for all other chars.
val inline isDigit:         char -> bool
/// Returns `true` for any char in the range '0' - '9', 'a' - 'f', 'A' - 'F' and `false` for all other chars.
val inline isHex:           char -> bool
/// Returns `true` for any char in the range '0' - '7' and `false` for all other chars.
val inline isOctal:         char -> bool


// ------------------
// Parsing whitespace
// ------------------

/// Parses the tab char '\t' and returns '\t'. Note that a tab char is treated like any other non-newline char:
/// the column number is incremented by (only) 1.
val tab: Parser<char,'u>

/// Parses a newline ("\n", "\r\n" or "\r"). Returns '\n'.
/// Is equivalent to `pchar '\n'`.
val newline<'u> : Parser<char,'u>

/// `skipNewline` is an optimized implementation of `newline |>> ignore`.
val skipNewline<'u> : Parser<unit,'u>

/// `newlineReturn x` is an optimized implementation of `newline >>% x`.
val newlineReturn: 'a -> Parser<'a,'u>

/// Parses a unicode newline ("\n", "\r\n", "\r", `"\u0085"`, `"\u000C"`, `"\u2028"`, or `"\u2029"`).
/// Returns '\n'.
/// In contrast to all other parsers in FParsec except `unicodeWhitespace` this parser
/// also increments the internal line count for unicode newline characters other than '\n' and '\r'.
val unicodeNewline: Parser<char,'u>

/// Parses a space (' '), a tab ('\t') or a newline ("\n", "\r\n" or "\r").
/// Returns '\n' for a newline and otherwise the parsed char.
val whitespace: Parser<char,'u>

/// Parses a unicode newline ("\n", "\r\n", "\r", `"\u0085"`, `"\u000C"`,
/// `"\u2028"`, or `"\u2029"`) or any whitespace char identified by `System.Char.IsWhiteSpace`.
/// Returns '\n' for a newline and otherwise the parsed char.
/// In contrast to all other parsers in FParsec except `unicodeNewline` this parser
/// also increments the internal line count for unicode newline characters other than '\n' and '\r'.
val unicodeWhitespace: Parser<char,'u>

/// Skips over any sequence of *zero* or more whitespaces (space (' '), tab ('\t')
/// or newline ("\n", "\r\n" or "\r")).
val spaces: Parser<unit,'u>

/// Skips over any sequence of *one* or more whitespaces (space (' '), tab('\t')
/// or newline ("\n", "\r\n" or "\r")).
val spaces1: Parser<unit,'u>

/// The parser `eof` only succeeds at the end of the input. It never consumes input.
val eof: Parser<unit,'u>


// ------------------------
// Parsing strings directly
// ------------------------

/// `pstring str` parses the string `str` and returns `str`.
/// It is an atomic parser: either it succeeds or it fails without consuming any input.
/// `str` may not contain newline chars (`'\n'` or `'\r'`).
val pstring:    string -> Parser<string,'u>
/// `skipString str` is an optimized implementation of `pstring str |>> ignore`.
val skipString: string -> Parser<unit,'u>
/// `stringReturn str x` is an optimized implementation of `pstring str >>% x`.
val stringReturn: string -> 'a -> Parser<'a,'u>

/// `pstringCI str` parses any string that case-insensitively matches the string `str`.
/// It returns the *parsed* string.
/// `str` may not contain newline chars (`'\n'` or `'\r'`).
val pstringCI:    string -> Parser<string,'u>
/// `skipStringCI str` is an optimized implementation of `pstringCI str |>> ignore`.
val skipStringCI: string -> Parser<unit,'u>
/// `stringCIReturn str x` is an optimized implementation of `pstringCI str >>% x`.
val stringCIReturn: string -> 'a -> Parser<'a,'u>

/// `anyString n` parses any sequence of `n` chars or newlines ("\n", "\r\n" or "\r").
/// It returns the parsed string. In the returned string all newlines are normalized to "\n".
/// `anyString n` is an atomic parser: either it succeeds or it fails without consuming any input.
val anyString:  int32  -> Parser<string,'u>
/// `skipAnyString n` is an optimized implementation of `anyString n |>> ignore`.
val skipAnyString:  int32  -> Parser<unit,'u>

/// Parses any chars before the end of the line and skips to the beginning of the next line (if there is one).
/// The line is terminated by a newline ("\n", "\r\n" or "\r") or the end of the input stream.
/// Returns the parsed chars before the end of the line as a string (without a newline).
val restOfLine: Parser<string,'u>

/// `skipRestOfLine` is an optimized implementation of `restOfLine |>> ignore`.
val skipRestOfLine: Parser<unit,'u>

/// Skips over any chars before the end of the line. Stops before the newline ("\n", "\r\n" or "\r")
/// or at the end of the input stream if there is no newline.
val skipToEndOfLine: Parser<unit,'u>

/// `skipToString str nMax` skips over all chars before the first occurance of the string `str`.
/// If more than `nMax` chars come before the first occurance of `str`, the parser *fails after consuming* `nMax` chars.
/// Newlines ("\n", "\r\n" or "\r") are counted as single chars.
/// `skipToString str nMax` throws an `ArgumentOutOfRangeException` if `nMax` is negative.
val skipToString:        string -> int -> Parser<unit,'u>
/// `charsTillString str nMax` parses all chars before the first occurance of the string `str`and
/// then skips over `str`. It returns the parsed chars before the string.
/// If more than `nMax` chars  come before the first occurance of `str`, the parser *fails after consuming* `nMax` chars.
/// Newlines ("\n", "\r\n" or "\r") are counted as single chars and
/// in the returned string all newlines are normalized to "\n".
/// `charsTillString str nMax` throws an `ArgumentOutOfRangeException` if `nMax` is negative.
val charsTillString:     string -> int -> Parser<string,'u>
/// `skipCharsTillString str nMax` is an optimized implementation of `charsTillString str nMax |>> ignore`.
val skipCharsTillString: string -> int -> Parser<unit,'u>

/// `skipToStringCI str nMax` skips over all chars before the first case-insensitive occurance of the string `str`.
/// If more than `nMax` chars come before the first case-insensitive occurance of `str`,
/// the parser *fails after consuming* `nMax` chars.
/// Newlines ("\n", "\r\n" or "\r") are counted as single chars.
/// `skipToStringCI str nMax` throws an `ArgumentOutOfRangeException` if `nMax` is negative.
val skipToStringCI:        string -> int -> Parser<unit,'u>
/// `charsTillStringCI str nMax` parses all chars before the first case-insensitive occurance of the string `str`
/// and then skips over it. It returns the parsed chars before the string.
/// If more than `nMax` chars come before the first case-insensitive occurance of `str`,
/// the parser *fails* after consuming `nMax` chars.
/// Newlines ("\n", "\r\n" or "\r") are counted as single chars and
/// in the returned string all newlines are normalized to "\n".
/// `charsTillStringCI str nMax` throws an `ArgumentOutOfRangeException` if `nMax` is negative.
val charsTillStringCI:     string -> int -> Parser<string,'u>
/// `skipCharsTillStringCI str nMax` is an optimized implementation of `charsTillStringCI str nMax |>> ignore`.
val skipCharsTillStringCI: string -> int -> Parser<unit,'u>

/// `manySatisfy f` parses a sequence of *zero* or more chars that satisfy the predicate function `f`
/// (i.e.  chars for which `f` returns `true`). It returns the parsed chars as a string.
///
/// Any newline ("\n", "\r\n" or "\r") is converted to the single char '\n'.
/// Thus, to accept a newline `f '\n'` must return `true`. `f` will never be called
/// with '\r' and the string returned by `manySatisfy f` will never contain an '\r'.
val manySatisfy:        (char -> bool)                   -> Parser<string,'u>
/// `manySatisfy2 f1 f` behaves like `manySatisfy f`, except that the
/// first char of the parsed string must satisfy `f1` instead of `f`.
val manySatisfy2:       (char -> bool) -> (char -> bool) -> Parser<string,'u>
/// `skipManySatisfy f` is an optimized implementation of `manySatisfy f |>> ignore`.
val skipManySatisfy:    (char -> bool)                   -> Parser<unit,'u>
/// `skipManySatisfy2 f1 f` is an optimized implementation of `manySatisfy2 f1 f |>> ignore`.
val skipManySatisfy2:   (char -> bool) -> (char -> bool) -> Parser<unit,'u>

/// `many1Satisfy f` parses a sequence of *one* or more chars that satisfy the predicate function `f`
/// (i.e. chars for which `f` returns `true`). It returns the parsed chars as a string.
/// If the first char does not satisfy `f`, this parser fails without consuming input.
///
/// Any newline ("\n", "\r\n" or "\r") is converted to the single char '\n'.
/// Thus, to accept a newline `f '\n'` must return `true`. `f` will never be called
/// with '\r' and the string returned by `many1Satisfy f` will never contain an '\r'.
val many1Satisfy:       (char -> bool)                   -> Parser<string,'u>
/// `many1Satisfy2 f1 f` behaves like `many1Satisfy f`, except that the
/// first char of the parsed string must satisfy `f1` instead of `f`.
val many1Satisfy2:      (char -> bool) -> (char -> bool) -> Parser<string,'u>
/// `skipMany1Satisfy f` is an optimized implementation of `many1Satisfy f |>> ignore`.
val skipMany1Satisfy:   (char -> bool)                   -> Parser<unit,'u>
/// `skipMany1Satisfy2 f1 f` is an optimized implementation of `many1Satisfy2 f1 f |>> ignore`.
val skipMany1Satisfy2:  (char -> bool) -> (char -> bool) -> Parser<unit,'u>

/// `many1SatisfyL f label` is an optimized implementation of `many1Satisfy f <?> label`.
val many1SatisfyL:      (char -> bool)                   -> string -> Parser<string,'u>
/// `many1Satisfy2L f1 f label` is an optimized implementation of `many1Satisfy2 f1 f <?> label`.
val many1Satisfy2L:     (char -> bool) -> (char -> bool) -> string -> Parser<string,'u>
/// `skipMany1SatisfyL f label` is an optimized implementation of `skipMany1Satisfy f <?> label`.
val skipMany1SatisfyL:  (char -> bool)                   -> string -> Parser<unit,'u>
/// `skipMany1Satisfy2L f1 f label` is an optimized implementation of `skipMany1Satisfy2 f1 f <?> label`.
val skipMany1Satisfy2L: (char -> bool) -> (char -> bool) -> string -> Parser<unit,'u>

/// `manyMinMaxSatisfy nMin nMax f` parses a sequence of `nMin` or more chars that satisfy the
/// predicate function `f` (i.e. chars for which `f` returns `true`), but not more than `nMax` chars.
/// It returns the parsed chars as a string. This parser is atomic, i.e. if the first `nMin` chars
/// do not all satisfy `f`, the parser fails without consuming any input.
///
/// Any newline ("\n", "\r\n" or "\r") is converted to the single char '\n'.
/// Thus, to accept a newline `f '\n'` must return `true`. `f` will never be called with '\r'
/// and the string returned by `manyMinMaxSatisfy nMin nMax f` will never contain an '\r'.
///
/// `manyMinMaxSatisfy` throws an `ArgumentOutOfRangeException` if `nMax` is negative.
val manyMinMaxSatisfy:       int -> int -> (char -> bool)                   -> Parser<string,'u>
/// `manyMinMaxSatisfy2 nMin nMax f1 f` behaves like `manyMinMaxSatisfy nMin nMax f`, except that the first char of the parsed string must satisfy `f1` instead of `f`.
val manyMinMaxSatisfy2:      int -> int -> (char -> bool) -> (char -> bool) -> Parser<string,'u>
/// `skipManyMinMaxSatisfy nMin nMax f` is an optimized implementation of `manyMinMaxSatisfy nMin nMax f |>> ignore`.
val skipManyMinMaxSatisfy:   int -> int -> (char -> bool)                   -> Parser<unit,'u>
/// `skipManyMinMaxSatisfy2 nMin nMax f1 f` is an optimized implementation of `manyMinMaxSatisfy2 nMin nMax f1 f |>> ignore`.
val skipManyMinMaxSatisfy2:  int -> int -> (char -> bool) -> (char -> bool) -> Parser<unit,'u>

/// `manyMinMaxSatisfyL nMin nMax f label` is an optimized implementation of `manyMinMaxSatisfy nMin nMax f <|> label`.
val manyMinMaxSatisfyL:      int -> int -> (char -> bool)                   -> string -> Parser<string,'u>
/// `manyMinMaxSatisfy2L nMin nMax f1 f label` is an optimized implementation of `manyMinMaxSatisfy2 nMin nMax f1 f <|> label`.
val manyMinMaxSatisfy2L:     int -> int -> (char -> bool) -> (char -> bool) -> string -> Parser<string,'u>
/// `skipManyMinMaxSatisfyL nMin nMax f label` is an optimized implementation of `skipManyMinMaxSatisfy nMin nMax f <|> label`.
val skipManyMinMaxSatisfyL:  int -> int -> (char -> bool)                   -> string -> Parser<unit,'u>
/// `skipManyMinMaxSatisfy2L nMin nMax f1 f label` is an optimized implementation of `skipManyMinMaxSatisfy2 nMin nMax f1 f <|> label`.
val skipManyMinMaxSatisfy2L: int -> int -> (char -> bool) -> (char -> bool) -> string -> Parser<unit,'u>

/// `regex pattern` matches the .NET regular expression given by the string `pattern` on the chars
/// beginning at the current index in the input stream. It returns the string matched by the regular expression.
/// If the regular expression does not match, the parser fails without consuming input.
///
/// The `System.Text.RegularExpressions.Regex` object that is internally used to match the pattern is constructed
/// with the `RegexOptions` `MultiLine` and `ExplicitCapture`. In order to ensure that the regular expression
/// can only match at the beginning of a string, `"\\A"` is automatically prepended to the pattern.
///
/// Newline chars ('\r' and '\n') in the pattern are interpreted literally.
/// For example, an '\n' char in the pattern will only match "\n", not "\r" or "\r\n".
/// However, in the returned string all newlines ("\n", "\r\n" or "\r") are normalized to "\n".
///
/// For large files the regular expression is *not* applied to a string containing *all* the remaining chars
/// in the stream. The number of chars that are guaranteed to be visible to the regular expression is specified
/// during construction of the `CharStream`. If one of the `runParser` function` is used to run the parser,
/// this number is 43690.
val regex: string -> Parser<string,'u>

// ----------------------------------------------
// Parsing strings with the help of other parsers
// ----------------------------------------------

/// `manyChars cp` parses a sequence of *zero* or more chars with the char parser `cp`.
/// It returns the parsed chars as a string.
///
/// `manyChars cp` is an optimized implementation of `many (attempt cp)` that returns
/// the chars as a string instead of a char list.  The equivalence to `many (attempt p)`
///  instead of `many p` implies that `manyChars` never fails.
val manyChars:       Parser<char,'u> -> Parser<string,'u>
/// `manyChars2 cp1 cp` behaves like `manyChars2 cp`, except that it parses the first char with `cp1` instead of `cp`.
val manyChars2:      Parser<char,'u> -> Parser<char,'u> -> Parser<string,'u>
/// `skipManyChars cp` is an optimized implementation of `manyChars cp |>> ignore`.
val skipManyChars:   Parser<'a,'u>   -> Parser<unit,'u>
/// `skipManyChars2 cp1 cp` is an optimized implementation of `manyChars2 cp1 cp |>> ignore`.
val skipManyChars2:  Parser<'a,'u>   -> Parser<'a,'u>   -> Parser<unit,'u>

/// `many1Chars cp` parses a sequence of *one* or more chars with the char parser `cp`.
/// It returns the parsed chars as a string.
///
/// `many1Chars cp` is an optimized implementation of `many1 (attempt cp)` that returns
/// the chars as a string instead of a char list.  The equivalence to `many1 (attempt p)`
/// instead of `many1 p` implies that  `many1Chars` never fails after consuming input.
val many1Chars:      Parser<char,'u> -> Parser<string,'u>
/// `many1Chars2 cp1 cp` behaves like `many1Chars2 cp`, except that it parses the first char with `cp1` instead of `cp`.
val many1Chars2:     Parser<char,'u> -> Parser<char,'u> -> Parser<string,'u>
/// `skipMany1Chars cp` is an optimized implementation of `many1Chars cp |>> ignore`.
val skipMany1Chars:  Parser<'a,'u>   -> Parser<unit,'u>
/// `skipMany1Chars2 cp1 cp` is an optimized implementation of `many1Chars2 cp1 cp |>> ignore`.
val skipMany1Chars2: Parser<'a,'u>   -> Parser<'a,'u>   -> Parser<unit,'u>

/// `manyCharsTill cp endp` parses chars with the char parser `cp` until the parser `endp` succeeds.
/// It stops after `endp` and returns the parsed chars as a string.
///
/// `manyCharsTill cp endp` is an optimized implementation of `manyTill cp endp` that returns the chars as a string instead of a char list.
val manyCharsTill:      Parser<char,'u> -> Parser<'b,'u> -> Parser<string,'u>
/// `manyCharsTillApply cp endp f` parses chars with the char parser `cp` until the parser `endp` succeeds.
/// It stops after `endp` and returns the result of the function application `f str b`,
/// where `str` is the parsed string and `b` is result returned by `endp`.
val manyCharsTillApply: Parser<char,'u> -> Parser<'b,'u> -> (string -> 'b -> 'c) -> Parser<'c,'u>
/// `skipManyCharsTill cp endp` is an optimized implementation of `manyCharsTill cp endp |>> ignore`.
val skipManyCharsTill:  Parser<'a,'u>   -> Parser<'b,'u> -> Parser<unit,'u>

/// `many1CharsTill cp endp` parses one char with the char parser `cp`.
/// Then it parses more chars with `cp` until the parser `endp` succeeds.
/// It stops after `endp` and returns the parsed chars as a string.
///
/// `many1CharsTill cp endp` is an optimized implementation of `pipe2 cp (manyCharsTill cp endp) (fun c1 str -> c1.ToString() + str)`
val many1CharsTill:       Parser<char,'u>                    -> Parser<'b,'u> -> Parser<string,'u>
/// `many1CharsTill2 cp1 cp endp` behaves like `many1CharsTill cp endp`, except that it parses the first char with `cp1` instead of `cp`.
val many1CharsTill2:      Parser<char,'u> -> Parser<char,'u> -> Parser<'b,'u> -> Parser<string,'u>

/// `many1CharsTillApply cp endp` parses one char with the char parser `cp`.
/// Then it parses more chars with `cp` until the parser `endp` succeeds.
/// It stops after `endp` and returns the result of the function application `f str b`,
/// where `str` is the parsed string and `b` is result returned by `endp`.
val many1CharsTillApply:  Parser<char,'u>                    -> Parser<'b,'u> -> (string -> 'b -> 'c) -> Parser<'c,'u>
/// `many1CharsTillApply2 cp1 cp endp` behaves like `many1CharsTillApply cp endp`, except that it parses the first char with `cp1` instead of `cp`.
val many1CharsTillApply2: Parser<char,'u> -> Parser<char,'u> -> Parser<'b,'u> -> (string -> 'b -> 'c) -> Parser<'c,'u>
/// `skipMany1CharsTill cp endp` is an optimized implementation of `many1CharsTill cp endp |>> ignore`.
val skipMany1CharsTill:   Parser<'a,'u>                      -> Parser<'b,'u> -> Parser<unit,'u>
/// `skipMany1CharsTill2 cp1 cp endp` is an optimized implementation of `many1CharsTill2 cp1 cp endp |>> ignore`.
val skipMany1CharsTill2:  Parser<'a,'u>   -> Parser<'a,'u>   -> Parser<'b,'u> -> Parser<unit,'u>

/// `manyStrings sp` parses a sequence of *zero* or more strings with the string parser `sp`.
/// It returns the strings in concatenated form.
/// `manyStrings sp` is an optimized implementation of `manyReduce (+) "" sp`.
val manyStrings:   Parser<string,'u>                      -> Parser<string,'u>
/// `manyStrings2 sp1 sp` behaves like `manyStrings sp`, except that it parses the first string with `sp1` instead of `sp`.
val manyStrings2:  Parser<string,'u> -> Parser<string,'u> -> Parser<string,'u>

/// `many1Strings sp` parses a sequence of *one* or more strings with the string parser `sp`.
/// It returns the strings in concatenated form.
/// Note that `many1Strings sp` does not require the first string to be non-empty.
val many1Strings:  Parser<string,'u>                      -> Parser<string,'u>
/// `many1Strings2 sp1 sp` behaves like `many1Strings sp`, except that it parses the first string with `sp1` instead of `sp`.
val many1Strings2: Parser<string,'u> -> Parser<string,'u> -> Parser<string,'u>

/// `skipped p` applies the parser `p` and returns the chars skipped over by `p` as a string.
/// All newlines ("\r\n", "\r" or "\n") are normalized to "\n".
val skipped: Parser<unit,'u> -> Parser<string,'u>

/// `p |> withSkippedString f` applies the parser `p` and returns the result of `f str x`,
/// where `str` is the string skipped over by `p` and `x` is the result returned by `p`.
val withSkippedString: (string -> 'a -> 'b) -> Parser<'a,'u> -> Parser<'b,'u>


// ---------------
// Parsing numbers
// ---------------

/// Encodes the various options of the `numberLiteral` parser.
[<System.Flags>]
type NumberLiteralOptions =
     | None                             = 0
     | AllowSuffix                      = 0b000000000001
     | AllowMinusSign                   = 0b000000000010
     | AllowPlusSign                    = 0b000000000100
     | AllowFraction                    = 0b000000001000
     | AllowFractionWOIntegerPart       = 0b000000010000
     | AllowExponent                    = 0b000000100000
     | AllowHexadecimal                 = 0b000001000000
     | AllowBinary                      = 0b000010000000
     | AllowOctal                       = 0b000100000000
     | AllowInfinity                    = 0b001000000000
     | AllowNaN                         = 0b010000000000

     | IncludeSuffixCharsInString       = 0b100000000000

     | DefaultInteger                   = 0b000111000110
     | DefaultUnsignedInteger           = 0b000111000000
     | DefaultFloat                     = 0b011001101110

/// The return type of the `numberLiteral` parser. An instance contains the parsed
/// number literal and various bits of information about it.
/// Note that the `String` member contains the string literal without the suffix chars,
/// except if the `NumberLiteralOptions` passed to the `numberLiteral` parser have the
/// `IncludeSuffixCharsInString` flag set.
/// Any parsed suffix chars are always available through the `SuffixChar1` - `4` members.
type NumberLiteral =
    new: string:string * info:NumberLiteralResultFlags
         * suffixChar0: char * suffixChar1: char * suffixChar2: char * suffixChar3: char -> NumberLiteral

    /// The parsed number literal string. Only includes the parsed suffix chars if the
    /// `NumberLiteralOptions` passed to the `numberLiteral` parser have the `IncludeSuffixCharsInString` flag set.
    member String: string
    /// Eencodes various bits of information on the string literal.
    member Info: NumberLiteralResultFlags

    member SuffixLength: int
    /// Returns the first suffix char, or EOS if no suffix char was parsed.
    member SuffixChar1: char
    /// Returns the second suffix char, or EOS if less than two suffix chars were parsed.
    member SuffixChar2: char
    /// Returns the third suffix char, or EOS if less than three suffix chars were parsed
    member SuffixChar3: char
    /// Returns the fourth suffix char, or EOS if less than four suffix chars were parsed
    member SuffixChar4: char

    member HasMinusSign: bool
    member HasPlusSign: bool
    member HasFraction: bool
    member HasExponent: bool
    member IsInteger: bool
    member IsDecimal: bool
    member IsHexadecimal: bool
    member IsBinary: bool
    member IsOctal: bool
    member IsNaN: bool
    member IsInfinity: bool
    
    override Equals: obj -> bool
    override GetHashCode: unit -> int

and /// Encodes various bits of information about a parsed number literal.
    [<System.Flags>]
    NumberLiteralResultFlags =
     | None             = 0
     | SuffixLengthMask = 0b0000000000001111
     | HasMinusSign     = 0b0000000000010000
     | HasPlusSign      = 0b0000000000100000
     | HasIntegerPart   = 0b0000000001000000
     | HasFraction      = 0b0000000010000000
     | HasExponent      = 0b0000000100000000
     | IsDecimal        = 0b0000001000000000
     | IsHexadecimal    = 0b0000010000000000
     | IsBinary         = 0b0000100000000000
     | IsOctal          = 0b0001000000000000
     | BaseMask         = 0b0001111000000000
     | IsInfinity       = 0b0010000000000000
     | IsNaN            = 0b0100000000000000


/// `numberLiteral options label` parses a number literal and returns the result in form
/// of a `NumberLiteral` value. The given `NumberLiteralOptions` argument determines the kind
/// of number literals accepted. The string `label` is used in the `Expected` error message
/// that is generated when the parser fails without consuming input.
///
/// The parser fails without consuming input, if not at least one digit (including the 0 in the
/// format specifiers "0x" etc.) can be parsed. It fails after consuming input, if no decimal
/// digit comes after an exponent marker or no valid digit comes after a format specifier.
val numberLiteral:    NumberLiteralOptions -> string
                   -> Parser<NumberLiteral,'u>

/// `numberLiteralE` is an uncurried version of `numberLiteral` that can be used to
/// implement number parsers without having to construct a `numberLiteral` closure.
val numberLiteralE:    NumberLiteralOptions -> errorInCaseNoLiteralFound: ErrorMessageList
                    -> State<'u> -> Reply<NumberLiteral,'u>

/// Parses a floating-point number in decimal or hexadecimal format.
/// The special values NaN and Inf(inity)? (case insensitive) are also recognized.
///
/// The parser fails
/// without consuming input, if not at least one digit (including the '0' in "0x") can be parsed,
/// after consuming input, if no digit comes after an exponent marker or no hex digit comes after "0x",
/// after consuming input, if the value represented by the input string (after rounding) is greater than `System.Double.MaxValue` or less than `System.Double.MinValue`.
val pfloat: Parser<float,'u>


/// Parses an integer in decimal, hexadecimal ("0x" prefix), octal ("0o") or binary ("0b") format.
/// The parser fails
/// without consuming input, if not at least one digit (including the '0' in the format specifiers "0x" etc.) can be parsed,
/// after consuming input, if no digit comes after an exponent marker or no hex digit comes after a format specifier,
/// after consuming input, if the value represented by the input string is greater than `System.Int64.MaxValue` or less than `System.Int64.MinValue`.
val pint64: Parser<int64,'u>

/// Parses an integer in decimal, hexadecimal ("0x" prefix), octal ("0o") or binary ("0b") format.
/// The parser fails
/// without consuming input, if not at least one digit (including the '0' in the format specifiers "0x" etc.) can be parsed,
/// after consuming input, if no digit comes after an exponent marker or no hex digit comes after a format specifier,
/// after consuming input, if the value represented by the input string is greater than `System.Int32.MaxValue` or less than `System.Int32.MinValue`.
val pint32: Parser<int32,'u>

/// Parses an integer in decimal, hexadecimal ("0x" prefix), octal ("0o") or binary ("0b") format.
/// The parser fails
/// without consuming input, if not at least one digit (including the '0' in the format specifiers "0x" etc.) can be parsed,
/// after consuming input, if no digit comes after an exponent marker or no hex digit comes after a format specifier,
/// after consuming input, if the value represented by the input string is greater than `System.Int16.MaxValue` or less than `System.Int16.MinValue`.
val pint16: Parser<int16,'u>

/// Parses an integer in decimal, hexadecimal ("0x" prefix), octal ("0o") or binary ("0b") format.
/// The parser fails
/// without consuming input, if not at least one digit (including the '0' in the format specifiers "0x" etc.) can be parsed,
/// after consuming input, if no digit comes after an exponent marker or no hex digit comes after a format specifier,
/// after consuming input, if the value represented by the input string is greater than 127 or less than -128.
val pint8: Parser<int8,'u>

/// Parses an unsigned integer in decimal, hexadecimal ("0x" prefix), octal ("0o") or binary ("0b") format.
/// The parser fails
/// without consuming input, if not at least one digit (including the '0' in the format specifiers "0x" etc.) can be parsed,
/// after consuming input, if no digit comes after an exponent marker or no hex digit comes after a format specifier,
/// after consuming input, if the value represented by the input string is greater than `System.UInt64.MaxValue`.
val puint64: Parser<uint64,'u>

/// Parses an unsigned integer in decimal, hexadecimal ("0x" prefix), octal ("0o") or binary ("0b") format.
/// The parser fails
/// without consuming input, if not at least one digit (including the '0' in the format specifiers "0x" etc.) can be parsed,
/// after consuming input, if no digit comes after an exponent marker or no hex digit comes after a format specifier,
/// after consuming input, if the value represented by the input string is greater than `System.UInt32.MaxValue`.
val puint32: Parser<uint32,'u>

/// Parses an unsigned integer in decimal, hexadecimal ("0x" prefix), octal ("0o") or binary ("0b") format.
/// The parser fails
/// without consuming input, if not at least one digit (including the '0' in the format specifiers "0x" etc.) can be parsed,
/// after consuming input, if no digit comes after an exponent marker or no hex digit comes after a format specifier,
/// after consuming input, if the value represented by the input string is greater than `System.UInt16.MaxValue`.
val puint16: Parser<uint16,'u>

/// Parses an unsigned integer in decimal, hexadecimal ("0x" prefix), octal ("0o") or binary ("0b") format.
/// The parser fails
/// without consuming input, if not at least one digit (including the '0' in the format specifiers "0x" etc.) can be parsed,
/// after consuming input, if no digit comes after an exponent marker or no hex digit comes after a format specifier,
/// after consuming input, if the value represented by the input string is greater than 255.
val puint8: Parser<uint8,'u>


// -------------------
// Conditional parsing
// -------------------

/// `followedByChar c` is an optimized implementation of `followedByL (pchar c) ("'" + c.ToString() + "'")`.
val followedByChar:        char   -> Parser<unit,'u>
/// `notFollowedByChar c` is an optimized implementation of `notFollowedByL (pchar c) ("'" + c.ToString() + "'")`.
val notFollowedByChar:     char   -> Parser<unit,'u>
/// `followedByString str` is an optimized implementation of `followedByL (pstring str) ("'" + str + "'")`.
val followedByString:      string -> Parser<unit,'u>
/// `followedByStringCI str` is an optimized implementation of `followedByL (pstringCI str) ("'" + str + "'")`.
val followedByStringCI:    string -> Parser<unit,'u>
/// `notFollowedByString str` is an optimized implementation of `notFollowedByL (pstring str) ("'" + str + "'")`.
val notFollowedByString:   string -> Parser<unit,'u>
/// `notFollowedByStringCI str` is an optimized implementation of `notFollowedByL (pstringCI str) ("'" + str + "'")`.
val notFollowedByStringCI: string -> Parser<unit,'u>

/// `nextCharSatisfies f` succeeds if the predicate function `f` returns `true`
/// when applied to the char *after* the current char, otherwise it fails.
/// If there is no char after the current char, this parser fails (as opposed to `nextCharSatisfiesNot`).
/// This parser never changes the parser state.
/// If this parser fails, it returns no descriptive error message; hence it should only be
/// used together with parsers that take care of a potential error.
val nextCharSatisfies: (char -> bool) -> Parser<unit,'u>

/// `nextCharSatisfiesNot f` succeeds if the predicate function `f` returns `false`
/// when applied to the char *after* the current char, otherwise it fails.
/// If there is no char after the current char, this parser succeeds (as opposed to `nextCharSatisfies`).
/// This parser never changes the parser state.
/// If this parser fails, it returns no descriptive error message; hence it should only be
/// used together with parsers that take care of a potential error.
val nextCharSatisfiesNot: (char -> bool) -> Parser<unit,'u>

// `currentCharSatisfies f` is equivalent to `followedBy (satisfy f)`.

/// `currentCharSatisfies f` succeeds if the predicate function `f` returns `true`
/// when applied to the current char, otherwise it fails.
/// If the stream already has reached the end, this parser fails (as opposed to `currentCharSatisfiesNot`).
/// This parser never changes the parser state.
/// If this parser fails, it returns no descriptive error message; hence it should only be
/// used together with parsers that take care of a potential error.
val currentCharSatisfies: (char -> bool) -> Parser<unit,'u>

/// `currentCharSatisfiesNot f` succeeds if the predicate function `f` returns `false`
/// when applied to the current char, otherwise it fails.
/// If the stream already has reached the end, this parser succeeds (as opposed to `currentCharSatisfies`).
/// This parser never changes the parser state.
/// If this parser fails, it returns no descriptive error message; hence it should only be
/// used together with parsers that take care of a potential error.
val currentCharSatisfiesNot: (char -> bool) -> Parser<unit,'u>

/// `previousCharSatisfies f` succeeds if the predicate function `f` returns `true`
/// when applied to the char *before* the current char, otherwise it fails.
/// If the current char is the first char in the stream, this parser fails (as opposed to `previousCharSatisfiesNot`).
/// This parser never changes the parser state.
/// If this parser fails, it returns no descriptive error message; hence it should only be
/// used together with parsers that take care of a potential error.
val previousCharSatisfies: (char -> bool) -> Parser<unit,'u>

/// `previousCharSatisfiesNot f` succeeds if the predicate function `f` returns `false`
/// when applied to the char *before* the current char, otherwise it fails.
/// If the current char is the first char in the stream, this parser succeeds (as opposed to `previousCharSatisfies`).
/// This parser never changes the parser state.
/// If the current char is the first char in the stream, the parser succeeds.
/// If this parser fails, it returns no descriptive error message; hence it should only be
/// used together with parsers that take care of a potential error.
val previousCharSatisfiesNot: (char -> bool) -> Parser<unit,'u>


// ================
// Helper functions
// ================

/// `EOS` is equal to `CharStream.Iterator.EndOfStreamChar`.
[<Literal>]
val EOS: char = CharStream.Iterator.EndOfStreamChar

/// `foldCase str` returns a case-folded version of `str`
/// with all chars mappend using the (non-Turkic) Unicode 1-to-1 case folding mappings
/// for chars below 0x10000. If the argument is `null`, `null` is returned.
val foldCase: string -> string

/// `normalizeNewlines str` returns a version of `str`
/// with all occurances of "\r\n" and "\r" replaced by "\n".
/// If the argument is `null`, `null` is returned.
val normalizeNewlines: string -> string

/// Returns a hexadecimal string representation of the `float` argument.
val floatToHexString: float -> string

/// Returns the `float` value represented by the given string in hexadecimal format.
/// Raises a `System.FormatException` in case the string representation is invalid.
/// Raises a `System.OverflowException` if the (absolute) value is too large to be represented by a `float`.
val floatOfHexString: string -> float

/// Returns a hexadecimal string representation of the `float32` argument.
val float32ToHexString: float32 -> string

/// Returns the `float32` value represented by the given string in hexadecimal format.
/// Raises a `System.FormatException` in case the string representation is invalid.
/// Raises a `System.OverflowException` if the (absolute) value is too large to be represented by a `float32`.
val float32OfHexString: string -> float32
