// Copyright (c) Stephan Tolksdorf 2007-2011
// License: Simplified BSD License. See accompanying documentation.

[<AutoOpen>]
module FParsec.Error

type Expected = ErrorMessage.Expected
type ExpectedString = ErrorMessage.ExpectedString
type ExpectedStringCI = ErrorMessage.ExpectedCaseInsensitiveString
type Unexpected = ErrorMessage.Unexpected
type UnexpectedString = ErrorMessage.UnexpectedString
type UnexpectedStringCI = ErrorMessage.UnexpectedCaseInsensitiveString
type Message = ErrorMessage.Message
type NestedError = ErrorMessage.NestedError
type CompoundError = ErrorMessage.CompoundError
type OtherErrorMessage = ErrorMessage.Other

val (|Expected|_|): ErrorMessage -> string option
val (|ExpectedString|_|): ErrorMessage -> string option
val (|ExpectedStringCI|_|): ErrorMessage -> string option
val (|Unexpected|_|): ErrorMessage -> string option
val (|UnexpectedString|_|): ErrorMessage -> string option
val (|UnexpectedStringCI|_|): ErrorMessage -> string option
val (|Message|_|): ErrorMessage -> string option
val (|NestedError|_|): ErrorMessage -> (Position * obj * ErrorMessageList) option
val (|CompoundError|_|): ErrorMessage -> (string * Position * obj * ErrorMessageList) option
val (|OtherErrorMessage|_|): ErrorMessage -> obj option

[<Literal>]
val NoErrorMessages: ErrorMessageList = null;;
val (|ErrorMessageList|NoErrorMessages|): ErrorMessageList -> Choice<ErrorMessage*ErrorMessageList,unit>

val inline isSingleErrorMessageOfType: ErrorMessageType -> ErrorMessageList -> bool

/// `expectedError label` creates an `ErrorMessageList` with a single `Expected label` message.
val expected:         string -> ErrorMessageList
/// `expectedStringError str` creates an `ErrorMessageList` with a single `ExpectedString str` message.
val expectedString:   string -> ErrorMessageList
/// `expectedStringCIError str` creates an `ErrorMessageList` with a single `ExpectedStringCI str` message.
val expectedStringCI: string -> ErrorMessageList

/// `unexpectedError label` creates an `ErrorMessageList` with a single `Unexpected label` message.
val unexpected:         string -> ErrorMessageList
/// `unexpectedStringError str` creates an `ErrorMessageList` with a single `UnexpectedString str` message.
val unexpectedString:   string -> ErrorMessageList
/// `unexpectedStringCIError str` creates an `ErrorMessageList` with a single `UnexpectedStringCI str` message.
val unexpectedStringCI: string -> ErrorMessageList

/// `messageError msg` creates an `ErrorMessageList` with a single `Message msg` message.
val messageError:  string -> ErrorMessageList

/// `otherError o` creates an `ErrorMessageList` with a single `OtherError o` message.
val otherError:  obj -> ErrorMessageList

/// `backtrackError stream msgs` creates an `ErrorMessageList` with a single `BacktrackPoint stream.Position msgs` message,
/// except if `msgs` is already an `ErrorMessageList` with a single `BacktrackPoint(_, _)` message,
/// in which case `msgs` is returned instead.
val nestedError:            CharStream<'u> -> ErrorMessageList -> ErrorMessageList

/// `compoundError label state msgs` creates an `ErrorMessageList` with a single `CompoundError label stream.Position msgs` message,
/// except if `msgs` is an `ErrorMessageList` with a single `BacktrackPoint(pos2, msgs2)` message,
/// in which case an `ErrorMessageList` with a single `CompoundError label pos2 msgs2` message is returned instead.
val compoundError:   string -> CharStream<'u> -> ErrorMessageList -> ErrorMessageList

/// `mergeErrors error1 error2` is equivalent to `ErrorMessageList.Merge(error1, error2)`.
val
#if NOINLINE
#else
    inline
#endif
           mergeErrors: ErrorMessageList -> ErrorMessageList -> ErrorMessageList

/// Represents a simple container type that brings together the position, user state and error messages of a parser error.
[<Sealed>]
type ParserError =
    new: Position * userState:obj * ErrorMessageList -> ParserError

    member Position: Position
    member UserState: obj
    member Messages: ErrorMessageList

    /// Returns a string representation of the `ParserError`.
    override ToString: unit -> string

    /// Returns a string representation of the `ParserError`.
    ///
    /// The given `CharStream` must contain the content of the original `CharStream`
    /// for which this `ParserError` was generated (at the original indices).
    ///
    /// For each error location the printed position information is augmented
    /// with the line of text surrounding the error position, together with a '^'-marker
    /// pointing to the exact location of the error in the input stream.
    member ToString: streamWhereErrorOccurred: CharStream<'u> -> string

    /// Writes a string representation of the `ParserError` to the given `TextWriter` value.
    ///
    /// The given `CharStream` must contain the content of the original `CharStream`
    /// for which this `ParserError` was generated (at the original indices).
    ///
    /// For each error location the printed position information is augmented
    /// with the line of text surrounding the error position, together with a '^'-marker
    /// pointing to the exact location of the error in the input stream.
    member WriteTo:   textWriter: System.IO.TextWriter
                    * streamWhereErrorOccurred: CharStream<'u>
                    * ?tabSize: int
                    * ?columnWidth: int 
                    * ?initialIndention: string * ?indentionIncrement: string
                    -> unit

    /// Writes a string representation of the `ParserError` to the given `TextWriter` value.
    ///
    /// For each error position `getStreamByName` is called with the `StreamName` of the `Position`.
    /// The returned `CharStream` must be `null` or contain the content of the `CharStream` for which
    /// the error was generated (at the original indices).
    ///
    /// If `getStreamByName` returns a non-null `CharStream`, the printed error position information is
    /// augmented with the line of text surrounding the error position, together with a '^'-marker
    /// pointing to the exact location of the error in the input stream.
    member WriteTo:   textWriter: System.IO.TextWriter
                    * getStream: (Position -> CharStream<'u>)
                    * ?tabSize: int
                    * ?columnWidth: int 
                    * ?initialIndention: string * ?indentionIncrement: string                    
                    -> unit

    /// Writes a string representation of the `ParserError` to the given `TextWriter` value.
    ///
    /// The format of the position information can be customized by specifying the `positionPrinter`
    /// argument. The given function is expected to print a representation of the passed `Position` value
    /// to the passed `TextWriter` value. If possible, it should indent text lines with the passed string
    /// and take into account the maximum column count (including indention) passed as the last argument.
    member WriteTo:   textWriter: System.IO.TextWriter
                    * ?positionPrinter: (System.IO.TextWriter -> Position -> string -> int -> unit)
                    * ?columnWidth: int 
                    * ?initialIndention: string * ?indentionIncrement: string
                    -> unit

    override Equals: obj -> bool
    override GetHashCode: unit -> int


val inline internal raiseInfiniteLoopException: string -> CharStream -> 'a