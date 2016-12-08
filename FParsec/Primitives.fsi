// Copyright (c) Stephan Tolksdorf 2007-2011
// License: Simplified BSD License. See accompanying documentation.

[<AutoOpen>]
module FParsec.Primitives

open FParsec
open FParsec.Error

/// The parser succeeded.
[<Literal>] val Ok:         ReplyStatus = ReplyStatus.Ok;;

/// The parser failed.
[<Literal>] val Error:      ReplyStatus = ReplyStatus.Error;;

/// The parser failed and no error recovery (except after backtracking) should be tried.
[<Literal>] val FatalError: ReplyStatus = ReplyStatus.FatalError;;

/// The type of the parser functions supported by FParsec combinators.
type Parser<'Result, 'UserState> = CharStream<'UserState> -> Reply<'Result>

// =================================
// Parser primitives and combinators
// =================================

// Two basic primitives that are only seldomly directly used in user code:

/// The parser `preturn x` always succeeds with the result `x` (without changing the parser state).
/// `preturn x` is defined as `fun stream -> Reply(x)`.
val preturn: 'a -> Parser<'a,'u>

/// The parser `pzero` always fails with an empty error message list, i.e. an unspecified error.
/// `pzero x` is defined as `fun stream -> Reply(Error, NoErrorMessages)`.
val pzero: Parser<'a,'u>

// ---------------------------
// Chaining and piping parsers
// ---------------------------

/// The parser `p >>= f` first applies the parser `p` to the input, then applies the function `f`
/// to the result returned by `p` and finally applies the parser returned by `f` to the input.
val (>>=): Parser<'a,'u> -> ('a -> Parser<'b,'u>) -> Parser<'b,'u>

/// The parser `p >>% x` applies the parser `p` and returns the result `x`.
val (>>%): Parser<'a,'u> -> 'b -> Parser<'b,'u>

/// The parser `p1 >>. p2` applies the parsers `p1` and `p2` in sequence and returns the result of `p2`.
val (>>.): Parser<'a,'u> -> Parser<'b,'u> -> Parser<'b,'u>

/// The parser `p1 .>> p2` applies the parsers `p1` and `p2` in sequence and returns the result of `p1`.
val (.>>): Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a,'u>

/// The parser `p1 .>>. p2` applies the parsers `p1` and `p2` in sequence and returns the results in a tuple.
val (.>>.): Parser<'a,'u> -> Parser<'b,'u> -> Parser<('a * 'b),'u>

/// The parser `between popen pclose p` applies the parsers `pOpen`, `p` and `pEnd` in sequence.
/// It returns the result of `p`.
val between: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> Parser<'c,'u>

/// The parser `p |>> f` applies the parser `p` and
/// returns the result `f x`,  where `x` is the result returned by `p`.
val (|>>): Parser<'a,'u> -> ('a -> 'b) -> Parser<'b,'u>

/// The parser `pipe2 p1 p2 f` applies the parsers `p1` and `p2` in sequence.
/// It returns the result `f a b`, where `a` and `b` are the results returned by `p1` and `p2`.
val pipe2: Parser<'a,'u> -> Parser<'b,'u> -> ('a -> 'b -> 'c) -> Parser<'c,'u>

/// The parser `pipe3 p1 p2 p3 f` applies the parsers `p1`, `p2` and `p3` in sequence.
/// It returns the result `f a b c`, where `a`, `b` and `c` are the results returned by `p1`, `p2` and `p3`.
val pipe3: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> ('a -> 'b -> 'c -> 'd) -> Parser<'d,'u>

/// The parser `pipe4 p1 p2 p3 p4 f` applies the parsers `p1`, `p2`, `p3` and `p4` in sequence.
/// It returns the result `f a b c d`, where `a`, `b`, `c` and `d` are the results returned by `p1`, `p2`, `p3` and `p4`.
val pipe4: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> Parser<'d,'u> -> ('a -> 'b -> 'c -> 'd -> 'e) -> Parser<'e,'u>

/// The parser `pipe5 p1 p2 p3 p4 p5 f` applies the parsers `p1`, `p2`, `p3`, `p4` and `p5` in sequence.
/// It returns the result of the function application `f a b c d e`, where `a`, `b`, `c`, `d` and `e` are the results returned by `p1`, `p2`, `p3`, `p4` and `p5`.
val pipe5: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> Parser<'d,'u> -> Parser<'e,'u> -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> Parser<'f, 'u>


// -----------------------------------------------
// Parsing alternatives and recovering from errors
// -----------------------------------------------

/// The parser `p1 <|> p2` first applies the parser `p1`.
/// If `p1` succeeds, the result of `p1` is returned.
/// If `p1` fails with a non-fatal error and *without changing the parser state*,
/// the parser `p2` is applied.
/// Note: The stream position is part of the parser state, so if `p1` fails after consuming input,
/// `p2` will not be applied.
val (<|>): Parser<'a,'u> -> Parser<'a,'u> -> Parser<'a,'u>

/// The parser `choice ps` is an optimized implementation of `p1 <|> p2 <|> ... <|> pn`,
/// where `p1` ... `pn` are the parsers in the sequence `ps`.
val choice: seq<Parser<'a,'u>> -> Parser<'a,'u>

/// The parser `choiceL ps label` is an optimized implementation of `choice ps <?> label`.
val choiceL: seq<Parser<'a,'u>> -> string -> Parser<'a,'u>

/// The parser `p <|>% x` is an optimized implementation of `p <|> preturn x`.
val (<|>%): Parser<'a,'u> -> 'a -> Parser<'a,'u>

/// The parser `opt p` parses an optional occurrence of `p` as an option value.
/// `opt p` is an optimized implementation of `(p |>> Some) <|>% None`.
val opt: Parser<'a,'u> -> Parser<'a option,'u>

/// The parser `optional p` skips over an optional occurrence of `p`.
/// `optional p` is an optimized implementation of `(p >>% ()) <|>% ()`.
val optional: Parser<'a,'u> -> Parser<unit,'u>


/// The parser `attempt p` applies the parser `p`.
/// If `p` fails after changing the parser state or with a fatal error,
/// `attempt p` will backtrack to the original parser state and report a non-fatal error.
val attempt: Parser<'a,'u> -> Parser<'a,'u>

/// The parser `p >>=? f` behaves like `p >>= f`, except that it will backtrack to the beginning
/// if the parser returned by `f` fails with a non-fatal error and without changing the parser state,
/// even if `p` has changed the parser state.
val (>>=?): Parser<'a,'u> -> ('a -> Parser<'b,'u>) -> Parser<'b,'u>

/// The parser `p1 >>? p2` behaves like `p1 >>. p2`, except that it will backtrack
/// to the beginning if `p2` fails with a non-fatal error and without changing the parser state,
/// even if `p1` has changed the parser state.
val (>>?): Parser<'a,'u> -> Parser<'b,'u> -> Parser<'b,'u>

/// The parser `p1 .>>? p2` behaves like `p1 .>> p2`, except that it will backtrack
/// to the beginning if `p2` fails with a non-fatal error and without changing the parser state,
/// even if `p1` has changed the parser state.
val (.>>?): Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a,'u>

/// The parser `p1 .>>.? p2` behaves like `p1 .>>. p2`, except that it will backtrack
/// to the beginning if `p2` fails with a non-fatal error and without changing the parser state,
/// even if `p1` has changed the parser state.
val (.>>.?): Parser<'a,'u> -> Parser<'b,'u> -> Parser<('a * 'b),'u>

// -------------------------------------
// Conditional parsing and looking ahead
// -------------------------------------

/// The parser `notEmpty p` behaves like `p`,
/// except that it fails when `p` succeeds without consuming input
/// or changing the parser state in any other way.
val notEmpty: Parser<'a,'u> -> Parser<'a,'u>

/// The parser `followedBy p` succeeds if the parser `p` succeeds at the current position.
/// Otherwise it fails with a non-fatal error. This parser never changes the parser state.
/// If the parser `followedBy p` fails, it returns no descriptive error message.
/// Hence it should only be used together with other parsers that take care of a potential error.
/// Alternatively, `followedByL p label` can be used to ensure a more descriptive error message.
val followedBy: Parser<'a,'u> -> Parser<unit,'u>

/// The parser `followedByL p` behaves like `followedBy p`,
/// except that it returns an `Expected label` error message when the parser `p` fails.
val followedByL: Parser<'a,'u> -> string -> Parser<unit,'u>

/// The parser `notFollowedBy p` succeeds if the parser `p` fails to parse at the current position.
/// Otherwise it fails with a non-fatal error. This parser never changes the parser state.
/// If the parser `notFollowedBy p` fails, it returns no descriptive error message.
/// Hence it should only be used together with other parsers that take care of a potential error.
/// Alternatively, `notFollowedByL p label` can be used to ensure a more descriptive error message.
val notFollowedBy: Parser<'a,'u> -> Parser<unit,'u>

/// The parser `notFollowedByL p` behaves like `notFollowedBy p`,
/// except that it returns an `Unexpected label` error message when the parser `p` fails.
val notFollowedByL: Parser<'a,'u> -> string -> Parser<unit,'u>

/// The parser `lookAhead p` parses `p` and restores the original parse state afterwards.
/// In case `p` fails after changing the parser state, the error messages are wrapped in a `NestedError`.
/// If it succeeds, any error messages are discarded. Fatal errors are turned into normal errors.
val lookAhead: Parser<'a,'u> -> Parser<'a,'u>


// --------------------------
// Customizing error messages
// --------------------------

/// The parser `p <?> label` applies the parser `p`. If `p` does not change the parser state
/// (usually because `p` failed), the error messages are replaced with `expected label`.
val (<?>): Parser<'a,'u> -> string -> Parser<'a,'u>

/// The parser `p <??> label` behaves like `p <?> label`, except that when `p` fails
/// after changing the parser state (for example, because `p` consumes input before it fails),
/// a `CompoundError` message is generated with both the given string `label` and the
/// error messages generated by `p`.
val (<??>): Parser<'a,'u> -> string -> Parser<'a,'u>

/// The parser `fail msg` always fails with a `messageError msg`.
/// The error message will be displayed together with other error messages generated for
/// the same input position.
val fail: string -> Parser<'a,'u>

/// The parser `failFatally msg` always fails with a `messageError msg`. It signals a
/// FatalError, so that no error recovery is attempted (except via backtracking constructs).
val failFatally: string -> Parser<'a,'u>

// -----------------
// Parsing sequences
// -----------------

/// The parser `tuple2 p1 p2` applies the parsers `p1` and `p2` in sequence and
/// returns the results in a tuple.
/// `tuple2 p1 p2` is defined as `p1 .>>. p2`.
val tuple2: Parser<'a,'u> -> Parser<'b,'u> -> Parser<('a * 'b),'u>

/// The parser `tuple3 p1 p2 p3` applies the parsers `p1`, `p2` and `p3` in sequence and
/// returns the results in a tuple.
val tuple3: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> Parser<('a * 'b * 'c),'u>

/// The parser `tuple4 p1 p2 p3 p4` applies the parsers `p1`, `p2`, `p3` and `p4` in sequence and
/// returns the results in a tuple.
val tuple4: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> Parser<'d,'u>  -> Parser<('a * 'b * 'c * 'd),'u>

/// The parser `tuple5 p1 p2 p3 p4 p5` applies the parsers `p1`, `p2`, `p3`, `p4` and `p5` in sequence and
/// returns the results in a tuple.
val tuple5: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> Parser<'d,'u> -> Parser<'e,'u>  -> Parser<('a * 'b * 'c * 'd * 'e),'u>


// p{n}

/// The parser `parray n p` parses `n` occurences of `p` and
/// returns the returns the results in an array.
/// For example, `parray 3 p` is equivalent to `pipe3 p p p (fun a b c -> [|a;b;c|])`.
val parray: int -> Parser<'a,'u> -> Parser<'a[],'u>

/// The parser `skipArray n p` is an optimized implementation of `parray n p |>> ignore`.
val skipArray: int -> Parser<'a,'u> -> Parser<unit,'u>


// p*

/// The parser `many p` repeatedly applies the parser `p` until `p` fails.
/// It returns a list of the results returned by `p`.
/// At the end of the sequence `p` must fail without changing the parser state and without
/// signalling a `FatalError`, otherwise `many p` will fail with the error reported by `p`.
/// `many p` tries to guard against an infinite loop by throwing an exception
/// if `p` succeeds without changing the parser state.
val many: Parser<'a,'u> -> Parser<'a list,'u>

/// The parser `skipMany p` is an optimized implementation of `many p |>> ignore`.
val skipMany: Parser<'a,'u> -> Parser<unit,'u>


// p+

/// The parser `many1 p` behaves like `many p`, except that it requires `p` to succeed at least one time.
/// `many1 p` is an optimized implementation of `pipe2 p (many p) (fun hd tl -> hd::tl)`.
val many1: Parser<'a,'u> -> Parser<'a list,'u>

/// The parser `skipMany1 p` is an optimized implementation of `many1 p |>> ignore`.
val skipMany1: Parser<'a,'u> -> Parser<unit,'u>


// (p (sep p)*)?

/// The parser `sepBy p sep` parses *zero* or more occurrences of `p` separated by `sep`
/// (in EBNF notation: `(p (sep p)*)?`).
val sepBy: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `skipSepBy p sep` is an optimized implementation of `sepBy p sep |>> ignore`.
val skipSepBy: Parser<'a,'u> -> Parser<'b,'u> -> Parser<unit,'u>


// p (sep p)*

/// The parser `sepBy1 p sep` parses *one* or more occurrences of `p` separated by `sep`
/// (in EBNF notation: `p (sep p)*`).
val sepBy1: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `skipSepBy1 p sep` is an optimized implementation of `sepBy1 p sep |>> ignore`.
val skipSepBy1: Parser<'a,'u> -> Parser<'b,'u> -> Parser<unit,'u>


// (p (sep p)* sep?)?

/// The parser `sepEndBy p sep` parses *zero* or more occurrences of `p` separated and
/// optionally ended by `sep` (in EBNF notation: `(p (sep p)* sep?)?`).
/// It returns a list of the results returned by `p`.
val sepEndBy: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `skipSepEndBy p sep` is an optimized implementation of `sepEndBy p sep |>> ignore`.
val skipSepEndBy: Parser<'a,'u> -> Parser<'b,'u> -> Parser<unit,'u>


// p (sep p)* sep?

/// The parser `sepEndBy1 p sep` parses *one* or more occurrences of `p` separated and
/// optionally ended by `sep` (in EBNF notation: `p (sep p)* sep?`).
/// It returns a list of the results returned by `p`.
val sepEndBy1: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `skipSepEndBy1 p sep` is an optimized implementation of `sepEndBy1 p sep |>> ignore`.
val skipSepEndBy1: Parser<'a,'u> -> Parser<'b,'u> -> Parser<unit,'u>


/// The `parser manyTill p endp` repeatedly applies the parser `p` 
/// for as long as `endp` fails (without changing the parser state).
/// It returns a list of the results returned by `p`.
val manyTill: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `skipManyTill p endp` is an optimized implementation of `manyTill p endp |>> ignore`.
val skipManyTill: Parser<'a,'u> -> Parser<'b,'u> -> Parser<unit,'u>

/// The parser `many1Till p endp` behaves like `manyTill p endp`, except that it requires `p` to succeed at least one time.
/// `many1Till p endp` is an optimized implementation of `pipe2 p (manyTill p endp) (fun hd tl -> hd::tl)`.
val many1Till: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

val skipMany1Till: Parser<'a,'u> -> Parser<'b,'u> -> Parser<unit,'u>


[<Sealed>]
type Inline =

#if NOINLINE
  static member
#else
  [<NoDynamicInvocation>]
  static member inline
#endif
                       Many: stateFromFirstElement: ('T -> 'State)
                           * foldState: ('State -> 'T -> 'State)
                           * resultFromState: ('State -> 'Result)
                           * elementParser: Parser<'T,'U>
                           * ?firstElementParser: Parser<'T,'U>
                           * ?resultForEmptySequence: (unit -> 'Result)
                          -> Parser<'Result,'U>

#if NOINLINE
  static member
#else
  [<NoDynamicInvocation>]
  static member inline
#endif
                       SepBy: stateFromFirstElement: ('T -> 'State)
                            * foldState: ('State -> 'Separator -> 'T -> 'State)
                            * resultFromState: ('State -> 'Result)
                            * elementParser: Parser<'T,'U>
                            * separatorParser: Parser<'Separator,'U>
                            * ?firstElementParser: Parser<'T,'U>
                            * ?resultForEmptySequence: (unit -> 'Result)
                            * ?separatorMayEndSequence: bool
                           -> Parser<'Result,'U>

#if NOINLINE
  static member
#else
  [<NoDynamicInvocation>]
  static member inline
#endif
                       ManyTill: stateFromFirstElement: ('T -> 'State)
                               * foldState: ('State -> 'T -> 'State)
                               * resultFromStateAndEnd: ('State -> 'E -> 'Result)
                               * elementParser: Parser<'T,'U>
                               * endParser: Parser<'E,'U>
                               * ?firstElementParser: Parser<'T,'U>
                               * ?resultForEmptySequence: ('E -> 'Result)
                              -> Parser<'Result,'U>

// (((p op p) op p) ... op p)

/// The parser `chainl1 p op` parses one or more occurrences of `p` separated by `op`
/// (in EBNF notation: `p (op p)*`).
/// It returns the value obtained by *left* associative application of all functions
/// returned by `op` to the results returned by `p`,
/// i.e. `f_n (... (f_2 (f_1 x_1 x_2) x_3) ...) x_n+1`,
/// where `f_1` to `f_n` are the functions returned by the parser `op` and
/// `x_1` to `x_n+1` are the values returned by `p`. If only a single occurance
/// of `p` and no occurance of `op` is parsed, the result of `p` is returned directly.
val chainl1: Parser<'a,'u> -> Parser<('a -> 'a -> 'a),'u>       -> Parser<'a,'u>

/// The parser `chainl p op defVal` is equivalent to `chainl1 p op <|>% defVal`.
val chainl:  Parser<'a,'u> -> Parser<('a -> 'a -> 'a),'u> -> 'a -> Parser<'a,'u>


// (p op ... (p op (p op p)))

/// The parser `chainr1 p op` parses one or more occurrences of `p` separated by `op`
/// (in EBNF notation: `p (op p)*`).
/// It returns the value obtained by *right* associative application of all functions
/// returned by `op` to the results returned by `p`,
/// i.e. `f1 x_1 (f_2 x_2 (... (f_n x_n x_n+1) ...))`,
/// where `f_1` to `f_n` are the functions returned by the parser `op` and
/// `x_1` to `x_n+1` are the values returned by `p`. If only a single occurance
/// of `p` and no occurance of `op` is parsed, the result of `p` is returned directly.
val chainr1: Parser<'a,'u> -> Parser<('a -> 'a -> 'a),'u>       -> Parser<'a,'u>

/// The parser `chainr p op defVal` is equivalent to `chainr1 p op <|>% defVal`.
val chainr:  Parser<'a,'u> -> Parser<('a -> 'a -> 'a),'u> -> 'a -> Parser<'a,'u>


// ------------------------------
// Computation expression syntax
// ------------------------------

/// The type of the "builder object" that can be used to build parsers with
/// F#'s "computation expression" syntax a.k.a. "workflow" syntax.
[<Sealed>]
type ParserCombinator =
    new : unit -> ParserCombinator
    member Delay: f:(unit -> Parser<'a,'u>) -> Parser<'a,'u>
    member Return: 'a -> Parser<'a,'u>
    member Bind: Parser<'a,'u>*('a -> Parser<'b,'u>) -> Parser<'b,'u>
    member Zero: unit -> Parser<'a,'u>
    member ReturnFrom: Parser<'a,'u> -> Parser<'a,'u>
    // no Combine member by purpose
    member TryWith: p:Parser<'a,'u> * cf:(exn -> Parser<'a,'u>) -> Parser<'a,'u>
    member TryFinally: p:Parser<'a,'u>* ff:(unit -> unit) -> Parser<'a,'u>

/// The builder object for building parsers using F#'s computation expression syntax.
val parse : ParserCombinator


// ----------------------
// Other helper functions
// ----------------------

// a helper function for defining mutually recursive parser values

/// `let p, pRef = createParserForwardedToRef()` creates a parser `p` that forwards all
/// calls to the parser in the reference cell `pRef`. Initially, `pRef` holds a reference
/// to a dummy parser that raises an exception on any invocation.
val createParserForwardedToRef: unit -> Parser<'a,'u> * Parser<'a,'u> ref
