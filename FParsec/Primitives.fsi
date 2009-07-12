// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

// The basic idea behind combinator parsing is to compose parsers for higher-level
// grammar expressions from simple atomic parsers. A parser combinator library provides
// those predefined atomic parsers and the means to combine them into larger units.
// In the case of FParsec all parsers are F# functions, functions that can be combined
// with the help of higher-level functions, so-called "combinators".
//
// In FParsec a parser is a function mapping a `State<'UserState>` value to a
// `Reply<'Result,'UserState>` value:
//
//     type Parser<'Result,'UserState> = State<'UserState> -> Reply<'Result,'UserState>
//
// Each reply value contains a result value, an output state, error messages
// associated with the output state and a status indicating whether the parser succeeded.
// (If the parser failed, the result value is undefined.)
//
// The input and output state values are instances of the immutable `State<'UserState>`
// type defined in FParsecCS.dll. They contain two pieces of information: the current
// position in the input stream (CharStream.Iterator + column & line count +
// stream "name") and the current `UserState` value. The user-definable `UserState` type
// should normally capture all the relevant mutable parser state besides the input
// position, so that FParsec primitives such as `attempt` or `>>?` can correctly roll
// back the parser state when they need to backtrack after an error occured.
//
// FParsec is currently organized into three modules:
// - The `Error` module contains the definition of the `ErrorMessage`,
//   `ErrorMessageList` and `ParserError` types and various associated helper functions.
// - The `Primitives` module (this module) contains the definitions of the `Parser<_,_>`
//   type abbreviation, the `Reply<_,_>` type and various primitive parsers and parser
//   combinators.
//   (All parsers in this module could in principle also be implemented for an abstract
//   state type.)
// - The `CharParsers` module contains parsers, parser combinators and helper functions
//   for parsing character-based text input.
//   (Parsers in this module depend on the specific interface of the State<_>
//    type for accessing the contents of an underlying `CharStream`.

module FParsec.Primitives

open FParsec.Error

type ReplyStatus = Ok         =  1
                 | Error      =  0
                 | FatalError = -1

/// The parser succeeded.
[<Literal>] val Ok:         ReplyStatus = ReplyStatus.Ok

/// The parser failed.
[<Literal>] val Error:      ReplyStatus = ReplyStatus.Error

/// The parser failed and no error recovery (except after backtracking) should be tried.
[<Literal>] val FatalError: ReplyStatus = ReplyStatus.FatalError


[<StructuralEquality(false); StructuralComparison(false)>]
type Reply<'Result,'UserState> = struct
    new: 'Result * State<'UserState> -> Reply<'Result,'UserState>
    new: ReplyStatus * ErrorMessageList * State<'UserState> -> Reply<'Result,'UserState>
    new: ReplyStatus * 'Result * ErrorMessageList * State<'UserState> -> Reply<'Result,'UserState>

    // Note: the actual order of the fields in this struct is defined in Primitives.fs

    val mutable Status: ReplyStatus
    /// If Status <> Ok then the value of the Result field is undefined and may be equal to Unchecked.defaultof<'Result>.
    val mutable Result: 'Result
    val mutable State:  State<'UserState>
    val mutable Error:  ErrorMessageList

    override Equals: obj -> bool
    override GetHashCode: unit -> int
end

/// The type of the parser functions supported by FParsec combinators.
type Parser<'Result, 'UserState> = State<'UserState> -> Reply<'Result,'UserState>

// =================================
// Parser primitives and combinators
// =================================

// Exactly describing the behaviour of parser combinators in words would often result in
// documentation that is several times larger than the actual implementation. In order to
// keep the following doc strings succinct, we describe the combinators in somewhat loose
// (but hopefully consistent) terms and, where possible, give equivalent definitions that
// only use a small set of basic primitives. The `PrimitiveTests.Reference` module
// contains simple (but sometimes naive) reference implementations of most of the
// functions below.
//
// Some common terminology we use in the descriptions:
//
//   "the parser returns `x`"
//        means "if the parser is successful the `Result` field of the
//        returned `Reply<_,_>` contains the value `x`"
//
//   "the parser fails/succeeds without changing the parser state"
//        means "the parser fails/succeeds with an output state (the `State` field in
//        the `Reply<_,_>` field) equal to the input state (the state value passed to the parser)


// Two basic primitives that are only seldomly directly used in user code:

/// The parser `preturn x` always succeeds with the result `x` (without changing the parser state).
/// `preturn x` is defined as `fun state -> Reply<_,_>(x, state)`.
val preturn: 'a -> Parser<'a,'u>

/// The parser `pzero` always fails with an empty error message list, i.e. an unspecified error.
/// `pzero x` is defined as `fun state -> Reply<_,_>(Error, NoErrorMessages, state)`.
val pzero: Parser<'a,'u>

// ---------------------------
// Chaining and piping parsers
// ---------------------------

/// The parser `p >>= f` first applies the parser `p`, then applies the function `f`
/// to the result returned by `p` and finally applies the parser returned by `f`.
val (>>=): Parser<'a,'u> -> ('a -> Parser<'b,'u>) -> Parser<'b,'u>

// The `>>=` combinator is the conceptual basis for all combinators that
// consecutively apply multiple parsers to the input.
// In order to precisely define its behaviour we give an equivalent definition:
//
// let (>>=) (p: Parser<'a,'u>) (f: 'a -> Parser<'b,'u>) =
//     fun state ->
//         let reply1 = p state
//         if reply1.Status = Ok then
//             let p2 = f reply1.Result
//             let mutable reply2 = p2 reply1.State
//             if reply2.State = reply1.State then
//                 reply2.Error <- mergeErrors reply1.Error reply2.Error
//             reply2
//         else
//             Reply<_,_>(reply1.Status, reply1.Error, reply1.State)
//
// (`mergeErrors` is a helper function from the `Error` module that merges two
//  `ErrorMessageList`s.)
//
// `>>=` establishes the *standard error handling* protocol for all combinators that
// chain multiple parsers: Error messages from different parsers are merged when they
// have the same state. When an intermediate parser run fails, the error is
// immediately propagated.

// In most situations one of the following specialized operators will be more
// convenient and faster than the `>>=` combinator.

/// The parser `p >>$ x` applies the parser `p` and returns the result `x`.
/// `p >>$ x` is an optimized implementation of `p >>= fun _ -> preturn x`.
val (>>$): Parser<'a,'u> -> 'b -> Parser<'b,'u>

/// The parser `p1 >>. p2` applies the parsers `p1` and `p2` in sequence. It returns the result of `p2`.
/// `p1 >>. p2` is an optimized implementation of `p1 >>= fun _ -> p2`.
val (>>.): Parser<'a,'u> -> Parser<'b,'u> -> Parser<'b,'u>

/// The parser `p1 .>> p2` applies the parsers `p1` and `p2` in sequence. It returns the result of `p1`.
/// `p1 .>> p2` is an optimized implementation of `p1 >>= fun x -> p2 >>$ x`.
val (.>>): Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a,'u>

/// The parser `between popen pclose p` applies the parsers `pOpen`, `p` and `pEnd` in sequence.
/// It returns the result of `p`.
/// `between popen pclose p` is an optimized implementation of `popen >>. (p .>> pclose)`.
val between: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> Parser<'c,'u>

/// The parser `p |>> f` applies the parser `p` and
/// returns the result `f x`,  where `x` is the result returned by `p`.
/// `p |>> f` is an optimized implementation of `p >>= fun x -> preturn (f x)`.
val (|>>): Parser<'a,'u> -> ('a -> 'b) -> Parser<'b,'u>

/// The parser `pipe2 p1 p2 f` applies the parsers `p1` and `p2` in sequence.
/// It returns the result `f a b`, where `a` and `b` are the results returned by `p1` and `p2`.
/// `pipe2 p1 p2 f` is an optimized implementation of `p1 >>= fun a -> p2 >>= fun b -> preturn (f a b)`.
val pipe2: Parser<'a,'u> -> Parser<'b,'u> -> ('a -> 'b -> 'c) -> Parser<'c,'u>

/// The parser `pipe3 p1 p2 p3 f` applies the parsers `p1`, `p2` and `p3` in sequence.
/// It returns the result `f a b c`, where `a`, `b` and `c` are the results returned by `p1`, `p2` and `p3`.
/// `pipe3 p1 p2 p3 f` is an optimized implementation of `p1 >>= fun a -> p2 >>= fun b -> p3 >>= fun c -> preturn (f a b c)`.
val pipe3: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> ('a -> 'b -> 'c -> 'd) -> Parser<'d,'u>

/// The parser `pipe4 p1 p2 p3 p4 f` applies the parsers `p1`, `p2`, `p3` and `p4` in sequence.
/// It returns the result `f a b c d`, where `a`, `b`, `c` and `d` are the results returned by `p1`, `p2`, `p3` and `p4`.
/// `pipe4 p1 p2 p3 p4 f` is an optimized implementation of `p1 >>= fun a -> p2 >>= fun b -> p3 >>= fun c -> p4 >>= fun d -> preturn (f a b c d)`.
val pipe4: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> Parser<'d,'u> -> ('a -> 'b -> 'c -> 'd -> 'e) -> Parser<'e,'u>

/// The parser `pipe5 p1 p2 p3 p4 p5 f` applies the parsers `p1`, `p2`, `p3`, `p4` and `p5` in sequence.
/// It returns the result of the function application `f a b c d e`, where `a`, `b`, `c`, `d` and `e` are the results returned by `p1`, `p2`, `p3`, `p4` and `p5`.
/// `pipe5 p1 p2 p3 p4 p5 f` is an optimized implementation of `p1 >>= fun a -> p2 >>= fun b -> p3 >>= fun c -> p4 >>= fun d -> p5 >>= fun e -> preturn (f a b c d e)`.
val pipe5: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> Parser<'d,'u> -> Parser<'e,'u> -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> Parser<'f, 'u>


// -----------------------------------------------
// Parsing alternatives and recovering from errors
// -----------------------------------------------

/// The parser `p1 <|> p2` first applies the parser `p1`.
/// If `p1` succeeds, the result of `p1` is returned.
/// If `p1` fails with a non-fatal error and with an *output state equal to the input state*,
/// the parser `p2` is applied.
/// Note: The stream position is part of the state, so if `p1` fails after consuming input,
/// `p2` will not be applied.
val (<|>): Parser<'a,'u> -> Parser<'a,'u> -> Parser<'a,'u>

//  A straighforward implementation of this combinator is:
//
//  let (<|>) (p1: Parser<'a,'u>) (p2: Parser<'a,'u>) : Parser<'a,'u> =
//      fun state ->
//          let reply1 = p1 state
//          if reply1.Status = Error && reply1.State = state then
//              let mutable reply2 = p2 state
//              if reply2.State = reply1.State then
//                  reply2.Error <- mergeErrors reply1.Error reply2.Error
//              reply2
//          else reply1
//
// The most important point to note here is that `p1 <|> p2` will always return with the
// reply of `p1` in case `p1` changes the state, even if `p1` eventually
// fails. Since a parser usually consumes input as soon as it can accept at least one
// atomic token from the input, this means that `p1 <|> p2` by default implements
// backtracking with only a one token look-ahead.
//
// Restricting the look-ahead this way has two advantages:
// 1) Error reporting is simplified and error messages are easier to understand
// because terminal errors can only occur at one position at a time.
// 2) Parser developers are guided towards more efficient grammar implementations
// because parsers requiring more than one token look-ahead need to be explicitly
// annotated with the `attempt` combinator (see below).

/// The parser `choice ps` is an optimized implementation of `p1 <|> p2 <|> ... <|> pn <|> pzero`,
/// where `p1` ... `pn` are the parsers in the sequence `ps`
val choice: seq<Parser<'a,'u>> -> Parser<'a,'u>

/// The parser `choiceL ps label` is an optimized implementation of `choice ps <?> label`.
val choiceL: seq<Parser<'a,'u>> -> string -> Parser<'a,'u>

/// The parser `p <|>$ x` is equivalent to `p <|> preturn x`.
val (<|>$): Parser<'a,'u> -> 'a -> Parser<'a,'u>

/// The parser `opt p` parses an optional occurrence of `p` as an option value.
/// `opt p` is an optimized implementation of `(p |>> Some) <|>$ None`.
val opt: Parser<'a,'u> -> Parser<'a option,'u>

/// The parser `optional p` skips over an optional occurrence of `p`.
/// `optional p` is an optimized implementation of `(p >>$ ()) <|>$ ()`.
val optional: Parser<'a,'u> -> Parser<unit,'u>


/// The parser `attempt p` applies the parser `p`.
/// If `p` fails with an output state different from the input state,
/// `attempt p` will backtrack to the original input state and then report a non-fatal error.
/// Thus, `attempt p1 <|> p2` will continue to try to parse `p2` even
/// if `p1` fails after consuming input or if `p1` fails with a fatal error.
val attempt: Parser<'a,'u> -> Parser<'a,'u>

// The following combinators come in handy when grammar productions have
// optional prefixes that would disrupt error recovery in `choice` statements.
// For example, `choice [spaces >>. p1; p2; spaces >>. p3]`, where `spaces`
// parses zero or more whitespaces, will not parse `p3` if is preceded by one or
// more whitespaces, since in that case the first parser alternative fails after
// having consumed the whitespaces. Usually this problem is best addressed by
// grouping alternatives with a common prefix and then factoring out the prefix.
// However, sometimes that's too bothersome and a solution like
// `choice [spaces >>? p1; p2; spaces >>? p3]` is more appropriate.
// (If one used `attempt (spaces >>. p1)` instead of `spaces >>? p1`, the alternatives
// `p2` and `p3` would be tried for any error in `p1`, even ones that occur deeply
// inside `p1`.

/// The parser `p >>=? f` behaves like `p >>= f`, except that it will
/// backtrack to the beginning if the parser returned by `f` fails with a
/// non-fatal error and with an unchanged state, even if `p` has changed the state.
val (>>=?): Parser<'a,'u> -> ('a -> Parser<'b,'u>) -> Parser<'b,'u>

/// The parser `p1 >>? p2` behaves like `p1 >>. p2`, except that it will backtrack
/// to the beginning if `p2` fails with a non-fatal error and without changing state,
/// even if `p1` has changed the state.
val (>>?): Parser<'a,'u> -> Parser<'b,'u> -> Parser<'b,'u>

/// The parser `p1 .>>? p2` behaves like `p1 .>> p2`, except that it will backtrack
/// to the beginning if `p2` fails with a non-fatal error and without changing state,
/// even if `p1` has changed the state.
val (.>>?): Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a,'u>


// -------------------------------------
// Conditional parsing and looking ahead
// -------------------------------------

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

/// The parser `lookAhead p` parses `p` and restores the original input state afterwards.
/// In case `p` fails, the error messages are wrapped in a `BacktrackError`.
/// If it succeeds, any error messages are discarded. Fatal errors are turned into normal errors.
val lookAhead: Parser<'a,'u> -> Parser<'a,'u>


// --------------------------
// Customizing error messages
// --------------------------

/// The parser `p <?> label` applies the parser `p`. If the output state returned by `p`
/// equals the input state (usually because `p` failed), the error messages are replaced
/// with `expectError label`.
val (<?>): Parser<'a,'u> -> string -> Parser<'a,'u>

/// The parser `p <??> label` behaves like `p <?> label`, except that when `p` fails
/// with a changed state (for example because `p` consumed input before it failed),
/// a `CompoundError` message is generated with both the given string `label` and the
/// error messages generated by `p`.
val (<??>): Parser<'a,'u> -> string -> Parser<'a,'u>

/// The parser `fail msg` always fails with a `messageError msg`.
/// The error message will be displayed together with other error messages generated for
/// the same input position.
/// `fail msg` is equivalent to `fun state -> Reply<_,_>(Error, messageError msg, state)`.
val fail: string -> Parser<'a,'u>

/// The parser `failFatally msg` always fails with a `messageError msg`. It signals a
/// FatalError, so that no error recovery is attempted (except via backtracking constructs).
/// `failFatally msg` is equivalent to `fun state -> Reply<_,_>(FatalError, messageError msg, state)`.
val failFatally: string -> Parser<'a,'u>

// -----------------
// Parsing sequences
// -----------------

/// The parser `tuple2 p1 p2` applies the parsers `p1` and `p2` in sequence and
/// returns the results in a tuple.
/// `tuple2 p1 p2` is equivalent to `pipe2 p1 p2 (fun a b -> (a, b))`.
val tuple2: Parser<'a,'u> -> Parser<'b,'u> -> Parser<('a * 'b),'u>

/// The parser `tuple3 p1 p2 p3` applies the parsers `p1`, `p2` and `p3` in sequence and
/// returns the results in a tuple.
/// `tuple3 p1 p2 p3` is equivalent to `pipe3 p1 p2 p3 (fun a b c -> (a, b, c))`.
val tuple3: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> Parser<('a * 'b * 'c),'u>

/// The parser `tuple4 p1 p2 p3 p4` applies the parsers `p1`, `p2`, `p3` and `p4` in sequence and
/// returns the results in a tuple.
/// `tuple4 p1 p2 p3 p4` is equivalent to `pipe4 p1 p2 p3 p4 (fun a b c d -> (a, b, c, d))`.
val tuple4: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u> -> Parser<'d,'u>  -> Parser<('a * 'b * 'c * 'd),'u>

/// The parser `tuple5 p1 p2 p3 p4 p5` applies the parsers `p1`, `p2`, `p3`, `p4` and `p5` in sequence and
/// returns the results in a tuple.
/// `tuple5 p1 p2 p3 p4 p5` is equivalent to `pipe5 p1 p2 p3 p4 p5 (fun a b c d e -> (a, b, c, d, e))`
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
/// At the end of the sequence `p` must fail without changing the state and without
/// signalling a `FatalError`, otherwise `many p` fails with the error reported by `p`.
/// `many p` tries to guard against an infinite loop by throwing an exception
/// if `p` succeeds without changing the parser state.
val many: Parser<'a,'u> -> Parser<'a list,'u>

/// The parser `skipMany p` is an optimized implementation of `many p |>> ignore`.
val skipMany: Parser<'a,'u> -> Parser<unit,'u>

/// The parser `manyRev p` is an optimized implementation of `many p |>> List.rev`.
val manyRev: Parser<'a,'u> -> Parser<'a list,'u>

/// The parser `manyFold acc0 f p` is an optimized implementation of
/// `many p |>> List.fold f acc0`.
val manyFold: 'b -> ('b -> 'a -> 'b) -> Parser<'a,'u> -> Parser<'b,'u>

/// The parser `manyReduce f defVal p` is an optimized implementation of
/// `(many1 p |>> List.reduce f) <|>$ defVal`.
val manyReduce:  ('a -> 'a -> 'a) -> 'a -> Parser<'a,'u> -> Parser<'a,'u>


// p+

/// The parser `many1 p` behaves like `many p`, except that it requires `p` to succeed at least one time.
/// `many1 p` is an optimized implementation of `pipe2 p (many p) (fun hd tl -> hd::tl)`.
val many1: Parser<'a,'u> -> Parser<'a list,'u>

/// The parser `skipMany1 p` is an optimized implementation of `many1 p |>> ignore`.
val skipMany1: Parser<'a,'u> -> Parser<unit,'u>

/// The parser `many1Rev p` is an optimized implementation of `many1 p |>> List.rev`.
val many1Rev: Parser<'a,'u> -> Parser<'a list,'u>

/// The parser `many1Fold acc0 f p` is an optimized implementation of
/// `many1 p |>> List.fold f acc0`.
val many1Fold: 'b -> ('b -> 'a -> 'b) -> Parser<'a,'u> -> Parser<'b,'u>

/// The parser `many1Reduce f` is an optimized implementation of
/// `many1 p |>> List.reduce f`.
val many1Reduce: ('a -> 'a -> 'a) -> Parser<'a,'u> -> Parser<'a,'u>

/// The parser `manyFoldApply f1 foldF applyF emptyF p` expands to an optimized implementation of
///
/// many p
/// |>> function
///     | []     -> emptyF ()
///     | hd::tl -> applyF (List.fold foldF (f1 hd) tl)
#if NOINLINE
val
#else
val inline
#endif
           manyFoldApply:    ('a -> 'b) -> ('b -> 'a -> 'b) -> ('b -> 'c) -> (unit -> 'c)
                          -> Parser<'a,'u> -> Parser<'c,'u>

/// The parser `manyFoldApply2 f1 foldF applyF emptyF p1 p` expands to an optimized implementation of
///
/// pipe2 p1 (many p) (fun hd tl -> hd::tl) <|>$ []
/// |>> function
///     | []     -> emptyF ()
///     | hd::tl -> applyF (List.fold foldF (f1 hd) tl)
#if NOINLINE
val
#else
val inline
#endif
           manyFoldApply2:    ('a -> 'b) -> ('b -> 'c -> 'b) -> ('b -> 'd) -> (unit -> 'd)
                           -> Parser<'a,'u> -> Parser<'c,'u> -> Parser<'d,'u>

/// The parser `many1FoldApply f1 foldF applyF p` expands to an optimized implementation of
/// `many1 p |>> function hd::tl -> applyF (List.fold foldF (f1 hd) tl)`.
#if NOINLINE
val
#else
val inline
#endif
           many1FoldApply:    ('a -> 'b) -> ('b -> 'a -> 'b) -> ('b -> 'c)
                           -> Parser<'a,'u> -> Parser<'c,'u>

/// The parser `many1FoldApply2 f1 foldF applyF p1 p` expands to an optimized implementation of
/// `pipe2 p1 (many p) (fun hd tl -> applyF (List.fold foldF (f1 hd) tl))`
#if NOINLINE
val
#else
val inline
#endif
           many1FoldApply2:   ('a -> 'b) -> ('b -> 'c -> 'b) -> ('b -> 'd)
                           -> Parser<'a,'u> -> Parser<'c,'u> -> Parser<'d,'u>


// (p (sep p)*)?

/// The parser `sepBy p sep` parses *zero* or more occurrences of `p` separated by `sep`
/// (in EBNF notation: `(p (sep p)*)?`).
val sepBy: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `skipSepBy p sep` is an optimized implementation of `sepBy p sep |>> ignore`.
val skipSepBy: Parser<'a,'u> -> Parser<'b,'u> -> Parser<unit,'u>

/// The parser `sepByRev p sep` is an optimized implementation of `sepBy p sep |>> List.rev`.
val sepByRev: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `sepByFold acc0 f p sep` is an optimized implementation of
/// `sepBy p sep |>> List.fold f acc0`.
val sepByFold: 'c -> ('c -> 'a -> 'c) -> Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u>

/// The parser `sepByReduce f defVal p sep` is an optimized implementation of
/// `(sepBy1 p sep |>> List.Reduce f) <|>$ defVal`.
val sepByReduce: ('a -> 'a -> 'a) -> 'a -> Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a,'u>

/// The parser `sepByFoldApply f1 foldF applyF emptyF p sep` expands to an optimized implementation of
///
/// sepBy p sep
/// |>> function
///     | []     -> emptyF ()
///     | hd::tl -> applyF (List.fold foldF (f1 hd) tl)
#if NOINLINE
val
#else
val inline
#endif
           sepByFoldApply:     ('a -> 'b) -> ('b -> 'a -> 'b) -> ('b -> 'c) -> (unit -> 'c)
                            -> Parser<'a,'u> -> Parser<'d,'u> -> Parser<'c,'u>


// p (sep p)*

/// The parser `sepBy1 p sep` parses *one* or more occurrences of `p` separated by `sep`
/// (in EBNF notation: `p (sep p)*`).
val sepBy1: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `skipSepBy1 p sep` is an optimized implementation of `sepBy1 p sep |>> ignore`.
val skipSepBy1: Parser<'a,'u> -> Parser<'b,'u> -> Parser<unit,'u>

/// The parser `sepBy1Rev p sep` is an optimized implementation of `sepBy1 p sep |>> List.rev`.
val sepBy1Rev: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `sepByFold acc0 f p sep` is an optimized implementation of
/// `sepBy1 p sep |>> List.fold f acc0`.
val sepBy1Fold: 'c -> ('c -> 'a -> 'c) -> Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u>

/// The parser `sepBy1Reduce f p sep` is an optimized implementation of
/// `sepBy1 p sep |>> List.reduce f`.
val sepBy1Reduce: ('a -> 'a -> 'a) -> Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a,'u>

/// The parser `sepBy1FoldApply f1 foldF applyF p sep` expands to an optimized implementation of
/// `sepBy1 p |>> function hd::tl -> applyF (List.fold foldF (f1 hd) tl)`.
#if NOINLINE
val
#else
val inline
#endif
           sepBy1FoldApply:    ('a -> 'b) -> ('b -> 'a -> 'b) -> ('b -> 'c)
                            -> Parser<'a,'u> -> Parser<'d,'u> -> Parser<'c,'u>

// (p (sep p)* sep?)?

/// The parser `sepEndBy p sep` parses *zero* or more occurrences of `p` separated and
/// optionally ended by `sep` (in EBNF notation: `(p (sep p)* sep?)?`).
/// It returns a list of the results returned by `p`.
val sepEndBy: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `skipSepEndBy p sep` is an optimized implementation of `sepEndBy p sep |>> ignore`.
val skipSepEndBy: Parser<'a,'u> -> Parser<'b,'u> -> Parser<unit,'u>

/// The parser `sepEndByRev p sep` is an optimized implementation of `sepEndBy p sep |>> List.rev`.
val sepEndByRev: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `sepEndByFold acc0 f p sep` is an optimized implementation of
/// `sepEndBy p sep |>> List.fold f acc0`.
val sepEndByFold: 'c -> ('c -> 'a -> 'c) -> Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u>

/// The parser `sepEndByReduce f defVal p sep` is an optimized implementation of
/// `(sepEndBy1 p sep |>> List.Reduce f) <|>$ defVal`.
val sepEndByReduce: ('a -> 'a -> 'a) -> 'a -> Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a,'u>

/// The parser `sepEndByFoldApply f1 foldF applyF emptyF p sep` expands to an optimized implementation of
///
/// sepEndBy p sep
/// |>> function
///     | []     -> emptyF ()
///     | hd::tl -> applyF (List.fold foldF (f1 hd) tl)
#if NOINLINE
val
#else
val inline
#endif
           sepEndByFoldApply:     ('a -> 'b) -> ('b -> 'a -> 'b) -> ('b -> 'c) -> (unit -> 'c)
                               -> Parser<'a,'u> -> Parser<'d,'u> -> Parser<'c,'u>


// p (sep p)* sep?

/// The parser `sepEndBy1 p sep` parses *one* or more occurrences of `p` separated and
/// optionally ended by `sep` (in EBNF notation: `p (sep p)* sep?`).
/// It returns a list of the results returned by `p`.
val sepEndBy1: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `skipSepEndBy p sep` is an optimized implementation of `sepEndBy p sep |>> ignore`.
val skipSepEndBy1: Parser<'a,'u> -> Parser<'b,'u> -> Parser<unit,'u>

/// The parser `sepEndBy1Rev p sep` is an optimized implementation of `sepEndBy p sep |>> List.rev`.
val sepEndBy1Rev: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `sepEndByFold acc0 f p sep` is an optimized implementation of
/// `sepEndBy p sep |>> List.fold f acc0`.
val sepEndBy1Fold: 'c -> ('c -> 'a -> 'c) -> Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u>

/// The parser `sepEndBy1Reduce f p sep` is an optimized implementation of
/// `sepEndBy1 p sep |>> List.reduce f`.
val sepEndBy1Reduce: ('a -> 'a -> 'a) -> Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a,'u>

/// The parser `sepEndBy1FoldApply f1 foldF applyF p sep` expands to an optimized implementation of
/// `sepEndBy1 p |>> function hd::tl -> applyF (List.fold foldF (f1 hd) tl)`.
#if NOINLINE
val
#else
val inline
#endif
           sepEndBy1FoldApply:    ('a -> 'b) -> ('b -> 'a -> 'b) -> ('b -> 'c)
                               -> Parser<'a,'u> -> Parser<'d,'u> -> Parser<'c,'u>


/// The parser `manyTill p endp` repeatedly applies the parser `p`
/// while `endp` does not succeed.
/// It stops at the parser state returned by `endp` and
/// returns a list of the results returned by `p`.
/// `manyTill p endp` is an optimized implementation of `many (notFollowedBy endp >>. p) .>> endp`
/// that doesn't have to apply `endp` twice at the end of the sequence.
val manyTill: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `skipManyTill p endp` is an optimized implementation of `manyTill p endp |>> ignore`.
val skipManyTill: Parser<'a,'u> -> Parser<'b,'u> -> Parser<unit,'u>

/// The parser `manyTillRev p endp` is an optimized implementation of `manyTill p endp |>> List.rev`.
val manyTillRev: Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a list,'u>

/// The parser `manyTillFold acc0 f p endp` is an optimized implementation of `manyTill p endp |>> List.fold f acc0`.
val manyTillFold: 'c -> ('c -> 'a -> 'c) -> Parser<'a,'u> -> Parser<'b,'u> -> Parser<'c,'u>

/// The parser `manyTillReduce f defVal p endp` is an optimized implementation of
///
/// manyTill p endp
/// |>> function
///     | []  -> defVal
///     | lst -> List.reduce f
val manyTillReduce: ('a -> 'a -> 'a) -> 'a -> Parser<'a,'u> -> Parser<'b,'u> -> Parser<'a,'u>

/// The parser `manyTillFoldApply f1 foldF applyF emptyF p sep` expands to an optimized implementation of
///
/// pipe2 (many (notFollowedBy endp >>. p)) endp
///       (fun lst b ->
///            match lst with
///            | []     -> emptyF b
///            | hd::tl -> applyF (List.fold foldF (f1 hd) tl)) b
#if NOINLINE
val
#else
val inline
#endif
           manyTillFoldApply:    ('a -> 'c) -> ('c -> 'a -> 'c) -> ('c -> 'b -> 'd) -> ('b -> 'd)
                              -> Parser<'a,'u> -> Parser<'b,'u> -> Parser<'d,'u>


// (((p op p) op p) ... op p)

/// The parser `chainl1 p op` parses one or more occurrences of `p` separated by `op`
/// (in EBNF notation: `p (op p)*`).
/// It returns the value obtained by *left* associative application of all functions
/// returned by `op` to the results returned by `p`,
/// i.e. `f_n (... (f_2 (f_1 x_1 x_2) x_3) ...) x_n+1`,
/// where `f_1` to `f_n` are the functions returned by the parser `op` and
/// `x_1` to `x_n+1` are the values returned by `p`.
val chainl1: Parser<'a,'u> -> Parser<('a -> 'a -> 'a),'u>       -> Parser<'a,'u>

/// The parser `chainl p op defVal` is equivalent to `chainl1 p op <|>$ defVal`.
val chainl:  Parser<'a,'u> -> Parser<('a -> 'a -> 'a),'u> -> 'a -> Parser<'a,'u>


// (p op ... (p op (p op p)))

/// The parser `chainr1 p op` parses one or more occurrences of `p` separated by `op`
/// (in EBNF notation: `p (op p)*`).
/// It returns the value obtained by *right* associative application of all functions
/// returned by `op` to the results returned by `p`,
/// i.e. `f1 x_1 (f_2 x_2 (... (f_n x_n x_n+1) ...))`,
/// where `f_1` to `f_n` are the functions returned by the parser `op` and
/// `x_1` to `x_n+1` are the values returned by `p`.
val chainr1: Parser<'a,'u> -> Parser<('a -> 'a -> 'a),'u>       -> Parser<'a,'u>

/// The parser `chainr p op defVal` is equivalent to `chainr1 p op <|>$ defVal`.
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

