// Copyright (c) Stephan Tolksdorf 2008-2009
// License: Simplified BSD License. See accompanying documentation.

module FParsec.OperatorPrecedenceParser

open FParsec.Primitives

// Represents the associativity of infix operators.
type Assoc = None    = 0
           | Left    = 1
           | Right   = 2

/// This union type is used to define operators for an OperatorPrecedenceParser.
[<ReferenceEquality>]
type PrecedenceParserOp<'a,'u> =
     // the operators are parsed as if they were given as "pstring opString >>? whitespaceAfterOpParser"

     /// PrefixOp(opString, wsAfterOpParser, precedence, isAssociative, f)
     /// represents a prefix operator definition for the `OperatorPrecedenceParser`.
     | PrefixOp  of string * Parser<unit,'u> * int * bool * ('a -> 'a)
     /// PostfixOp(opString, wsAfterOpParser, precedence, isAssociative, f)
     /// represents a postfix operator definition for the `OperatorPrecedenceParser`.
     | PostfixOp of string * Parser<unit,'u> * int * bool * ('a -> 'a)
     /// InfixOp(opString, wsAfterOpParser, precedence, associativity, f)
     /// represents an infix operator definition for the `OperatorPrecedenceParser`.
     | InfixOp   of string * Parser<unit,'u> * int * Assoc * ('a -> 'a -> 'a)
     /// TernaryOp(op1String, wsAfterOp1Parser, op2String, wsAfterOp2Parser, precedence, associativity, f)
     /// represents a ternary operator definition for the `OperatorPrecedenceParser`.
     | TernaryOp of string * Parser<unit,'u> *
                    string * Parser<unit,'u> * int * Assoc * ('a -> 'a -> 'a -> 'a)

     /// PrefixOp'(opString, wsAfterOpParser, precedence, isAssociative, f),
     /// The state passed as an argument to the function is captured immediately before
     /// the operator string is parsed and contains the position of the operator.
     | PrefixOp'  of string * Parser<unit,'u> * int * bool * (State<'u> -> 'a -> 'a)
     /// PostfixOp'(opString, wsAfterOpParser, precedence, isAssociative, f),
     /// The state passed as an argument to the function is captured immediately before
     /// the operator string is parsed and contains the position of the operator.
     | PostfixOp' of string * Parser<unit,'u> * int * bool * (State<'u> -> 'a -> 'a)
     /// InfixOp'(opString, wsAfterOpParser, precedence, associativity, f),
     /// The state passed as an argument to the function is captured immediately before
     /// the operator string is parsed and contains the position of the operator.
     | InfixOp'   of string * Parser<unit,'u> * int * Assoc * (State<'u> -> 'a -> 'a -> 'a)
     /// TernaryOp'(op1String, wsAfterOp1Parser, op2String, wsAfterOp2Parser, precedence, associativity, f)
     /// The states passed as arguments to the function are captured immediately before
     /// the first and second operator strings are parsed and contain the positions of the operators.
     | TernaryOp' of string * Parser<unit,'u> *
                     string * Parser<unit,'u> * int * Assoc * (State<'u> -> State<'u> -> 'a -> 'a -> 'a -> 'a)
     with
         override ToString: unit -> string

/// Represents a dynamically configurable parser for parsing expressions involving
/// prefix, postfix, infix and ternary operators of different precedence and associativity.
type OperatorPrecedenceParser<'a,'u> =
    /// Constructs an OperatorPrecedenceParser instance and optionally adds the given operators.
    new: ?ops:seq<PrecedenceParserOp<'a,'u>> -> OperatorPrecedenceParser<'a,'u>

    // Operators with higher precedence bind tighter.

    // The middle expression in a ternary expression (e.g. expr2 in "expr1 ? expr2 : expr3")
    // is parsed as a "fresh" expression that is not influenced by the precedence of the
    // surrounding operators.

    // Expressions involving operators with identical precedence are parsed as follows
    // (where  o1,   o2   are two infix operators,
    //         pre1, pre2 are two prefix operators,
    //         po1,  po2  are two postfix operators
    //  and all operators have identical precedence):

    //  x o1 pre1 y ==> x o1 (pre1 y),
    //  x o1 y po1  ==> x o1 (y po1),
    //  x o1 y o2 z ==> (x o1 y) o2 z,  if o1   and o2   are left-associative
    //  x o1 y o2 z ==> x o1 (y o2 z),  if o1   and o2   are right-associative
    //  pre1 x po1  ==> (pre1 x) po1,   if pre1 or po1  is associative
    //  pre1 pre2 x ==> pre1 (pre2 x),  if pre1 or pre2 is associative
    //  x po1 po2   ==> (x po1) po2,    if po1  or po2  is associative

    // In the following situations the OperatorConflictHandler will be called
    // and the expression will only be parsed as indicated if the handler returns
    // an empty error message. The default handler always generate an error.

    //  x o1 y o2 z ==> (x o1 y) o2 z, if o1 and o2 have different associativity,
    //                                 or o1 and o2 are non-associative
    //  pre1 pre2 x ==> pre1 (pre2 y), if pre1 and pre2 are non-associative
    //  pre1 x po1  ==> (pre1 y) po1,  if pre1 and po1  are non-associative
    //  x po1 po2   ==> (y po1) po2,   if po1  and po2  are non-associative

    /// The expression parser. This is a constant closure that forwards all work to an
    /// internal instance method, so that the expression parser always reflects the latest
    /// configuration of the `OperatorPrecedenceParser` instance.
    /// The parser can be safely called from different threads as long as the
    /// `OperatorPrecedenceParser` instance is not mutated at the same time.
    member ExpressionParser: Parser<'a,'u>

    /// This parser is called to parse the terms in between the operators. There is no default,
    /// so you must set this parser before you can call the `ExpressionParser`.
    /// Note that the term parser is also expected to parse any whitespace after a term.
    member TermParser: Parser<'a, 'u> with get, set

    /// This function is called when the precedence parser encounters two conflicting
    /// operators in the parser input.
    /// If the conflict handler returns `null` or an empty string, the operators are
    /// parsed as if both were (left-)associative, otherwise a parser error with the
    /// returned message is generated.
    /// The default handler will always generate an error message.
    member OperatorConflictHandler: (   State<'u> -> PrecedenceParserOp<'a,'u>
                                     -> State<'u> -> PrecedenceParserOp<'a,'u>
                                     -> string) with get, set

    /// Adds an operator to the grammar.
    /// Raises an `ArgumentException` if the operator definition conflicts with a
    /// a previous definition or contains empty strings or a non-positive precedence.
    member AddOperator: PrecedenceParserOp<'a,'u> -> unit

    /// Calls `AddOperator` with each operator in the given sequence.
    member AddOperators: seq<PrecedenceParserOp<'a,'u>> -> unit

    /// Removes the given operator from the grammar.
    /// Returns `false` if the operator was not previously registered, otherwise true.
    member RemoveOperator: PrecedenceParserOp<'a,'u> -> bool

    /// Removes the infix operator with the given string from the grammar.
    /// Returns `false` if no infix operator with that string was previously registered, otherwise `true`.
    member RemoveInfixOp: string -> bool

    /// Removes the prefix operator with the given string from the grammar.
    /// Returns `false` if no prefix operator with that string was previously registered, otherwise `true`.
    member RemovePrefixOp: string -> bool

    /// Removes the postfix operator with the given string from the grammar.
    /// Returns `false` if no postfix operator with that string was previously registered, otherwise `true`.
    member RemovePostfixOp: string -> bool

    /// Removes the ternary operator with the given strings from the grammar.
    /// Returns `false` if no ternary operator with these strings was previously registered, otherwise `true`.
    member RemoveTernaryOp: string*string -> bool

    /// Returns a sequence with a snapshot of the operators currently registered with the `OperatorPrecedenceParser`.
    member Operators: seq<PrecedenceParserOp<'a,'u>>
