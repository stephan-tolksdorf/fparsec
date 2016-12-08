// Copyright (c) Stephan Tolksdorf 2010-2011
// License: Simplified BSD License. See accompanying documentation.

module FParsec.StaticMapping

#if LOW_TRUST
#else

/// `createStaticCharIndicatorFunction invert charsInSet`
/// creates an optimized indicator function for the chars specified by the `charsInSet` sequence.
/// If `invert` is `false` (`true`), the returned indicator function will return `true` (`false`)
/// if and only if it is called with a char contained in `charsInSet`.
val createStaticCharIndicatorFunction:
        invert: bool -> charsInSet: seq<char>   -> (char -> bool)

/// `createStaticCharRangeIndicatorFunction invert rangesInSet`
/// creates an optimized indicator function for the chars in the ranges specified by the `rangesInSet` sequence.
/// If `invert` is `false` (`true`), the returned indicator function will return `true` (`false`) if and only if it is
/// called with a char contained in at least one of the ranges of `rangesInSet`.
val createStaticCharRangeIndicatorFunction:
        invert: bool -> rangesInSet: seq<Range> -> (char -> bool)

/// `createStaticIntIndicatorFunction invert valuesInSet`
/// creates an optimized indicator function for the integers specified by the `valuesInSet` sequence.
/// If `invert` is `false` (`true`), the returned indicator function will return `true` (`false`) if and only if it is
/// called with an integer contained in `valuesInSet`.
val createStaticIntIndicatorFunction:
        invert: bool -> valuesInSet: seq<int>   -> (int -> bool)

/// `createStaticIntRangeIndicatorFunction invert rangesInSet`
/// creates an optimized indicator function for the integers in the ranges specified by the `rangesInSet` sequence.
/// If `invert` is `false` (`true`), the returned indicator function will return `true` (`false`) if and only if it is
/// called with an `int` contained in at least one of the ranges of `rangesInSet`.
val createStaticIntRangeIndicatorFunction:
        invert: bool -> rangesInSet: seq<Range> -> (int -> bool)

/// `createStaticIntMapping defaultValue keyValues`
/// creates an optimized mapping function that maps integer keys to values.
/// The `keyValues` sequence specifies the key-value pairs for the mapping.
/// All keys not specified in `keyValues` are mapped to `defaultValue`.
val createStaticIntMapping:
        defaultValue: 'T -> keyValues: #seq<int*'T>   -> (int -> 'T)

/// `createStaticIntRangeMapping defaultValue keyValues`
/// creates an optimized mapping function that maps integer key ranges to values.
/// The `keyValues` sequence specifies the range-value pairs for the mapping.
/// All keys not contained in one of the ranges in `keyValues` are mapped to `defaultValue`.
val createStaticIntRangeMapping:
        defaultValue: 'T -> keyValues: #seq<Range*'T> -> (int -> 'T)

/// `createStaticStringMapping defaultValue keyValues`
/// creates an optimized mapping function that maps string keys to values.
/// The `keyValues` sequence specifies the key-value pairs for the mapping.
/// All keys not specified in `keyValues` are mapped to `defaultValue`. A `null` key is not supported.
val createStaticStringMapping:
        defaultValue: 'T -> keyValues: #seq<string*'T> -> (string -> 'T)


val internal filterOutDefaultValueRanges:
                  comparer: System.Collections.Generic.EqualityComparer<'T>
               -> ranges: Range[]
               -> values: 'T[]
               -> defaultValue: 'T
               -> Range[]*'T[]

val internal createStaticIntIndicatorFunctionImpl<'TInt when 'TInt : struct> :
                  lengthCap: int -> densityThreshold: double
               -> minValue: int -> maxValue: int
               -> invert: bool -> ranges: Range[]
               -> ('TInt -> bool)

val internal createStaticIntMappingImpl:
                  lengthCap: int -> densityThreshold: double
               -> minKey: int -> maxKey: int
               -> defaultValue: 'T -> ranges: Range[] -> values: 'T[]
               -> (int -> 'T)

#endif