// Copyright (c) Stephan Tolksdorf 2009
// License: Simplified BSD License. See accompanying documentation.

module internal FParsec.Internals

// the following functions are defined using inline IL to help fsc generate code the JIT knows better how to optimize
let inline isNull<'a when 'a : not struct> (x: 'a) = (# "ldnull ceq" x : bool #) // match box x with null -> true  | _ -> false
let inline isNotNull<'a when 'a : not struct> (x: 'a) = (# "ldnull cgt.un" x : bool #) // match box x with null -> false | _ -> true
let inline referenceEquals<'a when 'a : not struct> (x: 'a) (y: 'a) = LanguagePrimitives.PhysicalEquality x y

let inline isNullOrEmpty (s: string) = isNull s || s.Length = 0

// These operators are faster than = and <>. They are not public because
// their names conflict with the operators in the OCaml compatibility module
let inline (==) (s1: State<'u>) (s2: State<'u>) = s1.Equals(s2)
let inline (!=) (s1: State<'u>) (s2: State<'u>) = not (s1 == s2)

// the F# compiler doesn't yet "fuse" multiple '+' string concatenations into one, as the C# compiler does
let inline concat3 (a: string) (b: string) (c: string) = System.String.Concat(a, b, c)
let inline concat4 (a: string) (b: string) (c: string) (d: string) = System.String.Concat(a, b, c, d)

let containsNewlineChar = Helper.ContainsNewlineChar

let inline private escapeCharHelper (isChar: bool) (c: char) (f: char -> string) =
    if c > '\'' && c < '\u007f' then
        if c <> '\\' then f c else "\\\\"
    else
        match c with
        | '\b' -> "\\b"
        | '\t' -> "\\t"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        //| '\"' -> if isChar then "\"" else "\\\""
        //| '\'' -> if isChar then "\\'"  else "'"
        | _ -> if System.Char.IsControl(c) then
                   let c = int c
                   let cs = Array.zeroCreate 6
                   cs.[0] <- '\\'; cs.[1] <- 'u'
                   for j = 0 to 3 do
                       cs.[5 - j] <- "0123456789abcdef".[((c >>> 4*j) &&& 0xf)]
                   new string(cs)
                else f c

let private escapeCharInString c = escapeCharHelper false c (fun c -> System.Char.ToString(c))

let private escapeString (s: string) =
    let rec escape sb i start =
        if i < s.Length then
            let esc = escapeCharHelper false s.[i] (fun _ -> null)
            if isNull esc then escape sb (i + 1) start
            else
                let sb = if isNull sb then new System.Text.StringBuilder(s.Length + 5)
                         else sb
                sb.Append(s, start, i - start).Append(esc) |> ignore
                escape sb (i + 1) (i + 1)
        elif isNull sb then s
        else sb.Append(s, start, s.Length - start).ToString()
    escape null 0 0

let quoteChar c = if c <> '\'' then concat3 "'" (escapeCharInString c) "'" else "\"'\""
let quoteString s = concat3 "'" (escapeString s) "'"
