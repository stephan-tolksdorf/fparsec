// Copyright (c) Stephan Tolksdorf 2008-2009
// License: Simplified BSD License. See accompanying documentation.

module FParsec.OperatorPrecedenceParser

open System.Diagnostics

open FParsec.Internals
open FParsec.Error
open FParsec.Primitives
open FParsec.CharParsers

type Assoc = None    = 0
           | Left    = 1
           | Right   = 2

[<ReferenceEquality>]
type PrecedenceParserOp<'a,'u> =
     | PrefixOp  of string * Parser<unit,'u> * int * bool * ('a -> 'a)
     | PostfixOp of string * Parser<unit,'u> * int * bool * ('a -> 'a)
     | InfixOp   of string * Parser<unit,'u> * int * Assoc * ('a -> 'a -> 'a)
     | TernaryOp of string * Parser<unit,'u> * string * Parser<unit,'u> * int * Assoc * ('a -> 'a -> 'a -> 'a)

     | PrefixOp'  of string * Parser<unit,'u> * int * bool * (State<'u> -> 'a -> 'a)
     | PostfixOp' of string * Parser<unit,'u> * int * bool * (State<'u> -> 'a -> 'a)
     | InfixOp'   of string * Parser<unit,'u> * int * Assoc * (State<'u> -> 'a -> 'a -> 'a)
     | TernaryOp' of string * Parser<unit,'u> * string * Parser<unit,'u> * int * Assoc * (State<'u> -> State<'u> -> 'a -> 'a -> 'a -> 'a)
     with
         override t.ToString() =
             let assocToString = function
                               | Assoc.Left  -> "left-associative"
                               | Assoc.Right -> "right-associative"
                               | _           -> "non-associative"
             match t with
             | PrefixOp  (str, _, prec, isAssoc, _) | PrefixOp' (str, _, prec, isAssoc, _) ->
                 sprintf "prefix operator \"%s\" (precedence: %i%s)" str prec (if isAssoc then "" else ", non-associative")
             | PostfixOp  (str, _, prec, isAssoc, _) | PostfixOp' (str, _, prec, isAssoc, _) ->
                 sprintf "postfix operator \"%s\" (precedence: %i%s)" str prec (if isAssoc then "" else ", non-associative")
             | InfixOp (str, _, prec, assoc, _) |  InfixOp' (str, _, prec, assoc, _) ->
                 sprintf "infix operator \"%s\" (precedence: %i, %s)" str prec (assocToString assoc)
             | TernaryOp (str, _, str2, _, prec, assoc,_) | TernaryOp' (str, _, str2, _, prec, assoc, _) ->
                 sprintf "ternary operator \"%s\" \"%s\" (precedence: %i, %s)" str str2 prec (assocToString assoc)

type internal Fixity = Infix   = 0
                     | Prefix  = 1
                     | Postfix = 2

// an internally used union type for operators
type internal Operator<'a,'u>(op: PrecedenceParserOp<'a,'u>) =
    let str, ws, fix, prec, assoc =
            match op with
            | PrefixOp  (str,ws,prec,isAssoc,_)    | PrefixOp'  (str,ws,prec,isAssoc,_)   -> str, ws, Fixity.Prefix,  prec, if isAssoc then Assoc.Right else Assoc.None
            | PostfixOp (str,ws,prec,isAssoc,_)    | PostfixOp' (str,ws,prec,isAssoc,_)   -> str, ws, Fixity.Postfix, prec, if isAssoc then Assoc.Left  else Assoc.None
            | InfixOp   (str,ws,prec,assoc,_)      | InfixOp'   (str,ws,prec,assoc,_)     -> str, ws, Fixity.Infix, prec, assoc
            | TernaryOp (str,ws,_,_, prec,assoc,_) | TernaryOp' (str,ws,_,_,prec,assoc,_) -> str, ws, Fixity.Infix, prec, assoc
    let str2, ws2 = match op with TernaryOp (_,_,str2,ws2,_,_,_) | TernaryOp' (_,_,str2,ws2,_,_,_) -> str2, ws2 | _ -> null, Unchecked.defaultof<_>

    let apply1  = match op with PrefixOp   (_,_,_,_,f) | PostfixOp  (_,_,_,_,f) -> f | _ -> Unchecked.defaultof<_>
    let apply1' = match op with PrefixOp'  (_,_,_,_,f) | PostfixOp' (_,_,_,_,f) -> OptimizedClosures.FastFunc2<_,_,_>.Adapt(f) | _ -> Unchecked.defaultof<_>
    let apply2  = match op with InfixOp    (_,_,_,_,f)     -> OptimizedClosures.FastFunc2<_,_,_>.Adapt(f)       | _ -> Unchecked.defaultof<_>
    let apply2' = match op with InfixOp'   (_,_,_,_,f)     -> OptimizedClosures.FastFunc3<_,_,_,_>.Adapt(f)     | _ -> Unchecked.defaultof<_>
    let apply3  = match op with TernaryOp  (_,_,_,_,_,_,f) -> OptimizedClosures.FastFunc3<_,_,_,_>.Adapt(f)     | _ -> Unchecked.defaultof<_>
    let apply3' = match op with TernaryOp' (_,_,_,_,_,_,f) -> OptimizedClosures.FastFunc5<_,_,_,_,_,_>.Adapt(f) | _ -> Unchecked.defaultof<_>

    member t.OriginalOp = op
    member t.Fixity = fix
    member t.String = str
    member t.WhitespaceAfterStringParser = ws
    member t.Ternary2ndString = str2
    member t.WhitespaceAfter2ndStringParser = ws2

    member t.Precedence = prec
    member t.Assoc = assoc

    member t.Apply1  = apply1
    member t.Apply1' = apply1'
    member t.Apply2  = apply2
    member t.Apply2' = apply2'
    member t.Apply3  = apply3
    member t.Apply3' = apply3'

    member t.IsTernary = isNotNull t.Ternary2ndString

    override t.ToString() = t.OriginalOp.ToString()


type internal OpLookahead<'a,'u> = struct
    /// the reply.State value before Op was parsed
    val mutable State: State<'u>
    /// the reply.Error value before Op was parsed
    val mutable Error: ErrorMessageList
    val mutable Op: Operator<'a,'u>

    member t.IsValid = isNotNull t.Op
    member t.SetInvalid() = t.Op <- Unchecked.defaultof<_>
end


let internal oppArrayLength = 256 // must be a power of 2

let internal expectedPrefixOp          = expectedError "prefix operator"
let internal expectedInfixOp           = expectedError "infix operator"
let internal expectedInfixOrPostfixOp  = expectedError "infix or postfix operator"
let internal expectedPostfixOp         = expectedError "postfix operator"

type OperatorPrecedenceParser<'a,'u> private () =
    let mutable expressionParser = Unchecked.defaultof<Parser<'a,'u>>
    let mutable termParser : Parser<'a,'u> = fun state -> failwith "OperatorPrecedenceParser.termParser is not initialized";
    let mutable operatorConflictHandler = OperatorPrecedenceParser<'a,'u>.DefaultOperatorConflictHandler

    // lhsOps and rhsOps are arrays of operator arrays. Both have a fixed size of
    // oppArrayLength (which must be a power of 2). All operators beginning with the
    // same char modulo oppArrayLength are grouped together in the same inner
    // array. The inner arrays are sorted by the operator.String property in descending
    // lexical order. The index of an inner array in the outer array is given by the
    // inner array's operator strings' first char modulo oppArrayLength. An empty
    // inner array is represended by null.
    // lhsOps and rhsOps both contain exactly one operator for any registered operator
    // string. If a prefix operator is registered with the same string as that of a
    // postfix, infix or ternary operator, then lhsOps contains the prefix operator
    // while rhsOps contains the other one.
    let lhsOps = Array.zeroCreate oppArrayLength
    let rhsOps = Array.zeroCreate oppArrayLength

    let mutable nPrefixOps = 0
    let mutable nInfixOps = 0
    let mutable nPostfixOps = 0

    let reserved = new System.Collections.Generic.Dictionary<_,_>()

    let zeroPrecedenceOp = Operator<'a,'u>(PrefixOp(null, spaces, -1, true, fun x -> x))

    member t.ExpressionParser = expressionParser
    member t.TermParser with get() = termParser and set v = termParser <- v
    member t.OperatorConflictHandler with get() = operatorConflictHandler and set v = operatorConflictHandler <- v

    member private t.InitializeExpressionParser() =
        expressionParser <- fun state ->
             let mutable la = Unchecked.defaultof<_>
             let mutable reply = Unchecked.defaultof<Reply<_,_>>
             reply.Status <- Ok
             reply.State <- state
             t.ParseExpression(state, zeroPrecedenceOp, &la, &reply)
             if reply.Status = Ok && ((nInfixOps ||| nPostfixOps) <> 0) then // we could always consume more infix and postfix operators
                let error = if nInfixOps <> 0 && nPostfixOps <> 0 then expectedInfixOrPostfixOp
                            elif nInfixOps <> 0 then expectedInfixOp
                            else expectedPostfixOp
                reply.Error <- mergeErrors reply.Error error
             reply

    new (?ops: seq<PrecedenceParserOp<'a,'u>>) as t =
        OperatorPrecedenceParser<_,_>()
        then
            match ops with Some s -> t.AddOperators(s) | _ -> ()
            t.InitializeExpressionParser()

    static member private DefaultOperatorConflictHandler (state1: State<'u>) (op1: PrecedenceParserOp<'a,'u>) (state2: State<'u>) (op2: PrecedenceParserOp<'a,'u>) =
        let pos1, pos2 = state1.Pos, state2.Pos
        sprintf "The %s conflicts with the %s %s."
                (op2.ToString()) (op1.ToString())
                (if pos1.StreamName = pos2.StreamName then
                     if pos1.Line = pos2.Line then "on the same line at col. " + pos1.Column.ToString()
                     else sprintf "at (Ln: %i, Col: %i)" pos1.Line pos1.Column
                  else sprintf "at (%s, Ln: %i, Col: %i)" pos1.StreamName pos1.Line pos1.Column)

    static member private FindPosition(opTable: Operator<_,_>[][], str: string) =
        if str.Length > 0 then
            let c0 = str.[0]
            let i = int c0 &&& (oppArrayLength - 1)
            let array = opTable.[i]
            let mutable j = 0
            let mutable nameExists = false
            if isNotNull array then
                while j < array.Length && array.[j].String > str do j <- j + 1
                if j < array.Length && array.[j].String = str then nameExists <- true
            (nameExists, i, j)
        else (false, -1, -1)

    member t.AddOperator(operator: PrecedenceParserOp<'a,'u>) =
        let conflictsMessage (op: Operator<_,_>) (oldOp: Operator<_,_>) =
            "The definition of the " + (op.ToString()) + " conflicts with (or duplicates) the previous definition of the " + (oldOp.ToString()) + "."

        let findPositionToInsert (opTable: Operator<_,_>[][]) (str: string) (op: Operator<_,_>) =
            let (nameExists, i, j) as pos = OperatorPrecedenceParser<_,_>.FindPosition(opTable, str)
            if nameExists then
                let oldOp = opTable.[i].[j]
                if    str <> op.String // str could be op.Ternary2ndString
                   || oldOp.Fixity = op.Fixity // name already exists
                   || (oldOp.Fixity = Fixity.Infix   && op.Fixity = Fixity.Postfix)
                   || (oldOp.Fixity = Fixity.Postfix && op.Fixity = Fixity.Infix)
                then invalidArg "operator" (conflictsMessage op oldOp)
            pos

        let insert (opTable: Operator<_,_>[][]) (nameExists, i, j) (op: Operator<_,_>) overwrite =
            let array = opTable.[i]
            if nameExists then
                if overwrite then array.[j] <- op
            elif isNull array then opTable.[i] <- [|op|]
            else
                let newArray = Array.zeroCreate (array.Length + 1)
                if j > 0 then System.Array.Copy(array, 0, newArray, 0, j)
                newArray.[j] <- op
                if j < array.Length then System.Array.Copy(array, j, newArray, j + 1, array.Length - j)
                opTable.[i] <- newArray

        let op = new Operator<_,_>(operator)

        if op.Precedence <= 0 then
            invalidArg "operator" "The operator precedence must be greater 0."
        match op.Assoc with
        | Assoc.Left | Assoc.Right | Assoc.None -> ()
        | _ -> invalidArg "operator" "Operator has an invalid associativity value."
        if op.String.Length = 0 || (op.IsTernary && op.Ternary2ndString.Length = 0) then
            invalidArg "operator" ("The " + op.ToString() + " definition contains an empty string.")

        let mutable reservedOp = Unchecked.defaultof<_>
        if    reserved.TryGetValue(op.String, &reservedOp)
           || (op.IsTernary && reserved.TryGetValue(op.Ternary2ndString, &reservedOp))
        then
            invalidArg "operator" (conflictsMessage op reservedOp)

        let pos = findPositionToInsert (if op.Fixity = Fixity.Prefix then lhsOps else rhsOps) op.String op
        if op.IsTernary then
            if op.String = op.Ternary2ndString then
                invalidArg "operator" ("The " + op.ToString() + " definition must contain different strings for the first and second part of the operator.")
            // make sure the Ternary2ndString isn't registered as an operator
            findPositionToInsert lhsOps op.Ternary2ndString op |> ignore
            findPositionToInsert rhsOps op.Ternary2ndString op |> ignore
            reserved.Add(op.Ternary2ndString, op)

        insert lhsOps pos op (op.Fixity =  Fixity.Prefix)
        insert rhsOps pos op (op.Fixity <> Fixity.Prefix)
        match op.Fixity with
        | Fixity.Infix   -> nInfixOps   <- nInfixOps   + 1
        | Fixity.Prefix  -> nPrefixOps  <- nPrefixOps  + 1
        | _              -> nPostfixOps <- nPostfixOps + 1

    member t.AddOperators(operators: seq<PrecedenceParserOp<'a,'u>>) =
        for op in operators do t.AddOperator(op)

    member t.RemoveInfixOp(string: string)   = t.Remove(Fixity.Infix, string)
    member t.RemovePrefixOp(string: string)  = t.Remove(Fixity.Prefix, string)
    member t.RemovePostfixOp(string: string) = t.Remove(Fixity.Postfix, string)

    member t.RemoveTernaryOp(string1: string, string2: string) =
        let mutable reservedOp = Unchecked.defaultof<_>
        if not (reserved.TryGetValue(string2, &reservedOp)) || string1 <> reservedOp.String then false
        else
            reserved.Remove(string2) |> ignore
            t.Remove(Fixity.Infix, string1)

    member t.RemoveOperator(op: PrecedenceParserOp<'a, 'u>) =
        let fixity, str, opTable =
            match op with
            | PrefixOp  (str,_,_,_,_)     | PrefixOp'  (str,_,_,_,_)     -> Fixity.Prefix,  str, lhsOps
            | PostfixOp (str,_,_,_,_)     | PostfixOp' (str,_,_,_,_)     -> Fixity.Postfix, str, rhsOps
            | InfixOp   (str,_,_,_,_)     | InfixOp'   (str,_,_,_,_)     -> Fixity.Infix,   str, rhsOps
            | TernaryOp (str,_,_,_,_,_,_) | TernaryOp' (str,_,_,_,_,_,_) -> Fixity.Infix,   str, rhsOps
        let nameExists, i, j = OperatorPrecedenceParser<_,_>.FindPosition(opTable, str)
        if not nameExists then false
        else
            let top = opTable.[i].[j]
            if top.OriginalOp <> op then false
            elif top.IsTernary then t.RemoveTernaryOp(top.String, top.Ternary2ndString)
            else t.Remove(fixity, str)

    member private t.Remove(fixity: Fixity, str: string) =
        let opTable, altOpTable = match fixity with
                                  | Fixity.Prefix -> lhsOps, rhsOps
                                  | _             -> rhsOps, lhsOps
        let nameExists, i, j = OperatorPrecedenceParser<_,_>.FindPosition(opTable, str)
        if not nameExists || opTable.[i].[j].Fixity <> fixity then false
        else
            // remove from opTables
            let array    = opTable.[i]
            let altArray = altOpTable.[i]
            if altArray.[j].Fixity <> fixity then
                array.[j] <- altArray.[j]
            else
                let n = array.Length
                if n > 1 then
                    let newArray    = Array.zeroCreate (n - 1)
                    let newAltArray = Array.zeroCreate (n - 1)
                    if j > 0 then
                        System.Array.Copy(array,    0, newArray,    0, j)
                        System.Array.Copy(altArray, 0, newAltArray, 0, j)
                    if j + 1 < n then
                        System.Array.Copy(array,    j + 1, newArray,    j, n - j - 1)
                        System.Array.Copy(altArray, j + 1, newAltArray, j, n - j - 1)
                    opTable.[i]    <- newArray
                    altOpTable.[i] <- newAltArray
                else
                    opTable.[i]    <- null
                    altOpTable.[i] <- null
            match fixity with
            | Fixity.Infix   -> nInfixOps   <- nInfixOps   - 1
            | Fixity.Prefix  -> nPrefixOps  <- nPrefixOps  - 1
            | _              -> nPostfixOps <- nPostfixOps - 1
            true

    member t.Operators =
        // we take a snapshot of the defined operators at the time this member is accessed
        let ra = new ResizeArray<_>()
        for i = 0 to lhsOps.Length - 1 do
            let array1 = lhsOps.[i]
            if isNotNull array1 then
                let array2 = rhsOps.[i]
                for j = 0 to array1.Length - 1 do
                    ra.Add(array1.[j].OriginalOp)
                    if array1.[j].Fixity <> array2.[j].Fixity then
                        ra.Add(array2.[j].OriginalOp)
        ra :> seq<_>

    member private t.ParseOp(opTable: Operator<'a,'u>[][], reply: byref<Reply<'a,'u>>) =
        let mutable result = Unchecked.defaultof<_>
        let cs = reply.State.Iter.Read2()
        let c1 = cs.Char1
        let c0 = cs.Char0
        let array = opTable.[int c0 &&& (oppArrayLength - 1)]
        if isNotNull array then
            let mutable j = 0
            while j < array.Length do
                let op = array.[j]
                let s = op.String
                if s.[0] = c0 then
                    if s.Length <= 1 || (s.[1] = c1 && (s.Length = 2 || reply.State.Iter.Match(s))) then
                        let state1 = reply.State.Advance(s.Length)
                        let reply2 = op.WhitespaceAfterStringParser(state1)
                        if reply2.Status <> Error || reply2.State != state1 then
                            if reply2.Status = Ok then
                                result <- op
                            reply.Status <- reply2.Status
                            reply.State  <- reply2.State
                            reply.Error  <- reply2.Error
                        j <- System.Int32.MaxValue // break
                    else j <- j + 1
                elif s.[0] >= c0 then j <- j + 1
                else j <- System.Int32.MaxValue // break
        result

    member private t.ParseTernaryOp2ndString(op: Operator<'a,'u>, reply: byref<Reply<'a,'u>>) =
        let s = op.Ternary2ndString
        if reply.State.Iter.Match(s) then
            let state1 = reply.State.Advance(s.Length)
            let reply2 = op.WhitespaceAfter2ndStringParser(state1);
            if reply2.Status <> Error || reply2.State != state1 then
                reply.State  <- reply2.State
                reply.Error  <- reply2.Error
                reply.Status <- reply2.Status
            else t.ParseTernaryOp2ndStringError(op, &reply)
        else t.ParseTernaryOp2ndStringError(op, &reply)
    member private t.ParseTernaryOp2ndStringError(op: Operator<'a,'u>, reply: byref<Reply<'a,'u>>) =
       reply.Status <- Error
       // we could consume more infix or postfix operators
       let error1 = let error = if nPostfixOps > 0 then expectedInfixOrPostfixOp
                                else expectedInfixOp
                    mergeErrors reply.Error error
       // or the 2nd half of the ternary operator
       let error2 = expectedError (quoteString op.Ternary2ndString)
       reply.Error <- concatErrorMessages error1 error2

    member private t.ParseRhsOpIntoLA(la: byref<OpLookahead<'a,'u>>, reply: byref<Reply<'a,'u>>) =
        la.State <- reply.State
        la.Error <- reply.Error
        la.Op <- t.ParseOp(rhsOps, &reply) // sets Op to null (and thus makes la Invalid) if reply.Status <> Ok

    member private t.ParseExpression(prevState: State<'u>, prevOp: Operator<'a,'u>, la: byref<OpLookahead<'a,'u>>, reply: byref<Reply<'a,'u>>) =
        Debug.Assert(reply.Status = Ok)
        if nPrefixOps = 0 then
            let error = reply.Error
            let state = reply.State // state before beginning of op
            reply <- termParser state
            if reply.State == state then
                reply.Error <- mergeErrors error reply.Error
            if reply.Status = Ok then
                t.ParseRhsOpIntoLA(&la, &reply)
                if la.IsValid then
                    t.ParseInfixAndPostfixOps(prevState, prevOp, &la, &reply)
            else la.SetInvalid()
        else
            let error = reply.Error
            let state = reply.State // state before beginning of op
            let op = t.ParseOp(lhsOps, &reply)
            if isNull op then
                // no prefix operator
                if reply.Status = Ok then // op might be null because the whitespace parser changed the state and returned an error
                    reply <- termParser state
                    if reply.State == state then
                        reply.Error <- mergeErrors (if nPrefixOps = 0 then error
                                                    else mergeErrors error expectedPrefixOp) reply.Error
                    if reply.Status = Ok then
                        t.ParseRhsOpIntoLA(&la, &reply)
                        if la.IsValid then
                            t.ParseInfixAndPostfixOps(prevState, prevOp, &la, &reply)
                    else la.SetInvalid()
                else la.SetInvalid()
            elif op.Fixity = Fixity.Prefix then
                // a prefix operator
                let mutable msg = null
                if    prevOp.Precedence <> op.Precedence || prevOp.Fixity <> Fixity.Prefix
                   || prevOp.Assoc <> Assoc.None || op.Assoc <> Assoc.None
                   || (msg <- operatorConflictHandler prevState prevOp.OriginalOp state op.OriginalOp
                       isNullOrEmpty msg)
                then
                    let lookahead = t.ParseExpression(state, op, &la, &reply)
                    if reply.Status = Ok then
                        reply.Result <- if isNotNull op.Apply1 then op.Apply1(reply.Result)
                                        else op.Apply1'.Invoke(state, reply.Result)
                        if la.IsValid then
                            t.ParseInfixAndPostfixOps(prevState, prevOp, &la, &reply)
                else
                    t.ReplyConflictError(state, error, msg, &reply)
                    la.SetInvalid()
            else
                reply.Status <- Error
                reply.Error  <- mergeErrors error (unexpectedError (op.ToString()))
                reply.State  <- state
                la.SetInvalid()

    member private t.ParseInfixAndPostfixOps(prevState: State<'u>, prevOp: Operator<'a,'u>,
                                             la: byref<OpLookahead<'a,'u>>, reply: byref<Reply<'a,'u>>) =
        Debug.Assert(la.IsValid && reply.Status = Ok)
        let state = la.State
        let op = la.Op
        if op.Fixity = Fixity.Infix then
            if    prevOp.Precedence < op.Precedence
               || (   prevOp.Precedence = op.Precedence
                   && prevOp.Fixity = Fixity.Infix
                   && prevOp.Assoc = op.Assoc && op.Assoc = Assoc.Right)
            then
                let result1 = reply.Result
                if not op.IsTernary then
                    t.ParseExpression(state, op, &la, &reply)
                    if reply.Status = Ok then
                        reply.Result <- if isNotNull op.Apply2 then
                                            op.Apply2.Invoke(result1, reply.Result)
                                        else op.Apply2'.Invoke(state, result1, reply.Result)
                        if la.IsValid then
                            t.ParseInfixAndPostfixOps(prevState, prevOp, &la, &reply)
                else
                    t.ParseExpression(state, zeroPrecedenceOp, &la, &reply)
                    if reply.Status = Ok then
                        let state2 = reply.State
                        let result2 = reply.Result
                        t.ParseTernaryOp2ndString(op, &reply)
                        if reply.Status = Ok then
                            t.ParseExpression(state, op, &la, &reply)
                            if reply.Status = Ok then
                                reply.Result <- if isNotNull op.Apply3 then
                                                    op.Apply3.Invoke(result1, result2, reply.Result)
                                                else op.Apply3'.Invoke(state, state2, result1, result2, reply.Result)
                                if la.IsValid then
                                    t.ParseInfixAndPostfixOps(prevState, prevOp, &la, &reply)
                        else la.SetInvalid()
            elif    prevOp.Precedence = op.Precedence && prevOp.Fixity = Fixity.Infix
                 && (prevOp.Assoc <> op.Assoc || op.Assoc = Assoc.None)
            then
                let msg = operatorConflictHandler prevState prevOp.OriginalOp state op.OriginalOp
                if not (isNullOrEmpty msg) then t.ReplyConflictError(msg, &la, &reply)

        elif op.Fixity = Fixity.Postfix then
            if    prevOp.Precedence < op.Precedence
              || (   prevOp.Precedence = op.Precedence
                  && prevOp.Fixity = Fixity.Infix)
            then
                reply.Result <- if isNotNull op.Apply1 then op.Apply1(reply.Result)
                                else op.Apply1'.Invoke(state, reply.Result)
                t.ParseRhsOpIntoLA(&la, &reply)
                if la.IsValid then
                    if la.Op.Fixity <> Fixity.Postfix then
                        t.ParseInfixAndPostfixOps(prevState, prevOp, &la, &reply)
                    else
                        t.ParseMorePostfixOps(state, op, &la, &reply)
                        if la.IsValid then
                            t.ParseInfixAndPostfixOps(prevState, prevOp, &la, &reply)
            elif    prevOp.Precedence = op.Precedence  // prevOp.Fixity = Fixity.Prefix
                 && (prevOp.Assoc = Assoc.None && op.Assoc = Assoc.None)
            then
                let msg = operatorConflictHandler prevState prevOp.OriginalOp state op.OriginalOp
                if not (isNullOrEmpty msg) then t.ReplyConflictError(msg, &la, &reply)

        else // currOp.Fixity = Fixity.Prefix
            // this doesn't have to be a syntax error because the grammar might allow two expressions next to each other
            la.SetInvalid()

    member private t.ParseMorePostfixOps(prevState: State<'u>, prevOp: Operator<'a,'u>,
                                         la: byref<OpLookahead<'a,'u>>,  reply: byref<Reply<'a,'u>>) =
        Debug.Assert(la.IsValid && reply.Status = Ok)
        let state = la.State
        let op = la.Op
        if prevOp.Precedence < op.Precedence
           || (    prevOp.Precedence = op.Precedence
               && (   prevOp.Assoc <> Assoc.None || op.Assoc <> Assoc.None
                   || (let msg = operatorConflictHandler prevState prevOp.OriginalOp state op.OriginalOp
                       isNullOrEmpty msg || (t.ReplyConflictError(msg, &la, &reply); false))))
        then
            reply.Result <- if isNotNull op.Apply1 then op.Apply1(reply.Result)
                            else op.Apply1'.Invoke(state, reply.Result)
            t.ParseRhsOpIntoLA(&la, &reply)
            if la.IsValid && la.Op.Fixity = Fixity.Postfix then
                t.ParseMorePostfixOps(state, op, &la, &reply)

    member private t.ReplyConflictError(state: State<'u>, error: ErrorMessageList, msg, reply: byref<Reply<'a,'u>>) =
        reply <- Reply(Error, mergeErrors error (messageError msg), state)

    member private t.ReplyConflictError(msg, la: byref<OpLookahead<'a,'u>>, reply: byref<Reply<'a,'u>>) =
        t.ReplyConflictError(la.State, la.Error, msg, &reply)
        la.SetInvalid()
