
// Original code:
//     Copyright (c) Microsoft Corporation 2005-2006.
//     This sample code is provided "as is" without warranty of any kind.
//     We disclaim all warranties, either express or implied, including the
//     warranties of merchantability and fitness for a particular purpose.

// Modifications:
//    Copyright (c) Stephan Tolksdorf 2015.
//    License: Simplified BSD License. See accompanying documentation.

module Interp

open Ast

open System.Collections.Generic

type Value = INT of int | FLOAT of float

type State = Dictionary<string, Value>

let printVal os v =
    match v with
    | INT n -> Printf.fprintf os "%d" n
    | FLOAT f -> Printf.fprintf os "%g" f

let rec prog (Prog l ) =
    stmts (new Dictionary<_,_>()) l

and stmts s l =
    List.iter (stmt s) l

and stmt (s: State) st =
    match st with
    | Assign (a,b) ->
        s.[a] <- expr s b
    | While (a,b) ->
        while expr s a <> INT 0 do
            stmt s b
    | Seq l ->
        stmts s l
    | IfThen (g,t) ->
        if (expr s g <> INT 0) then stmt s t
    | IfThenElse (g,t,e) ->
        if (expr s g <> INT 0) then stmt s t
        else stmt s e
    | Print (e) ->
        Printf.printf "--> %a\n" printVal (expr s e)
        stdout.Flush()

and expr (s: State) e =
    match e with
    | Val n ->
        match s.TryGetValue(n) with
        | true, v  -> v
        | false, _ -> Printf.eprintf "warning: location %s not defined\n" n;
                      INT 0
    | Expr.Int n -> INT n
    | Expr.Float f -> FLOAT f
    | Decr e2 ->
        match expr s e2 with
        | INT n -> INT (n-1)
        | FLOAT f -> failwith "cannot decrement a float"


