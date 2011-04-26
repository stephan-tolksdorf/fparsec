
// Copyright (c) Microsoft Corporation 2005-2006.
// This sample code is provided "as is" without warranty of any kind.
// We disclaim all warranties, either express or implied, including the
// warranties of merchantability and fitness for a particular purpose.
//

module Interp

open Ast

type v = INT of int | FLOAT of float

let printVal os v =
    match v with
    | INT n -> Printf.fprintf os "%d" n
    | FLOAT f -> Printf.fprintf os "%g" f

let rec prog (Prog l ) =
    stmts (Hashtbl.create 10) l

and stmts s l =
    List.iter (stmt s) l

and stmt s st =
    match st with
    | Assign (a,b) ->
        Hashtbl.replace s a (expr s b)
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

and expr s e =
    match e with
    | Val n ->
        if Hashtbl.mem s n then Hashtbl.find s n
        else
            Printf.eprintf "warning: location %s not defined\n" n;
            INT 0
    | Int n -> INT n
    | Float f -> FLOAT f
    | Decr e2 ->
        match expr s e2 with
        | INT n -> INT (n-1)
        | FLOAT f -> failwith "cannot decrement a float"


