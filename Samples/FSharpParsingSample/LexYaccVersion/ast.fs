// Copyright (c) Microsoft Corporation 2005-2006.
// This sample code is provided "as is" without warranty of any kind.
// We disclaim all warranties, either express or implied, including the
// warranties of merchantability and fitness for a particular purpose.

module Ast

type expr =
    | Val of string
    | Int of int
    | Float of float
    | Decr of expr

type stmt =
    | Assign of string * expr
    | While of expr * stmt
    | Seq of stmt list
    | IfThen of expr * stmt
    | IfThenElse of expr * stmt * stmt
    | Print of expr

type prog = Prog of stmt list


