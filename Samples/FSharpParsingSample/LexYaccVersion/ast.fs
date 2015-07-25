// Copyright (c) Microsoft Corporation 2005-2006.
// This sample code is provided "as is" without warranty of any kind.
// We disclaim all warranties, either express or implied, including the
// warranties of merchantability and fitness for a particular purpose.

module Ast

type Expr =
    | Val of string
    | Int of int
    | Float of float
    | Decr of Expr

type Stmt =
    | Assign of string * Expr
    | While of Expr * Stmt
    | Seq of Stmt list
    | IfThen of Expr * Stmt
    | IfThenElse of Expr * Stmt * Stmt
    | Print of Expr

type Prog = Prog of Stmt list


