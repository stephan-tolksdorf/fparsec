// Copyright (c) Stephan Tolksdorf 2007-2008
// License: Simplified BSD License. See accompanying documentation.

module Ast

type Grammar = Definition list

and  Definition = Def of string * Expression

and  Range =
     | Char  of char
     | Range of char * char

and  Expression =
     /// expression1 / expression2 / ...
     | Alt   of Expression list
     /// expression1 expression2 ...
     | Seq   of Expression list
     /// expression?
     | Opt   of Expression
     /// expression*
     | Star  of Expression
     /// expression+
     | Plus  of Expression
     /// &expression
     | And   of Expression
     /// !expression
     | Not   of Expression
     | Class of Range list
     | Literal of string
     | Identifier of string
     | Dot