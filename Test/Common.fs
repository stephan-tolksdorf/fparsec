[<AutoOpen>]
module FParsec.Test.Common

open FsCheck

let (===) left right = left = right |@ $"%A{left} != %A{right}"