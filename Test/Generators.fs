[<AutoOpen>]
module FParsec.Test.Generators

open FsCheck
open FsCheck.Xunit

let shrink validator t =
        Arb.Default.Derive().Shrinker t
        |> Seq.filter validator

type NonEmptyArray<'t> = NonEmptyArray of 't array with
    static member op_Explicit(NonEmptyArray arr) = arr
    
    static member generator =
        Arb.generate<'t array>
        |> Gen.filter (fun g -> g.Length > 0)
        |> Gen.map NonEmptyArray
    
    static member valid (NonEmptyArray arr) =
        arr.Length > 0
    
    static member shrink = shrink NonEmptyArray.valid
    
type FParsecGenerators =
    static member NonEmptyArray () =
        Arb.fromGenShrink (NonEmptyArray.generator, NonEmptyArray.shrink)

type FParsecPropertyAttribute () =
    inherit PropertyAttribute(Arbitrary = [| typeof<FParsecGenerators> |])