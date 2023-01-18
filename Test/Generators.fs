[<AutoOpen>]
module FParsec.Test.Generators

open System
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

type FiniteFloat = FiniteFloat of float with
    static member op_Explicit(FiniteFloat f) = f
    
    static member generator =
        Arb.generate<float>
        |> Gen.filter Double.IsFinite
        |> Gen.map FiniteFloat
    
    static member valid (FiniteFloat f) =
        Double.IsFinite f
    
    static member shrink = shrink FiniteFloat.valid

type PositiveFiniteFloat = PositiveFiniteFloat of float with
    static member op_Explicit(PositiveFiniteFloat f) = f
    
    static member generator =
        Arb.generate<float>
        |> Gen.filter Double.IsFinite
        |> Gen.filter (not << Double.IsNegative)
        |> Gen.map PositiveFiniteFloat
    
    static member valid (PositiveFiniteFloat f) =
        Double.IsFinite f && (not << Double.IsNegative) f
    
    static member shrink = shrink PositiveFiniteFloat.valid

type NegativeFiniteFloat = NegativeFiniteFloat of float with
    static member op_Explicit(NegativeFiniteFloat f) = f
    
    static member generator =
        Arb.generate<float>
        |> Gen.filter Double.IsFinite
        |> Gen.filter Double.IsNegative
        |> Gen.map NegativeFiniteFloat
    
    static member valid (NegativeFiniteFloat f) =
        Double.IsFinite f && Double.IsNegative f
    
    static member shrink = shrink NegativeFiniteFloat.valid

type FParsecGenerators =
    static member NonEmptyArray () =
        Arb.fromGenShrink (NonEmptyArray.generator, NonEmptyArray.shrink)
    
    static member FiniteFloat () =
        Arb.fromGenShrink (FiniteFloat.generator, FiniteFloat.shrink)
    
    static member PositiveFiniteFloat () =
        Arb.fromGenShrink (PositiveFiniteFloat.generator, PositiveFiniteFloat.shrink)
    
    static member NegativeFiniteFloat () =
        Arb.fromGenShrink (NegativeFiniteFloat.generator, NegativeFiniteFloat.shrink)

type FParsecPropertyAttribute () =
    inherit PropertyAttribute(Arbitrary = [| typeof<FParsecGenerators> |])