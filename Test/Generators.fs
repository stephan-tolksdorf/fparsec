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

type FiniteFloat32 = FiniteFloat32 of float32 with
    static member op_Explicit(FiniteFloat32 f) = f
    
    static member generator =
        Arb.generate<float32>
        |> Gen.filter Single.IsFinite
        |> Gen.map FiniteFloat32
    
    static member valid (FiniteFloat32 f) =
        Single.IsFinite f
    
    static member shrink = shrink FiniteFloat32.valid

type PositiveFiniteFloat32 = PositiveFiniteFloat32 of float32 with
    static member op_Explicit(PositiveFiniteFloat32 f) = f
    
    static member generator =
        Arb.generate<float32>
        |> Gen.filter Single.IsFinite
        |> Gen.filter (not << Single.IsNegative)
        |> Gen.map PositiveFiniteFloat32
    
    static member valid (PositiveFiniteFloat32 f) =
        Single.IsFinite f && (not << Single.IsNegative) f
    
    static member shrink = shrink PositiveFiniteFloat32.valid

type NegativeFiniteFloat32 = NegativeFiniteFloat32 of float32 with
    static member op_Explicit(NegativeFiniteFloat32 f) = f
    
    static member generator =
        Arb.generate<float32>
        |> Gen.filter Single.IsFinite
        |> Gen.filter Single.IsNegative
        |> Gen.map NegativeFiniteFloat32
    
    static member valid (NegativeFiniteFloat32 f) =
        Single.IsFinite f && Single.IsNegative f
    
    static member shrink = shrink NegativeFiniteFloat32.valid

type FParsecGenerators =
    static member NonEmptyArray () =
        Arb.fromGenShrink (NonEmptyArray.generator, NonEmptyArray.shrink)
    
    static member FiniteFloat () =
        Arb.fromGenShrink (FiniteFloat.generator, FiniteFloat.shrink)
    
    static member PositiveFiniteFloat () =
        Arb.fromGenShrink (PositiveFiniteFloat.generator, PositiveFiniteFloat.shrink)
    
    static member NegativeFiniteFloat () =
        Arb.fromGenShrink (NegativeFiniteFloat.generator, NegativeFiniteFloat.shrink)
    
    static member FiniteFloat32 () =
        Arb.fromGenShrink (FiniteFloat32.generator, FiniteFloat32.shrink)
    
    static member PositiveFiniteFloat32 () =
        Arb.fromGenShrink (PositiveFiniteFloat32.generator, PositiveFiniteFloat32.shrink)
    
    static member NegativeFiniteFloat32 () =
        Arb.fromGenShrink (NegativeFiniteFloat32.generator, NegativeFiniteFloat32.shrink)

type FParsecPropertyAttribute () =
    inherit PropertyAttribute(Arbitrary = [| typeof<FParsecGenerators> |])