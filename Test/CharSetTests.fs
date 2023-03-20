// Copyright (c) Stephan Tolksdorf 2008-2010
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.CharSetTests

open System
open Xunit
open FsCheck

[<FParsecProperty>]
let ``Calling Contains returns true when the CharSet contains the character`` (NonEmptyString charSet) =
    let cs = FParsec.CharSet(charSet)
    
    charSet |> Seq.forall cs.Contains

[<FParsecProperty>]
let ``Calling Contains returns false when the CharSet does not contain the character`` (NonEmptyString charSet) =
    let filteredChar = charSet[0]
    let filteredCharSet =
        charSet
        |> String.filter (fun c -> c <> filteredChar)
    
    let cs = FParsec.CharSet(filteredCharSet)
    
    not <| cs.Contains filteredChar

[<Fact>]
let ``Creating a CharSet with a null string throws a NullReferenceException`` () =
    Assert.Throws<NullReferenceException>(fun () -> FParsec.CharSet(null) |> ignore )

[<FParsecProperty>]
let ``Calling Contains returns false for any character when the CharSet is empty`` (c: char) =
    not <| FParsec.CharSet("").Contains(c)