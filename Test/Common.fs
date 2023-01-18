[<AutoOpen>]
module FParsec.Test.Common

open System
open System.Collections
open System.Text.RegularExpressions
open FsCheck


let (===) left right = left = right |@ $"%A{left} != %A{right}"

let (.*) input pattern = Regex.Match(input, pattern).Success |@ $"String: '{input}' doesn't match the pattern."

module Float =
    type private FloatParts =
        {
            Sign : int64
            Exponent : int64
            Fraction : int64
        }
    
    let private getFloatParts (f: float) =
        let ( ** ) v e =
            seq { for _ in 1 .. e do yield v }
            |> Seq.fold (fun acc v -> acc * v) 1L
        
        let bitsToLong (bits: int64 list) =
            let rec loop (place: int) (acc: int64) (bits: int64 list) =
                match bits with
                | head :: tail ->
                    let acc' = acc + ((2L ** place) * head)
                    loop (place + 1) acc' tail
                | [] ->
                    acc
            
            bits
            |> loop 0 0
            
        
        let bytes = BitConverter.GetBytes f
        let mutable bits : bool array = Array.zeroCreate 64
        BitArray(bytes).CopyTo(bits, 0)
        
        let bits' =
            bits
            |> List.ofArray
            |> List.map (fun b -> if b then 1L else 0L)
        
        let sign = bits'[63]
        let exponent = bits'[52..62]
        let fraction = bits'[0..51]
        
        {
            Sign = sign
            Exponent = exponent |> bitsToLong
            Fraction = fraction |> bitsToLong
        }
    
    let private hexValue = function
        | 0uy -> '0'
        | 1uy -> '1'
        | 2uy -> '2'
        | 3uy -> '3'
        | 4uy -> '4'
        | 5uy -> '5'
        | 6uy -> '6'
        | 7uy -> '7'
        | 8uy -> '8'
        | 9uy -> '9'
        | 10uy -> 'a'
        | 11uy -> 'b'
        | 12uy -> 'c'
        | 13uy -> 'd'
        | 14uy -> 'e'
        | 15uy -> 'f'
        | x -> failwith $"{x} is out of range for hex digit"
    
    let private fractionString floatParts =
        let splitByte (b: byte) =
            let top = b &&& 240uy >>> 4
            let bottom = b &&& 15uy
            [|bottom; top|]
        
        floatParts.Fraction                      // 3934024402416481L
        |> BitConverter.GetBytes                // [|97uy; 51uy; 11uy; 111uy; 249uy; 249uy; 13uy; 0uy|]
        |> Array.collect splitByte              // [|1uy; 6uy; 3uy; 3uy; 11uy; 0uy; 15uy; 6uy; 9uy; 15uy; 9uy; 15uy; 13uy; 0uy; 0uy; 0uy|]
        |> Array.take 13                        // [|1uy; 6uy; 3uy; 3uy; 11uy; 0uy; 15uy; 6uy; 9uy; 15uy; 9uy; 15uy; 13uy;|]
        |> Array.map hexValue                   // [|'1'; '6'; '3'; '3'; 'b'; '0'; 'f'; '6'; '9'; 'f'; '9'; 'f'; 'd'|]
        |> Array.skipWhile (fun c -> c = '0')   // [|'1'; '6'; '3'; '3'; 'b'; '0'; 'f'; '6'; '9'; 'f'; '9'; 'f'; 'd'|]
        |> Array.rev                            // [|'d'; 'f'; '9'; 'f'; '9'; '6'; 'f'; '0'; 'b'; '3'; '3'; '6'; '1'|]
        |> String                                    // "df9f96f0b3361"
    
    let private exponentString floatParts =
        let bias = 1023L
        
        floatParts.Exponent
        |> fun e -> e - bias
        |> Convert.ToString
    
    let private signString floatParts =
        if floatParts.Sign = 1L
        then "-"
        else ""
    
    let private (|Zero|Normal|SubNormal|Infinity|NaN|) floatParts =
        match floatParts.Exponent, floatParts.Fraction with
        | 0L, 0L -> Zero
        | 0L, _ -> SubNormal
        | 2047L, 0L -> Infinity
        | 2047L, _ -> NaN
        | _, _ -> Normal
    
    let private floatPartsToHexString floatParts =
        let sign =
            floatParts
            |> signString
        
        let fraction =
            floatParts
            |> fractionString

        let exponent =
            floatParts
            |> exponentString
        
        match floatParts with
        | Zero ->       $"{sign}0x0.0p0"
        | SubNormal ->  $"{sign}0x0.{fraction}p-1022"
        | Infinity ->   $"{sign}Infinity"
        | NaN ->        "NaN"
        | Normal ->     $"{sign}0x1.{fraction}p{exponent}"
    
    let toHexString (f: float) =
        getFloatParts f
        |> floatPartsToHexString