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
        let exponent = bits'[52..62] |> bitsToLong
        let fraction = bits'[0..51] |> bitsToLong
        
        {
            Sign = sign
            Exponent = exponent
            Fraction = fraction
        }
    
    let private toHexValue = function
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
    
    let private fromHexValue c =
        match Char.ToLower c with
        | '0' -> 0uy
        | '1' -> 1uy
        | '2' -> 2uy
        | '3' -> 3uy
        | '4' -> 4uy
        | '5' -> 5uy
        | '6' -> 6uy
        | '7' -> 7uy
        | '8' -> 8uy
        | '9' -> 9uy
        | 'a' -> 10uy
        | 'b' -> 11uy
        | 'c' -> 12uy
        | 'd' -> 13uy
        | 'e' -> 14uy
        | 'f' -> 15uy
        | x -> failwith $"{x} is not a valid hex digit"
    
    let private fractionString floatParts =
        let splitByte (b: byte) =
            let top = b &&& 240uy >>> 4
            let bottom = b &&& 15uy
            [|bottom; top|]
        
        floatParts.Fraction                      // 3934024402416481L
        |> BitConverter.GetBytes                // [|97uy; 51uy; 11uy; 111uy; 249uy; 249uy; 13uy; 0uy|]
        |> Array.collect splitByte              // [|1uy; 6uy; 3uy; 3uy; 11uy; 0uy; 15uy; 6uy; 9uy; 15uy; 9uy; 15uy; 13uy; 0uy; 0uy; 0uy|]
        |> Array.take 13                        // [|1uy; 6uy; 3uy; 3uy; 11uy; 0uy; 15uy; 6uy; 9uy; 15uy; 9uy; 15uy; 13uy;|]
        |> Array.map toHexValue                   // [|'1'; '6'; '3'; '3'; 'b'; '0'; 'f'; '6'; '9'; 'f'; '9'; 'f'; 'd'|]
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

module Float32 =
    type private FloatParts =
        {
            Sign : int
            Exponent : int
            Fraction : int
        }
    
    let private getFloatParts (f: float32) =
        let ( ** ) v e =
            seq { for _ in 1 .. e do yield v }
            |> Seq.fold (fun acc v -> acc * v) 1
        
        let bitsToInt (bits: int list) =
            let rec loop (place: int) (acc: int) (bits: int list) =
                match bits with
                | head :: tail ->
                    let acc' = acc + ((2 ** place) * head)
                    loop (place + 1) acc' tail
                | [] ->
                    acc
            
            bits
            |> loop 0 0
            
        
        let bytes = BitConverter.GetBytes f
        let mutable bits : bool array = Array.zeroCreate 32
        BitArray(bytes).CopyTo(bits, 0)
        
        let bits' =
            bits
            |> List.ofArray
            |> List.map (fun b -> if b then 1 else 0)
        
        let sign = bits'[31]
        let exponent = bits'[23..30] |> bitsToInt
        let fraction = bits'[0..22] |> bitsToInt
        
        {
            Sign = sign
            Exponent = exponent
            Fraction = fraction
        }
    
    let private toHexValue = function
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
    
    let private fromHexValue c =
        match Char.ToLower c with
        | '0' -> 0uy
        | '1' -> 1uy
        | '2' -> 2uy
        | '3' -> 3uy
        | '4' -> 4uy
        | '5' -> 5uy
        | '6' -> 6uy
        | '7' -> 7uy
        | '8' -> 8uy
        | '9' -> 9uy
        | 'a' -> 10uy
        | 'b' -> 11uy
        | 'c' -> 12uy
        | 'd' -> 13uy
        | 'e' -> 14uy
        | 'f' -> 15uy
        | x -> failwith $"{x} is not a valid hex digit"
    
    let private fractionString floatParts =
        let splitByte (b: byte) =
            let top = b &&& 240uy >>> 4
            let bottom = b &&& 15uy
            [|bottom; top|]
        
        floatParts.Fraction                       // 4788187
        |> fun f -> f <<< 1                       // 9576374 I'm still not sure why this is required
        |> BitConverter.GetBytes                // [|182uy; 31uy; 146uy; 0uy|]
        |> Array.collect splitByte              // [|6uy; 11uy; 15uy; 1uy; 2uy; 9uy; 0uy; 0uy|]
        |> Array.take 6                         // [|6uy; 11uy; 15uy; 1uy; 2uy; 9uy|]
        |> Array.map toHexValue                 // [|'6'; 'b'; 'f'; '1'; '2'; '9'|]
        |> Array.skipWhile (fun c -> c = '0')   // [|'6'; 'b'; 'f'; '1'; '2'; '9'|]
        |> Array.rev                            // [|'9'; '2'; '1'; 'f'; 'b'; '6'|]
        |> String                                    // "921fb6"
    
    let private exponentString floatParts =
        let bias = 127
        
        floatParts.Exponent
        |> fun e -> e - bias
        |> Convert.ToString
    
    let private signString floatParts =
        if floatParts.Sign = 1
        then "-"
        else ""
    
    let private (|Zero|Normal|SubNormal|Infinity|NaN|) floatParts =
        match floatParts.Exponent, floatParts.Fraction with
        | 0, 0 -> Zero
        | 0, _ -> SubNormal
        | 255, 0 -> Infinity
        | 255, _ -> NaN
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
        | SubNormal ->  $"{sign}0x0.{fraction}p-126"
        | Infinity ->   $"{sign}Infinity"
        | NaN ->        "NaN"
        | Normal ->     $"{sign}0x1.{fraction}p{exponent}"
    
    let toHexString (f: float32) =
        getFloatParts f
        |> floatPartsToHexString