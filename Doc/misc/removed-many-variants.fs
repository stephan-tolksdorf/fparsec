open FParsec

// the following optimized variants of the many combinator were removed in the update to 0.9 of FParsec

let manyRev            p = Inline.Many((fun x -> [x]), (fun xs x -> x::xs), (fun xs -> xs), p, resultForEmptySequence = fun () -> [])
let manyFold    acc0 f p = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                           Inline.Many((fun x -> optF.Invoke(acc0, x)), (fun acc x -> optF.Invoke(acc, x)), (fun acc -> acc), p, resultForEmptySequence = fun () -> acc0)
let manyReduce  f altX p = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                           Inline.Many((fun x0 -> x0), (fun x0 x -> optF.Invoke(x0, x)), (fun x0 -> x0), p, resultForEmptySequence = fun () -> altX)

let many1Rev           p = Inline.Many((fun x -> [x]), (fun xs x -> x::xs), (fun xs -> xs), p)
let many1Fold   acc0 f p = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                           Inline.Many((fun x -> optF.Invoke(acc0, x)), (fun acc x -> optF.Invoke(acc, x)), (fun x -> x), p)
let many1Reduce f      p = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                           Inline.Many((fun x0 -> x0), (fun x0 x -> optF.Invoke(x0, x)), (fun x0 -> x0), p)

let sepByRev           p sep = Inline.SepBy((fun x -> [x]), (fun xs _ x -> x::xs), (fun xs -> xs), p, sep, resultForEmptySequence = fun () -> [])
let sepByFold   acc0 f p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                               Inline.SepBy((fun x -> optF.Invoke(acc0, x)), (fun acc _ x -> optF.Invoke(acc, x)), (fun acc -> acc), p, sep, resultForEmptySequence = fun () -> acc0)
let sepByReduce f altX p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                               Inline.SepBy((fun x0 -> x0), (fun x0 _ x -> optF.Invoke(x0, x)), (fun x0 -> x0), p, sep, resultForEmptySequence = fun () -> altX)

let sepBy1Rev           p sep = Inline.SepBy((fun x -> [x]), (fun xs _ x -> x::xs), (fun xs -> xs), p, sep)
let sepBy1Fold   acc0 f p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                Inline.SepBy((fun x -> optF.Invoke(acc0, x)), (fun acc _ x -> optF.Invoke(acc, x)), (fun acc -> acc), p, sep)
let sepBy1Reduce f      p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                Inline.SepBy((fun x0 -> x0), (fun x0 _ x -> optF.Invoke(x0, x)), (fun x0 -> x0), p, sep)

let sepEndByRev           p sep = Inline.SepBy((fun x -> [x]), (fun xs _ x -> x::xs), (fun xs -> xs), p, sep, separatorMayEndSequence = true, resultForEmptySequence = fun () -> [])
let sepEndByFold   acc0 f p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                  Inline.SepBy((fun x -> optF.Invoke(acc0, x)), (fun acc _ x -> optF.Invoke(acc, x)), (fun acc -> acc), p, sep, separatorMayEndSequence = true, resultForEmptySequence = fun () -> acc0)
let sepEndByReduce f altX p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                  Inline.SepBy((fun x0 -> x0), (fun x0 _ x -> optF.Invoke(x0, x)), (fun x0 -> x0), p, sep, separatorMayEndSequence = true, resultForEmptySequence = fun () -> altX)

let sepEndBy1Rev           p sep = Inline.SepBy((fun x -> [x]), (fun xs _ x -> x::xs), (fun xs -> xs), p, sep, separatorMayEndSequence = true)
let sepEndBy1Fold   acc0 f p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                   Inline.SepBy((fun x -> optF.Invoke(acc0, x)), (fun acc _ x -> optF.Invoke(acc, x)), (fun acc -> acc), p, sep, separatorMayEndSequence = true)
let sepEndBy1Reduce f      p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                   Inline.SepBy((fun x0 -> x0), (fun x0 _ x -> optF.Invoke(x0, x)), (fun x0 -> x0), p, sep, separatorMayEndSequence = true)

let manyTillRev           p endp = Inline.ManyTill((fun x -> [x]), (fun xs x -> x::xs), (fun xs _ -> xs), p, endp, resultForEmptySequence = fun _ -> [])
let manyTillFold   acc0 f p endp = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                   Inline.ManyTill((fun x -> optF.Invoke(acc0, x)), (fun acc x -> optF.Invoke(acc, x)), (fun acc _ -> acc), p, endp, resultForEmptySequence = fun _ -> acc0)
let manyTillReduce f altX p endp = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                   Inline.ManyTill((fun x0 -> x0), (fun x0 x -> optF.Invoke(x0, x)), (fun x0 _ -> x0), p, endp, resultForEmptySequence = fun _ -> altX)
