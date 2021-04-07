module TestExtensions

open Expecto

/// Expects the `actual` sequence to equal the `expected` one after rounding the floats in both to the given digit count.
let sequenceEqualRounded (digits : int) actual expected message =
    let round (v:float) = System.Math.Round(v,digits)
    Expect.sequenceEqual (actual |> Seq.map round) (expected |> Seq.map round) message

