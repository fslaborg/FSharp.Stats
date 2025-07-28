namespace FSharp.Stats.Algebra


open System
open FSharp.Stats
open FSharp.Stats.Acceleration

[<Struct>]
type Householder<'T when 'T :> Numerics.INumber<'T>> =
    {
        V: Vector<'T>
        Tau: 'T
        Beta: 'T
    }


type Householder() =

    static member inline create<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)                              
                and 'T : struct
                and 'T : comparison
                and 'T :> ValueType
                and 'T :> Numerics.IRootFunctions<'T>>
                (x: Vector<'T>) :  Householder<'T> =
        
        let xSpan = x.AsSpan()
        let alpha = xSpan[0]
        let tail = xSpan.Slice(1)

        let zero = GenericMath.zero<'T>
        let one = GenericMath.one<'T>

        let sigma =
            SIMDUtils.mapFoldUnchecked(
                (fun v -> v * v),
                (fun x -> x * x),
                (+),
                (+),
                zero,
                tail
            )

        if sigma.Equals(zero) then
            let v = Vector.zeroCreate x.Length
            v.[0] <- one
            {
                V = v
                Tau = zero
                Beta = alpha
            }
        else
            let sum = alpha * alpha + sigma
            let beta = GenericMath.sqrt sum

            let v0 =
                if alpha <= zero then alpha - beta
                else -sigma / (alpha + beta)

            let tau =
                let v0Sq = v0 * v0
                (v0Sq + v0Sq) / (sigma + v0Sq)

            let v = Vector.divideScalar v0 x // SIMD-aware scalar division
            v.[0] <- one

            {
                V = v
                Tau = tau
                Beta = beta
            }

    static member inline applyLeft<'T when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T : equality
        and 'T :> ValueType>
        (h: Householder<'T>, A: Matrix<'T>, rowOffset: int) =

        let v = h.V
        let tau = h.Tau
        let m = A.NumRows
        let n = A.NumCols
        let vLen = v.Length

        //for j = 0 to n - 1 do
        //    let mutable dot = GenericMath.zero<'T>
        //    for i = 0 to vLen - 1 do
        //        dot <- dot + v.[i] * A.[rowOffset + i, j]
        //    let scale = tau * dot
        //    for i = 0 to vLen - 1 do
        //        A.[rowOffset + i, j] <- A.[rowOffset + i, j] - scale * v.[i]

        for j = 0 to n - 1 do
            let mutable dot = GenericMath.zero<'T>
            for i = 0 to vLen - 1 do
                let row = rowOffset + i
                if row < m then
                    dot <- dot + v.[i] * A.[row, j]
            let scale = tau * dot
            for i = 0 to vLen - 1 do
                let row = rowOffset + i
                if row < m then
                    A.[row, j] <- A.[row, j] - scale * v.[i]


    static member inline applyRight<'T when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T : equality
        and 'T :> ValueType>
        (h: Householder<'T>, A: Matrix<'T>, colOffset: int) =

        let v = h.V
        let tau = h.Tau
        let m = A.NumRows
        let vLen = v.Length

        for i = 0 to m - 1 do
            let mutable dot = GenericMath.zero<'T>
            for j = 0 to vLen - 1 do
                dot <- dot + A.[i, colOffset + j] * v.[j]
            let scale = tau * dot
            for j = 0 to vLen - 1 do
                A.[i, colOffset + j] <- A.[i, colOffset + j] - scale * v.[j]

