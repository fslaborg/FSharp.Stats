namespace FSharp.Stats.Distributions



/// Density
module Density =

    open FSharp.Stats


    /// Applying the given function to each of the elements of the array and returns the value in place.
    let private mapInPlace f (arr:array<'T>) =
        for i=0 to Array.length arr-1 do
            arr.[i] <- f arr.[i]
        arr



//    /// Generates array sequence (like R! seq.int)
//    let seqInit (from:float) (tto:float) (length:int) =
//        let stepWidth = (tto - from) / (float length - 1.)
//        Array.init length ( fun x -> (float x * stepWidth) + from)  



    let private massdist (_x:float[]) (_xmass:float[]) (_nx:int) (_xlow:float) (_xhigh:float) (_y:int) (_ny:int) =

    //    double fx, xdelta, xmi, xpos;   /* AB */
    //    int i, ix, ixmax, ixmin;

        let ixmin = 0;
        let ixmax = _ny - 2
        //* AB: line deleted */
        let xdelta = (_xhigh - _xlow) / (float(_ny - 1))

        let y = Array.init _y (fun yv -> 0.0)

        for i in [0.._nx-1] do
            let xpos = (_x.[i] - _xlow) / xdelta
            let ix = int(floor(xpos))
            let fx = xpos - float(ix)
            let xmi = _xmass.[i];   //* AB: new line  */

            if (ixmin <= ix && ix <= ixmax) then
                y.[ix] <- y.[ix] + (1. - fx) * xmi    //* AB */
                y.[ix + 1] <- y.[ix + 1] + fx * xmi      //* AB */    
            elif (ix = -1) then
                y.[0]  <- y.[0] + fx * xmi    
            elif (ix = ixmax + 1) then
                y.[ix] <- y.[ix] + (1. - fx) * xmi

        y




        // let _cut    = defaultArg cut 3
        // let _from   = defaultArg from (Array.min(x) - float(_cut) * _bandwidth)
        // let _to     = defaultArg tto  (Array.max(x) + float(_cut) * _bandwidth)

    //let approx (x:seq<float>) (y:seq<float>) (v:seq<float>) (ties:seq<float> -> float) = v


    let densityEstimation' _cut _from _to n weights bandwidth kernel data =

        let x = data |> Seq.toArray //|> Seq.Double.filterNaN |> Seq.Double.filterInfinity |> Seq.toArray    
        let N = Seq.length(data)
        let nx = x.Length 

        let lo = _from - 4. * bandwidth
        let up = _to + 4. * bandwidth
        let totalMass = weights |> Array.sum
        let y  =
            massdist x weights nx lo up (2*n) n 
            |> Array.map ( fun yValue -> yValue * totalMass)



        let kords =               
            let fftKords =  
                let a = Array.seqInit 0. (2.*(up - lo)) (2 * n)
                (a.[(n + 1)..(2 * n)-1] <- (a.[1..n-1] |> Array.rev |> Array.map (fun x -> x * -1.0)  ) )
                a 
                |> Array.map (kernel >> Complex.ofReal)
                |> Signal.FFT.forwardInPlace

                
            let fftY     = 
                y
                |> Array.map Complex.ofReal
                |> Signal.FFT.forwardInPlace

            Array.map2 (fun y k -> y * Complex.conjugate k) fftY fftKords
            |> Signal.FFT.inverseInPlace
            |> Seq.truncate (n)    
            |> Seq.map (fun c -> let tmp = c.RealPart / float y.Length
                                 if tmp > 0. then tmp else 0.)



        let xords = Array.seqInit lo up n
        let nx    = Array.seqInit _from _to n
        let kdeY  = Interpolation.Approximation.approx xords kords nx Seq.average |> Seq.toArray
    
        Array.zip nx kdeY
    



    let densityEstimation bandwidth kernel data =
        let _from,_to = Array.range data |> Intervals.values
        let weights = [||]
        densityEstimation' 3 _from _to 512 weights bandwidth kernel data


