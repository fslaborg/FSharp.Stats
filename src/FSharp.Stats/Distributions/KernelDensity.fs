namespace FSharp.Stats.Distributions



/// Module to perform Kernel density estimation
module KernelDensity =

    open FSharp.Stats

    type DensityKernel = float -> float -> float

    module Kernel =

        /// Gausian kernel
        let gaussian bw x = Distributions.Continuous.Normal.PDF 0. bw x
        /// Rectangular kernel
        let rectangular bw x =
             let a = bw * sqrt 3.
             if abs x < a then ( 0.5 / a ) else 0. 
        /// Triangular kernel
        let triangular bw x =
            let a = bw * sqrt 6.
            let ax = abs x
            if ax < a then ( 1. - ax / a ) else 0. 
        /// Epanechnikov kernel
        let epanechnikov bw x =
            let a = bw * sqrt 6. 
            let ax = abs x
            if ax < a then ( 1. - ax / a ) else 0.
        /// Biweight kernel
        let biweight bw x =
            let a = bw * sqrt 7.
            let ax = abs x 
            if ax < a then ( 15. / 16. * (1. - (ax / a)**2.)**2. / a ) else 0. 
        /// Cosine kernel
        let cosine bw x =
        /// Optcosine kernel
            let a = bw / sqrt (1./3. - 2./ System.Math.PI **2.)
            if abs(x)  < a then ( (1. + cos(System.Math.PI*x/a)) / 2. * a ) else 0.
        let optcosine bw x =
            let a = bw / sqrt (1. - 8. / System.Math.PI **2.)
            if x  < a then ( System.Math.PI / 4. * cos(System.Math.PI*x/(2. * a)) / a ) else 0.



    /// Applying the given function to each of the elements of the array and returns the value in place.
    let private mapInPlace f (arr:array<'T>) =
        for i=0 to Array.length arr-1 do
            arr.[i] <- f arr.[i]
        arr




    let private massdist (_x:float[]) (_xmass:float[]) (_nx:int) (_xlow:float) (_xhigh:float) (_y:int) (_ny:int) =

    //    double fx, xdelta, xmi, xpos;   /* AB */
    //    int i, ix, ixmax, ixmin;

        let ixmin = 0;
        let ixmax = _ny - 2
        //* AB: line deleted */
        let xdelta = (_xhigh - _xlow) / (float(_ny - 1))

        let y = Array.zeroCreate _y

        for i=0 to (_nx-1) do
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



    let estimateWith _cut _from _to n weights (kernel:DensityKernel) bandwidth data =

        let x = data |> Seq.toArray //|> Seq.Double.filterNaN |> Seq.Double.filterInfinity |> Seq.toArray    
        //let N = Seq.length(data)
        let nx = x.Length 

        let lo = _from - 4. * bandwidth
        let up = _to + 4. * bandwidth

        // normalize weights
        let weights' =
            let wsum = weights |> Array.sum
            weights |> Array.map (fun w -> w / wsum)

        let totalMass = weights' |> Array.sum
        let y  =
            massdist x weights' nx lo up (2*n) n 
            |> Array.map ( fun yValue -> yValue * totalMass)



        let kords =               
            let fftKords =  
                let a = Array.seqInit 0. (2.*(up - lo)) (2 * n)
                (a.[(n + 1)..(2 * n)-1] <- (a.[1..n-1] |> Array.rev |> Array.map (fun x -> x * -1.0)  ) )
                a 
                |> Array.map ((kernel bandwidth) >> Complex.ofReal)
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
    



    let estimate kernel bandwidth data =
        let _from,_to = Array.range data |> Intervals.values
        let weights = Array.create data.Length 1.
        estimateWith 3 _from _to 512 weights kernel bandwidth data


