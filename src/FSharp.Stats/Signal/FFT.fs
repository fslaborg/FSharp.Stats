namespace FSharp.Stats.Signal

open FSharp.Stats


/// FFT analysis converts a signal from its original domain (often time or space) to a representation in the frequency domain and vice versa.
module FFT =
    
    let private swapInPlace left right (arr:array<'T>) =
        let tmp = arr.[left]
        arr.[left]  <- arr.[right]
        arr.[right] <- tmp
        arr


    /// Applying the given function to each of the elements of the array and returns the value in place.
    let private mapInPlace f (arr:array<'T>) =
        for i=0 to Array.length arr-1 do
            arr.[i] <- f arr.[i]
        arr


    /// Reorder the elements of the input array in lexicographic order by the bits of their indices
    let private bitrev a =
        let n = Array.length a
        let mutable j = 0
        for i=0 to n-2 do 
            if i<j then swapInPlace i j a |> ignore
            let rec aux m j = 

                let m = m/2 
                let j = j ^^^ m 
                if j &&& m = 0 then aux m j else j
            j <- aux n j 


    // FFT Helper function
    let private fftAux (a : Complex array) n j sign m = 
        let w = 
            let t = pi * float (sign * m) / float j 
            Complex.Create (cos t, sin t)
        let mutable i = m 
        while i < n do 
            let ai = a. [i] 
            let t = w * a. [i + j]
            a. [i] <- ai + t 
            a. [i + j] <- ai - t 
            i <- i + 2*j


    let private fftPow2 sign a =
        let n = Array.length a
        bitrev a
        let mutable j = 1
        while j < n do
            for m = 0 to j - 1 do
                fftAux a n j sign m
            j <- 2 * j




    let rec private nextPow2 = function
        | 0 -> 1 
        | n -> 2 * nextPow2 (n / 2)


    let private pad n nb (w : complex array) = function
        | i when i < n -> w. [i] 
        | i when i > nb - n -> w. [nb-i]
        | _ -> Complex.zero 


    /// Bluestein’s convolution algorith
    let private bluestein a = 
    
        let bluesteinSequence n = 
            let s = pi / float n 
            Array.init n ( fun k -> 
                let t = s * float(k * k) 
                Complex.Create (cos t, sin t)
                )

        let n = Array.length a 
        let nb = nextPow2 (2*n - 1)  // Possible correct here
        let w = bluesteinSequence n 
        let y = pad n nb w |> Array.init nb 
        fftPow2 (-1) y
        let b = Array.create nb Complex.zero 
        for i=0 to n-1 do
            b. [i] <- Complex.conjugate w. [i] * a. [i] 
        fftPow2 (-1) b
        let b = Array.map2 ( * ) b y
        fftPow2 1 b
        let nbinv = Complex.Create ((1.0 / float nb),0.0)
        for i=0 to n-1 do 
            a. [i] <- nbinv * Complex.conjugate w. [i] * b. [i]




    let private fftInPlace sign a = 

        let isPow2 n =
            n &&& n-1 = 0

        let swapriInPlace a = 
            mapInPlace (fun z -> Complex.Create (z.i, z.r)) a
                

        let n = Array.length a 
    
        if isPow2 n then 
            fftPow2 sign a 
        else 
            if sign=1 then 
                swapriInPlace a |> ignore    
            bluestein a
            if sign=1 then swapriInPlace a |> ignore


    let forwardInPlace a = 
        fftInPlace 1 a
        a

    let inverseInPlace a = 
        fftInPlace (-1) a
        a


    /// No scaling in forward direction.
    let asymmetricScalingInPlace (a:array<float>) =
        a


    /// Universal; Symmetric scaling and common exponent (used in Maple) Default InverseExponent
    let symmetricScalingInPlace (a:array<float>) =
        let scalingFactor = 1.0 / float a.Length |> sqrt
        for i=0 to (a.Length-1) do
            a.[i] <- a.[i] * scalingFactor 
        a




    /// Only scale by 1/N in the inverse direction; No scaling in forward direction.
    let inverseAsymmetricScalingInPlace (a:array<float>) =
        let scalingFactor = 1.0 / float a.Length |> sqrt
        for i=0 to (a.Length-1) do
            a.[i] <- a.[i] * scalingFactor 
        a


    /// Universal; Symmetric scaling and common exponent (used in Maple) Default
    let inverseSymmetricScalingInPlace (a:array<float>) =
        let scalingFactor = 1.0 / float a.Length
        for i=0 to (a.Length-1) do
            a.[i] <- a.[i] * scalingFactor 
        a


