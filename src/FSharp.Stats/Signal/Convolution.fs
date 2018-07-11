namespace FSharp.Stats.Signal

open FSharp.Stats



module Convolution =
    
    /// Computes a complete non-circular convolution of x and y. This function does not use
    /// the fast fourier transformation to calculate the convolution and can be slow if x and y are large.
    /// This function does not perform a padding of x and y. 
    let convolve (v1:vector) (v2:vector) = 
        let k = (v1.Length+v2.Length-1)
        let tmp = Vector.zero k
        for i = 0 to v1.Length-1 do
            for j = 0 to (v2.Length-1) do 
                tmp.[i+j] <- tmp.[i+j] + (v1.[i] * v2.[j])
        tmp

