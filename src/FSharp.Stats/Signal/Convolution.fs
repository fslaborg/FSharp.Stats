namespace FSharp.Stats.Signal

open FSharp.Stats



module Convolution =
    
    /// <summary>Computes a complete non-circular convolution of x and y. This function does not use<br />the fast fourier transformation to calculate the convolution and can be slow if x and y are large.<br />This function does not perform a padding of x and y. </summary>
    /// <remarks></remarks>
    /// <param name="v1"></param>
    /// <param name="v2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let convolve (v1:vector) (v2:vector) = 
        let k = (v1.Length+v2.Length-1)
        let tmp = Vector.zeroCreate k
        for i = 0 to v1.Length-1 do
            for j = 0 to (v2.Length-1) do 
                tmp.[i+j] <- tmp.[i+j] + (v1.[i] * v2.[j])
        tmp

