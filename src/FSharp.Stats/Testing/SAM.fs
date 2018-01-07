namespace FSharp.Stats.Testing

open FSharp.Stats

module SAM = 

    /// Module with different estimation methods computing s0 
    module S0 =
        
        // <summary>
        /// Computes s0 using the nearest rank method
        // <summary>
        //
        /// <param name="percentile" >float</param>
        /// <param name="sis" >array of float</param>
        /// <param name="tt" >array of float</param>
        /// <param name="len" >int</param>
        /// <returns> s0 via nearest rank calulation </returns> 
        let nearestRank percentile (sis:array<float>) =
            if percentile < 0. || percentile > 1.0 then failwith "Percentile must be between 0.0 and 1.0"
            let sissorted= 
                sis
                |> Array.sort
            let index = System.Math.Round( percentile * float sis.Length, 0)
            
            sissorted.[(int index)]


        // <summary>
        /// Creates s0 by calulating the mean of the si values
        // <summary>
        //
        /// <param name="sis" >array of float</param>
        /// <param name="tt" >array of float</param>
        /// <param name="len" >int</param>
        /// <returns> s0 via median calulation </returns> 
        let meadian (sis:array<float>) = Array.median sis



        // <summary>
        /// Estimates s0
        // <summary>
        //
        /// <param name="sd" >array of float</param>
        /// <param name="tt" >array of float</param>
        /// <param name="len" >int</param>
        /// <returns> s0 via estimation </returns> 
        let estimate (sd:array<float>) (tt:array<float>) = 42



    let a = 42