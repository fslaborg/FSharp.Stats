namespace FSharp.Stats.Testing


module TTest =

    /// Equal or unequal sample sizes, assume nothing about variance.
    /// input: (mean1,variance1,N1) (mean2,variance2,N3)
    let private noAssumtion (m1,s1,n1:float) (m2,s2,n2:float) =        
        raise (System.NotImplementedException "Not ready")  

    let twoSampleFromMeanAndVar (assumeEqualVariances:bool) (mean1,variance1,n1) (mean2,variance2,n2) =
        raise (System.NotImplementedException "Not ready")     

    let twoSample (assumeEqualVariances:bool) sample1 sample2 =
        raise (System.NotImplementedException "Not ready")     


