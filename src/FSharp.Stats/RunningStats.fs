namespace FSharp.Stats


/// Module to compute common statistical measure on 
module SummeryStats =

    type SummeryStats<'T> = {
        N           : 'T
        Mean        : 'T
        SumOfSqures : 'T
        Min         : 'T
        Max         : 'T
    }

    let createSummeryStats n mean sos min max =
        {N=n;Mean=mean;SumOfSqures=sos;Min=min;Max=max}


    let var sStats = sStats.SumOfSqures / sStats.N


    type RunningStats<'T> = {
        N : int
        M1 : 'T
        M2 : 'T
        M3 : 'T
        M4 : 'T
    }

    let createRunningStats n m1 m2 m3 m4 =
        {N=n;M1=m1;M2=m2;M3=m3;M4=m4}
    
//    let inline ofSeq (items:seq<'T>) : RunningStats< 'U >  =
//        use e = items.GetEnumerator()
//        let rec loop n m1 m2 m3 m4 =
//            match e.MoveNext() with
//            | true  -> 
//                let n'       = n + 1
//                let delta    = e.Current - m1
//                let delta_n  = LanguagePrimitives.DivideByInt< 'U > delta n
//                let delta_n2 = delta_n * delta_n
//                let term1    = delta * delta_n * n'
//                let m1' = m1 + delta_n
//                let m4' = m4 + (term1 * delta_n2 * (n'*n' - 3*n' + 3) + 6 * delta_n2 * m2 - 4 * delta_n * m3)
//                let m3' = m3 + (term1 * delta_n * (n' - 2) - 3 * delta_n * m2)
//                let m2' = term1
//
//                loop (n + 1) m1' m2' m3' m4' 
//            | false -> if (n > 1) then createRunningStats n m1 m2 m3 m4 else createRunningStats n Unchecked.defaultof< 'U > Unchecked.defaultof< 'U > Unchecked.defaultof< 'U > Unchecked.defaultof< 'U >            
//        loop 0 LanguagePrimitives.GenericZero< 'U > LanguagePrimitives.GenericZero< 'U > LanguagePrimitives.GenericZero< 'U > LanguagePrimitives.GenericZero< 'U >  
    

