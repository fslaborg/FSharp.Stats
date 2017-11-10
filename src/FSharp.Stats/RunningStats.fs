namespace FSharp.Stats


/// Module to compute common statistical measure on 
module SummaryStats =

    type SummaryStats<'T> = {
        N           : 'T
        Mean        : 'T
        SumOfSquares : 'T
        Min         : 'T
        Max         : 'T
    }

    let createSummaryStats n mean sos min max =
        {N=n;Mean=mean;SumOfSquares=sos;Min=min;Max=max}

    
    ///
    let inline mean sStats = sStats.Mean
    ///
    let inline varPopulation sStats = sStats.SumOfSquares / sStats.N
    ///
    let inline var (sStats:SummaryStats<'T>) = 
        let one = LanguagePrimitives.GenericOne<'T>
        sStats.SumOfSquares / (sStats.N - one)


module RunningStats =

    type RunningStats<'T> = {
        N : int
        M1 : 'T
        M2 : 'T
        M3 : 'T
        M4 : 'T
    }

    let createRunningStats n m1 m2 m3 m4 =
        {N=n;M1=m1;M2=m2;M3=m3;M4=m4}

    //let inline combine (a:RunningStats<'T>) (b:RunningStats<'T>) = 
        
    //    let (..*) n a  = Ops.multByInt32 a n

    //    let cn = a.N + b.N
    //    let delta = b.M1 - a.M1
    //    let delta2 = delta * delta
    //    let delta3 = delta * delta2
    //    let delta4 = delta2 * delta2

    //    let cM1 = LanguagePrimitives.DivideByInt<'T> ( (Ops.multByInt32 a.M1 a.N ) + (Ops.multByInt32 b.M1 b.N))  cn
    //    let cM2 = LanguagePrimitives.DivideByInt<'T> (Ops.multByInt32 (a.M2 + b.M2 +  delta2) (a.N * b.N))  cn
    //    let cM3 = 
    //        let tmp = LanguagePrimitives.DivideByInt<'T> (Ops.multByInt32 (a.M3 + b.M3 + delta3) (a.N * b.N * (a.N - b.N))) (cn * cn)
    //        tmp + LanguagePrimitives.DivideByInt<'T> ((Ops.multByInt32 delta 3) * (Ops.multByInt32 b.M2 a.N) - (Ops.multByInt32 a.M2 b.N))  cn
    //    let cM4 =
    //        let tmp  = LanguagePrimitives.DivideByInt<'T> (Ops.multByInt32 (Ops.multByInt32 (a.M4 + b.M4 + delta4) (a.N*b.N)) (a.N*a.N - a.N*b.N + b.N*b.N)) (cn * cn * cn)
    //        //let tmp2 = LanguagePrimitives.DivideByInt<'T> ((Ops.multByInt32 delta2 6) * ((Ops.multByInt32 b.M2 (a.N * a.N)) + (Ops.multByInt32 a.M2 (b.N * b.N)))) (cn*cn)
    //        tmp + (LanguagePrimitives.DivideByInt<'T> (6 ..* delta2 * ( (a.N * a.N) ..* b.M2 +  (b.N * b.N) ..* a.M2)) (cn*cn)) + LanguagePrimitives.DivideByInt<'T>  (4 ..* delta * (a.N ..* b.M3 - b.N ..* a.M3)) cn
            
    //    createRunningStats cn cM1 cM2 cM3 cM4




    let inline mean rStats = 
        rStats.M1
    ///
    let inline varPopulation rStats = 
        LanguagePrimitives.DivideByInt rStats.M2 rStats.N
    ///
    let inline var (rStats:RunningStats<'T>) = 
        LanguagePrimitives.DivideByInt rStats.M2 (rStats.N-1)
    ///
    let inline stDev (rStats:RunningStats<'T>) = 
        sqrt (var rStats)
    ///
    let inline stDevPopulation (rStats:RunningStats<'T>) = 
        sqrt (varPopulation rStats)

//    ///Skewness
//    let inline skewness (rStats:RunningStats<'T>) = 
//        sqrt(double(n)) * M3/ pown(M2, 1.5)
//        sqrt (varPopulation rStats)
    
    /// Kurtosis
//    let inline kurtosis (rStats:RunningStats<'T>) = 
//        let one   = LanguagePrimitives.GenericOne< 'T >
//        let tmp = Ops.multByInt32 rStats.M4 rStats.N         
//        tmp / (rStats.M2 * rStats.M2) - (one + one + one)


    
//    let inline ofSeq (items:seq<'T>) : RunningStats< 'U >  =
//        use e = items.GetEnumerator()
//        let zero  = LanguagePrimitives.GenericZero< 'U > 
//        //let one   = LanguagePrimitives.GenericOne< 'U > 
                
//        let rec loop n (m1:'U) (m2:'U) (m3:'U) (m4:'U) =
//            match e.MoveNext() with
//            | true  -> 
//                let n'       = n + 1
//                let delta    = e.Current - m1
//                let delta_n  = LanguagePrimitives.DivideByInt< 'U > delta n
//                let delta_n2 = delta_n * delta_n
//                let term1    = Ops.multByInt32 (delta * delta_n) n'
//                let m1' = m1 + delta_n
//                let m4' = m4 + (Ops.multByInt32 (term1 * delta_n2) (n'*n' - 3*n' + 3)) + (Ops.multByInt32 (delta_n2 * m2) 6) - (Ops.multByInt32 (delta_n * m3) 4)
//                let m3' = m3 + (Ops.multByInt32 (term1 * m2) (n' - 2)) - (Ops.multByInt32 (delta_n * m2) 3 )
////                let m4' = m4 + (term1 * delta_n2 * (n'*n' - 3*n' + 3) + 6 * delta_n2 * m2 - 4 * delta_n * m3)
////                let m3' = m3 + (term1 * delta_n * (n' - 2) - 3 * delta_n * m2)
//                let m2' = term1

//                loop (n + 1) m1' m2' m3' m4' 
//            | false -> 
//                if (n > 1) then 
//                    createRunningStats n m1 m2 m3 m4 
//                else
//                    let nanU = zero / zero
//                    createRunningStats n nanU nanU nanU nanU
//        loop 0 zero zero zero zero
    

