namespace FSharp.Stats.Testing


/// Fisher-Z transformation for Pearson correlation coefficient after Hotelling (1953) for n< 50
module FisherHotelling =

    open FSharp.Stats

    /// Fisher-Z transformation for Pearson correlation coefficient    
    let private transformFisherZ r =
        0.5 * ( log ((1.+r) / (1.-r)) )
        
    /// Standart deviation Fisher-Z transformation for Pearson correlation coefficient
    let private stdFisherZ n =
        1. / sqrt(float(n-3))

    /// Fisher-Z transformation for Pearson correlation coefficient    
    /// after Hotelling (1953) for n < 50
    let private transformFisherHotellingZ r n =
        let hotelling r z = z - ( ( 3.*z + r) / (4.*(n-1.) ) )
        let z = transformFisherZ  r                
        if System.Double.IsNegativeInfinity(z) then       
            hotelling r (-18.71497388)
        elif System.Double.IsPositiveInfinity(z) then
            hotelling r (18.71497388)
        else
            hotelling r (z)                    


    /// Standart deviation Fisher-Z transformation for Pearson correlation coefficient
    /// after Hotelling (1953) for n< 50
    let private stdFisherHotellingZ n =
        if n < 1 then
            //printfn "Parameter warning: not n < 1"
            nan
        else
            1. / sqrt(float(n-1))

    type HotellingStatistics = { 
        Coefficient : float;
        PValue : float;
        ZValue : float; }
    
    let createHotellingStatistics r pval zval = { Coefficient = r; PValue = pval; ZValue = zval; } 

    let empty = { Coefficient = 0.0; PValue = nan; ZValue = nan; } 

    /// Pearson correlation (nan support by JackKnife leave one out)
    //  Biometry third edition R.Sokal / F. Rohlf page. 820
    let test dataA dataB = 
        let filtered = 
            Seq.zip dataA dataB
            |> Seq.filter (fun (a, b) -> not (System.Double.IsNaN(a) || System.Double.IsNaN(b)))

        let n = filtered |> Seq.length |> float
        let fdataA,fdataB = filtered|> List.ofSeq |> List.unzip        
        let cf = Correlation.Seq.pearson fdataA fdataB
        let nz = n * (transformFisherHotellingZ cf n)
        
        if n < 3. then 
            //printfn "Parameter warning: not n < 3"
            //[] |> Seq.ofList
            { Coefficient = cf; PValue = nan; ZValue = (transformFisherHotellingZ cf n) }
        
        else
            //printfn "%f" cf
            //Jackknife
            let jackknife =
                filtered             
                |> Seq.mapi (fun leaveOutI v -> filtered |> Seq.mapi (fun i x -> if (i = leaveOutI) then None else Some(x)))
            let pseudoCfs =
                jackknife
                |> Seq.map (fun fdatas -> 
                    let fdataA,fdataB = fdatas |> Seq.choose (fun x -> x) |> List.ofSeq |> List.unzip
                    Correlation.Seq.pearson fdataA fdataB
                    )
        
            let pseudoZs = pseudoCfs |> Seq.map (fun pcf -> (nz - ((n-1.)*(transformFisherHotellingZ pcf (n-1.)))) )
            let pseudoZ  = pseudoZs |> Seq.median
            let pseudoZStd =sqrt ((pseudoZs |> Seq.stDev) / (n-1.)) // maybe population
            let q = pseudoZ/pseudoZStd
            let studentT = TestStatistics.createTTest q (n-1.)
            //StudentT.DegreesOfFreedom <- (n-1.)
            //{ Coefficient = cf; PValue = 1.-StudentT.CumulativeDistribution(pseudoZ/pseudoZStd); ZValue = (transformFisherZ cf) }
            //{ Coefficient = cf; PValue = (pseudoZStd); ZValue = (transformFisherHotellingZ cf n) }
            //pseudoCfs
            if (System.Double.IsNaN(q)) then
                { Coefficient = cf; PValue = 0.0; ZValue = (transformFisherHotellingZ cf n) }            
            else
                { Coefficient = cf; PValue = studentT.PValue ; ZValue = (transformFisherHotellingZ cf n) }            

        // ##########################################################################
        // TEST: of Pearson correlation (nan support by JackKnife leave one out)
        // ################
        //let d1 = [159.;179.;100.;45.;384.;230.;100.;320.;80.;220.;320.;210.;]
        //let d2 = [14.4;15.2;11.3;2.5;22.7;14.9;1.41;15.81;4.19;15.39;17.25;9.52; ]
        //
        //let test = pearsonCorrelationNan d1 d2


