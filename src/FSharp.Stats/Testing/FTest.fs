namespace FSharp.Stats.Testing

module FTest = 

    open FSharp.Stats

    /// F-Test to compare two variances from data.
    let testVariancesFromData (data1:Vector<float>) (data2:Vector<float>) = 
        if data1.Length = 0 || data2.Length = 0 then failwithf "Data cannot be empty."
        let var1 = Seq.var data1
        let var2 = Seq.var data2
        let df1 = float data1.Length - 1.
        let df2 = float data2.Length - 1.
        if var1 > var2 then
            let statistic = var1 / var2
            Testing.TestStatistics.createFTest statistic df1 df2
        else
            let statistic = var2 / var1
            Testing.TestStatistics.createFTest statistic df2 df1

    /// F-Test to compare two variances from given parameters.
    let testVariancesFromVarAndDof (var1:float,df1:float) (var2:float,df2:float)  =
        if df1 = 0. || df2 = 0. then failwithf "Degrees of freedom cannot be zero."
        if var1 > var2 then 
            let statistic = var1/var2 
            FSharp.Stats.Testing.TestStatistics.createFTest statistic df1 df2 
        else
            let statistic = var2 / var1
            FSharp.Stats.Testing.TestStatistics.createFTest statistic df2 df1