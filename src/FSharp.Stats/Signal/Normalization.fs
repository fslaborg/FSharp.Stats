namespace FSharp.Stats


module Normalization =

    /// z normalization using the population standard deviation of population
    //Bortz J., Schuster C., Statistik für Human- und Sozialwissenschaftler, 7 (2010), p. 35
    let zScoreTransformPopulation (yVal:Vector<float>) =
        let yMean = Seq.mean yVal 
        let std   = Seq.stDevPopulation yVal
        yVal |> Vector.map (fun x -> (x - yMean) / std) 

    /// z normalization using the sample standard deviation
    //Bortz J., Schuster C., Statistik für Human- und Sozialwissenschaftler, 7 (2010), p. 35
    let zScoreTrans (yVal:Vector<float>) =
        let yMean = Seq.mean yVal
        let std   = Seq.stDev yVal
        yVal |> Vector.map (fun x -> (x - yMean) / std) 

