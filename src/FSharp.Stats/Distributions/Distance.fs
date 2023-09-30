namespace FSharp.Stats.Distributions

open System


/// Module to compute Distances between Distributions
module Distance =

    /// Module to compute Distances between 1D Distributions
    module OneDimensional = 

        // Implementation of CDF, Wasserstein and Energy Distance based on implementation in SciPy
        // https://github.com/scipy/scipy/blob/d8a77785d5c4943ef47c5824e6713ba9b1e68fc7/scipy/stats/stats.py#L7072
        //
        // Reference :
        // Bellemare, Danihelka, Dabney, Mohamed, Lakshminarayanan, Hoyer, Munos 
        // "The Cramer Distance as a Solution to Biased Wasserstein Gradients" (2017). :arXiv:`1705.10743`





        /// <summary>Basic square function</summary>
        /// <remarks></remarks>
        /// <param name="square"></param>
        /// <param name="x"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let private square x = x * x

        /// <summary>For value x, find index of the smallest value in sorted array which is larger than it</summary>
        /// <remarks></remarks>
        /// <param name="getCDFIndices"></param>
        /// <param name="values"></param>
        /// <param name="x"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let private getCDFIndices (values : float []) x =
            match Array.tryFindIndex (fun e -> e > x) values with
            | Some x -> x
            | None -> values.Length

        /// <summary>Returns the index mapping required to sort the values from small to large</summary>
        /// <remarks></remarks>
        /// <param name="getSortedIndices"></param>
        /// <param name="values"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let private getSortedIndices (values : float []) =
            values
            |> Array.indexed
            |> Array.sortBy snd
            |> Array.map fst

        /// <summary>Sort given values by the order of the indices</summary>
        /// <remarks></remarks>
        /// <param name="sortByIndices"></param>
        /// <param name="indices"></param>
        /// <param name="values"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let private sortByIndices indices (values : float []) =
            indices 
            |> Array.map (fun i -> values.[i])


        /// <summary>Computes the normalized positions of allValues between the values of the distribution vs</summary>
        /// <remarks></remarks>
        /// <param name="getCDF"></param>
        /// <param name="vs"></param>
        /// <param name="allValues"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let private getCDF (vs : float []) (allValues : float []) = 
            let len = vs.Length |> float
            let sorted = vs |> Array.sort 
            let cdfIndices = 
                allValues.[0 .. allValues.Length - 2]
                |> Array.map (getCDFIndices sorted)

            cdfIndices
            |> Array.map (fun i -> (float i) / len)

        /// <summary>Computes the weight-normalized positions of allValues between the values of the distribution vs</summary>
        /// <remarks></remarks>
        /// <param name="getWeightedCDF"></param>
        /// <param name="vs"></param>
        /// <param name="weights"></param>
        /// <param name="allValues"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let private getWeightedCDF (vs : float []) weights (allValues : float []) = 
            let len = vs.Length
            let sortIndices = vs |> getSortedIndices
            let sorted = sortByIndices sortIndices vs 
            let cdfIndices = 
                allValues.[0 .. allValues.Length - 2]
                |> Array.map (getCDFIndices sorted)
            let cumulativeWeights = 
                weights
                |> sortByIndices sortIndices               
                |> Array.scan (+) 0. // Returns an array representing the cumulative sum beginning from the start of the array up to each element
            sortByIndices cdfIndices cumulativeWeights
            |> Array.map (fun x -> x / cumulativeWeights.[len])


        /// <summary>Computes the distance between the two cumulative distributions</summary>
        /// <remarks></remarks>
        /// <param name="computeDistanceOfCDFs"></param>
        /// <param name="p"></param>
        /// <param name="xCDFs"></param>
        /// <param name="yCDFs"></param>
        /// <param name="deltas"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let private computeDistanceOfCDFs p xCDFs yCDFs deltas = 
            match p with
            | 0 -> failwith "p can't be 0"
            | 1 ->
                (xCDFs,yCDFs,deltas)
                |||> Array.map3 (fun xCDF yCDF delta ->
                    delta * (abs (xCDF - yCDF))
                )
                |> Array.sum
            | 2 ->
                (xCDFs,yCDFs,deltas)
                |||> Array.map3 (fun xCDF yCDF delta ->
                    delta * (square (xCDF - yCDF))
                )
                |> Array.sum
                |> sqrt
            | _ -> 
                let p = float p
                let pow x = Math.Pow(x,p)
                let root x = Math.Pow(x,(1. / p))

                (xCDFs,yCDFs,deltas)
                |||> Array.map3 (fun xCDF yCDF delta ->
                    delta * (pow (xCDF - yCDF))
                )
                |> Array.sum
                |> root

        let private validateWeightedDistribution (vs : float []) (weights : float []) xy =
            if vs.Length <> weights.Length then
                failwithf "Given values of distribtuion %s and its weights differ in length" xy
            if weights |> Array.exists ((>) 0.) then
                failwithf "Weights are not allowed to be negative but were negative for distribution %s" xy


        /// <summary>Distance between two 1D distributions. p has to be positive</summary>
        /// <remarks></remarks>
        /// <param name="p"></param>
        /// <param name="xs"></param>
        /// <param name="ys"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let cdfDistance p (xs : float []) (ys : float []) =

            if xs.Length = 0 then failwith "Given distribtuion xs is empty"
            if ys.Length = 0 then failwith "Given distribtuion ys is empty"

            let allValues = 
                Array.append xs ys 
                |> Array.sort

            // Difference of successive elements
            let deltas = 
                Array.init 
                    (allValues.Length - 1) 
                    (fun i -> allValues.[i+1] - allValues.[i])

            let xCDFs = 
                getCDF xs allValues

            let yCDFs = 
                getCDF ys allValues

            computeDistanceOfCDFs p xCDFs yCDFs deltas

        /// <summary>Distance between two 1D distributions. p has to be positive</summary>
        /// <remarks></remarks>
        /// <param name="p"></param>
        /// <param name="xs"></param>
        /// <param name="ys"></param>
        /// <param name="xWeights"></param>
        /// <param name="yWeights "></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let cdfDistanceWeighted p (xs : float []) (ys : float [])  xWeights yWeights =

            if xs.Length = 0 then failwith "Given distribtuion xs is empty"
            if ys.Length = 0 then failwith "Given distribtuion ys is empty"
            validateWeightedDistribution xs xWeights "xs"
            validateWeightedDistribution ys yWeights "ys"

            let allValues = 
                Array.append xs ys 
                |> Array.sort

            // Difference of successive elements
            let deltas = 
                Array.init 
                    (allValues.Length - 1) 
                    (fun i -> allValues.[i+1] - allValues.[i])

            let xCDFs = 
                getWeightedCDF xs xWeights allValues

            let yCDFs = 
                getWeightedCDF ys yWeights allValues

            computeDistanceOfCDFs p xCDFs yCDFs deltas



        /// <summary>Wasserstein distance between two 1D distributions</summary>
        /// <remarks></remarks>
        /// <param name="xs"></param>
        /// <param name="ys"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let wassersteinDistance (xs : float []) (ys : float []) =
            cdfDistance 1 xs ys

        /// <summary>Weighted Wasserstein Distance between two 1D distributions</summary>
        /// <remarks></remarks>
        /// <param name="xs"></param>
        /// <param name="ys"></param>
        /// <param name="xWeights"></param>
        /// <param name="yWeights"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let wassersteinDistanceWeighted (xs : float []) (ys : float []) xWeights yWeights =
            cdfDistanceWeighted 1 xs ys xWeights yWeights

        /// <summary>Energy distance between two 1D distributions</summary>
        /// <remarks></remarks>
        /// <param name="xs"></param>
        /// <param name="ys"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let energyDistance (xs : float []) (ys : float []) =
            cdfDistance 2 xs ys
            |> ((*) (sqrt 2.)) 

        /// <summary>Weighted Energy Distance between two 1D distributions</summary>
        /// <remarks></remarks>
        /// <param name="xs"></param>
        /// <param name="ys"></param>
        /// <param name="xWeights"></param>
        /// <param name="yWeights"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let energyDistanceWeighted (xs : float []) (ys : float []) xWeights yWeights =
            cdfDistanceWeighted 2 xs ys xWeights yWeights
            |> ((*) (sqrt 2.)) 
