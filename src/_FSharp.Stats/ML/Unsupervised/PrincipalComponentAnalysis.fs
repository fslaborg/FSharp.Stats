namespace BioFSharp.Stats.ML.Unsupervised


open BioFSharp.Stats
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double


(** Problems:
    - How to calculates Loadings/Correlation of Components
    - Matrix Adjustment: Doas standartisation has to be performend wtih the whole matix or orignal matrix if
      suplemenal variables are includet at transformation.
    
     *)

/// Principle component analysis 
module PCA =

    /// Represents a principle component 
    type Component = { // Eigenvectors are columns of matrix Q (loading matrix)
                       // The elements of Q (somethimes refered as loadings) differs from loadings only by the normalization. The elements of Q (Eigenvectors) are normalized such that
                       // the sum of the squared elements of a given component is equal to one.
                       EigenVector          : float[];
                       // EigenValues represent either the variance of the original data contained in each principal component (in case they were computed from the covariance matrix),
                       // or the amount of correlation captured by the respective principle components (in case they were computed from the correlation matrix)
                       EigenValue           : float;
                       // Coefficients of correlation 
                       Loadings             : float[];
                       Proportion           : float;
                       CumulativeProportion : float;
                       Index                : int;
                     }

    type AdjustmentDirection =
        | Obverse
        | Reverse

    /// AdjustmentFactorygiven,given a dataset, 
    // should generate a adjusted dataset
    type AdjustmentFactory = AdjustmentDirection -> Matrix<float> -> Matrix<float>

    /// Returns an AdjustmentFactory which centers the data
    let toAdjustCenter (data:Matrix<float>) : AdjustmentFactory =        
        let colMeans = StatisticalMeasure.Matrix.columnMean data |> Seq.toArray                     
        let adjust (direction:AdjustmentDirection) (aData:Matrix<float>) = 
            match direction with
            | Obverse -> aData |> Matrix.mapi (fun ri ci value -> value - colMeans.[ci] )
            | Reverse -> aData |> Matrix.mapi (fun ri ci value -> value + colMeans.[ci] )
        adjust


    /// Returns an AdjustmentFactory as covariance matrix
    let toAdjustCovariance (data:Matrix<float>) : AdjustmentFactory =                
        let colMeans = StatisticalMeasure.Matrix.columnMean data |> Seq.toArray                     
        let sqrtI = sqrt (float data.RowCount)
        let adjust (direction:AdjustmentDirection) (aData:Matrix<float>) = 
            match direction with
            | Obverse -> aData |> Matrix.mapi (fun ri ci value -> (value - colMeans.[ci]) / sqrtI )
            | Reverse -> aData |> Matrix.mapi (fun ri ci value -> (value * sqrtI) + colMeans.[ci] )
        adjust
                

    /// Returns an AdjustmentFactory which centers and standardize the data
    let toAdjustStandardize (data:Matrix<float>) : AdjustmentFactory =                
        let colMeans = StatisticalMeasure.Matrix.columnMean data |> Seq.toArray                     
        // Atttention: not entierly shure if space of data before
        let colStDev = [| for coli in data.EnumerateColumns() do
                                yield StatisticalMeasure.stDevPopulation coli |]        
        let adjust (direction:AdjustmentDirection) (aData:Matrix<float>) = 
            match direction with
            | Obverse -> aData |> Matrix.mapi (fun ri ci value -> if colStDev.[ci] = 0. then raise (System.ArgumentException(sprintf "Standard deviation cannot be zero (cannot standardize the constant variable at column index %i" ci))
                                                                  (value - colMeans.[ci]) / colStDev.[ci] )
            | Reverse -> aData |> Matrix.mapi (fun ri ci value -> if colStDev.[ci] = 0. then raise (System.ArgumentException(sprintf "Standard deviation cannot be zero (cannot standardize the constant variable at column index %i" ci))
                                                                  (value * colStDev.[ci]) + colMeans.[ci] )
        adjust


    /// Returns an AdjustmentFactory which centers and standardize the data
    let toAdjustCorrelation (data:Matrix<float>) : AdjustmentFactory =                
        let colMeans = StatisticalMeasure.Matrix.columnMean data |> Seq.toArray                     
        let sqrtI = sqrt (float data.RowCount)
        // Atttention: not entierly shure if space of data before
        let colStDev = [| for coli in data.EnumerateColumns() do
                                yield StatisticalMeasure.stDevPopulation coli |]        
        let adjust (direction:AdjustmentDirection)  (aData:Matrix<float>) = 
            match direction with
            | Obverse -> aData |> Matrix.mapi (fun ri ci value -> if colStDev.[ci] = 0. then raise (System.ArgumentException(sprintf "Standard deviation cannot be zero (cannot standardize the constant variable at column index %i" ci))
                                                                  (value - colMeans.[ci]) / colStDev.[ci] * sqrtI)
            | Reverse -> aData |> Matrix.mapi (fun ri ci value -> if colStDev.[ci] = 0. then raise (System.ArgumentException(sprintf "Standard deviation cannot be zero (cannot standardize the constant variable at column index %i" ci))
                                                                  (value * colStDev.[ci] / sqrtI) + colMeans.[ci] )
        adjust        
           
             

    /// Creates a principle component type
    let createComponent eigenVector eigenValue loadings proportion cumulativeProportion index =
        { EigenVector = eigenVector; EigenValue = eigenValue; Loadings = loadings; Proportion = proportion; CumulativeProportion = cumulativeProportion; Index = index; }


    /// Creates the principle components of eigenVectors and eigenValues
    let private createComponentsOf (eigenVectors: Matrix<float>) (singularValues:seq<float>) =
        // Eigenvalues are the square of the singular values
        let eigenValues = singularValues |> Seq.map (fun x -> x * x )        
        // Sort eigenvalues according abs
        let sortedEigenValues =                 
            eigenValues 
            |> Seq.mapi (fun i x -> (i,x))
            |> Seq.sortBy (fun (i,ev) -> - (abs ev))
        // Calculate proportions of variance
        let sumOfEigenValues = 
            let sum = eigenValues |> Seq.sumBy (fun x -> abs x)
            if sum = 0.0 then 0.0 else 1. / sum
        // Calculate cumulative proportions of variance
        let componentCumulative =
            sortedEigenValues 
            |> Seq.scan (fun (index,cpv) (colI,ev) -> 
                let componentProportion = (abs ev) * sumOfEigenValues
                (index + 1,cpv + componentProportion)) (0,0.0) |> Seq.skip 1 
        
//        // Calculate factor structure (not needed because Equals Q)
//        let singularValues = eigenValues |> Seq.map (fun ev -> sqrt ev) |> Seq.toArray
//        let lM             = DiagonalMatrix(singularValues.Length,singularValues.Length,singularValues)
//        let fsMatrix       = lM * eigenVectors

        // Create component type                
        Seq.map2 (fun (index,cpv) (colI,ev) ->
                let componentProportion = (abs ev) * sumOfEigenValues
                //createComponent (eigenVectors.Column(colI) |> Seq.toArray) ev (fsMatrix.Column(colI) |> Seq.toArray) componentProportion cpv index) componentCumulative sortedEigenValues 
                createComponent (eigenVectors.Column(colI) |> Seq.toArray) ev (singularValues |> Seq.toArray) componentProportion cpv index) componentCumulative sortedEigenValues 
        |> Seq.toArray        
        


    /// Computes a principal componant analysis of a given covariance matrix
    let computeOfCovarianceMatrix (covMatrix: #Matrix<float>) =
        // Calculate the eigenvectors and eigenvalues
        let evd = covMatrix.Evd()
        
        createComponentsOf (evd.EigenVectors) (evd.EigenValues |> Seq.map (fun x -> sqrt x.r))



    /// Computes a principal componant analysis of a given covariance matrix
    /// !Attention: Matrix needs to be centered before
    //  The SVD method is used for numerical accuracy
    let computeOfMatrix (dataMatrix: #Matrix<float>) =
        // Perform the Singular Value Decomposition (SVD) of the matrix                
        let transpose = if dataMatrix.RowCount < dataMatrix.ColumnCount then true else false
        
        let svd            =  if transpose then dataMatrix.Transpose().Svd(true) else dataMatrix.Svd(true)
        let singularValues = svd.W.Diagonal()
        // EigenVectors are the right sigular vectors
        let eigenVectors   = if transpose then svd.U else svd.VT.Inverse()
//        // Eigenvalues are the square of the singular values
//        let eigenValues = singularValues |> Vector.map (fun x -> x * x ) 
        
        createComponentsOf eigenVectors singularValues
    


    /// Computes a principal componant analysis
    let compute (adj:AdjustmentFactory) (dataMatrix: #Matrix<float>) =                             
        computeOfMatrix (adj AdjustmentDirection.Obverse dataMatrix)
        

//    /// Filter components according to min variance
//    let t =
//        1.0

    /// Returns feature matrix (eigenvector matrix) from components
    let getFeatureMatrixOfComponents (components:Component[]) = 
        components
        |> Seq.map (fun c -> c.EigenVector) 
        |> Seq.toList
        |> DenseMatrix.OfColumnArrays


    /// Returns communality
    let getCommunality (components:Component[]) = 
        let fsMatrix = 
            components
            |> Seq.map (fun c -> c.Loadings) 
            |> Seq.toList
            |> DenseMatrix.OfColumnArrays
        fsMatrix.TransposeAndMultiply(fsMatrix).Diagonal().ToArray()


    /// Projects a given matrix into principal component space (projections or factor scores)
    let transform (adj:AdjustmentFactory) (components:Component[]) (dataMatrix: #Matrix<float>) = 
        let dataM    = adj AdjustmentDirection.Obverse dataMatrix
        let featureM = getFeatureMatrixOfComponents components
            
        dataM * featureM


    ///   Reverts a set of projected data into it's original form. Complete reverse
    ///   transformation is only possible if all components are present, and, if the
    ///   data has been standardized, the original standard deviation and means of
    ///   the original matrix are known.
    let revert (adj:AdjustmentFactory) (components:Component[]) (dataMatrix: #Matrix<float>) =         
        let featureM = getFeatureMatrixOfComponents components        
        
        let revMatrix = (DenseMatrix.OfMatrix dataMatrix) * featureM.Transpose()
        adj AdjustmentDirection.Reverse revMatrix
         

    /// Contribution of an observation to a component
    //  High contribution value means contribution of this observation is larger then the average contribution
    let contributionOfTransformed (transformedDataMatrix: #Matrix<float>) = 
        // square future matrix 
        let sfm = transformedDataMatrix |> Matrix.map (fun x -> x * x)
        // sum over columns
        let colSums = sfm |> Matrix.sumRowsBy (fun coli col -> col)  
        
        sfm 
        |> Matrix.mapCols (fun coli col -> col.Divide(colSums.[coli]))


    /// Calculates the squared cosine of a component with an observation
    //  The squared cosine shows the importance of a component for a given observation 
    let importanceOfTransformed (transformedDataMatrix: #Matrix<float>) =         
        // square future matrix 
        let sfm = transformedDataMatrix |> Matrix.map (fun x -> x * x)
        let d2 = sfm |> Matrix.sumColsBy (fun coli col -> col)
        
        sfm 
        |> Matrix.mapCols (fun coli col -> col.PointwiseDivide(d2))

    
    /// Returns xy-coordinates for scree plot in a tuple (component number vs. EigenValue)    
    /// Scree plot: represents the ability of PCs to explain de variation in data
    let zipScree (components:Component[]) =
        components |> Seq.map ( fun c -> (float c.Index,c.EigenValue) )

    let zipScores componentIndex1 componentIndex2 (transformedDataMatrix: #Matrix<float>) = 
        Seq.zip (transformedDataMatrix.Column(componentIndex1)) (transformedDataMatrix.Column(componentIndex1))



