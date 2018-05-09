namespace FSharp.Stats.ML

module SurprisalAnalysis =
    open FSharp.Stats
    open FSharp.Stats.Algebra.LinearAlgebra
    open ProviderService
    
    ///Summary type for the results of applying Surprisal Analysis to a dataset
    type SAResult = 
        {
            ///left singular vectors resulting from the thin SVD of the input matrix
            LeftSingularVecors : Matrix<float>
            ///right singular vectors resulting from the thin SVD of the input matrix
            RightSingularVectors : Matrix<float>
            ///diagonal matrix containing the singular values resulting from the thin SVD of the input matrix
            SingularValuesDiag: Matrix<float>
            ///Vector containing the singular values resulting from the thin SVD of the input matrix in descending order
            SingularValues: Vector<float>
            ///
            MolecularPhenotypes: Matrix<float>
            ///
            Potentials: Matrix<float>
            //Error estimation not implemented yet
            //ErrorMatrix: Matrix<float>
        } 
    
    let private createSAResult lsv rsv svdiag sv mp pot = {LeftSingularVecors=lsv; RightSingularVectors=rsv; SingularValuesDiag=svdiag; SingularValues=sv; MolecularPhenotypes=mp; Potentials=pot}

    ///performs Surprisal Analysis on the input matrix A. For meaningfull results, A should be 
    ///of the following form:
    ///The rows contain measurements for a single unique entity,
    ///corresponding to timepoints represented by the columns
    let compute (A:Matrix<float>) : SAResult=
        //perform SVD
        let sv,lsv,rsv = thinSVD A
        //Singular values in diagonal matrix
        let svMatrix = Matrix.diag sv
        //get time dependant potentials
        let lagranges = svMatrix * rsv
        createSAResult lsv rsv svMatrix sv lsv lagranges

    let errorEstimation (A:Matrix<float>) =
        ()
