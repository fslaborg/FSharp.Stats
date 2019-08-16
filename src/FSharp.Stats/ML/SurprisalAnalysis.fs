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

    //let errorEstimation (A:Matrix<float>) =
    //    ()
    
    let getEnergyLandscapeData (saRes: SAResult) =
        
        let g1 = Matrix.getCol saRes.MolecularPhenotypes 1    
        let g2 = Matrix.getCol saRes.MolecularPhenotypes 2     
        let lambda1 = Matrix.getRow saRes.Potentials 1
        let lambda2 = Matrix.getRow saRes.Potentials 2  
        
        let XL1L2 =
            Array2D.init lambda1.Length lambda1.Length (fun lt1 lt2 ->
                Array.init (saRes.MolecularPhenotypes.NumRows) (fun i ->
                    - (lambda1.[lt1] * g1.[i]) - (lambda2.[lt2] * g2.[i]))
                |> Array.sum )
            |> JaggedArray.ofArray2D
        
        XL1L2
        //Chart.Surface(XL1L2,lab,lab,Opacity=0.85,Contours=Contours.initXyz(Show=true,Color="#8a8a8a"),Colorscale=Colorscale.Custom [(0.,"#0B12B0");((zero/2.),"#5F7BEF");(zero,"#BEBDBD");((zero+1.)/2.,"#E6925D");(1.,"#B4101F")])

    let getDiagonalEnergyData (saRes:SAResult)=
        [|0..saRes.Potentials.NumCols - 1|]
        |> Array.map (fun t -> 
            let l1 = saRes.Potentials.Row 1
            let l2 = saRes.Potentials.Row 2
            let g1 = saRes.MolecularPhenotypes.Column 1
            let g2 = saRes.MolecularPhenotypes.Column 2
            g1
            |> Seq.mapi (fun i x -> 
                    - l1.[t] * g1.[i] - l2.[t] * g2.[i]
                )
            |> Seq.sum
            )
        
        //|> Seq.mapi (fun i x -> labels |> Seq.item i ,labels|> Seq.item i,x) 
        //|> fun data -> Chart.Scatter3d(data,mode=StyleParam.Lines_Markers_Text,Color = "#474747",Dash=DrawingStyle.DashDot,Width = 5.,Labels = labels)//,Color="#121111")

    //let hikingpath labels lambda (saRes:ML.SurprisalAnalysis.SAResult) =
    //    [|0..saRes.Potentials.NumCols - 1|]
    //    |> Array.map (fun t -> 
    //        let l1 = saRes.Potentials.Row lambda
    //        let g1 = saRes.MolecularPhenotypes.Column lambda
    //        g1
    //        |> Seq.mapi (fun i x -> 
    //             - l1.[t] * g1.[i]
    //            )
    //        |> Seq.sum
    //        )
    //    |> Seq.mapi (fun i x -> 
    //        if lambda = 1 then 
    //            0,i,x
    //        else i,0,x)
    //    |> fun data -> Chart.Scatter3d(data,mode=StyleParam.Lines_Markers_Text,Dash=DrawingStyle.DashDot,Width = 5.,Labels = (labels|> Seq.map (fun l -> sprintf "l%i_%s" lambda l )))



//let com title labels saRes =
//    let xAxis() = LinearAxis.init(Title="lambda 2 at timepoint",Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=true,Showline=true)
//    let yAxis() = LinearAxis.init(Title="lambda 1 at timepoint",Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=true,Showline=true)
//    let ZAxis() = LinearAxis.init(Title="Free energy / RT",Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=true,Showline=true)//,Range=Range.MinMax(-1000.,1000.))
//    let surfacePlot =
//        getSurfacePlotData saRes
//        |> fun data -> 
//            let max = (data) |> Array.concat |> Array.max
//            let min = (data) |> Array.concat |> Array.min
//            let zero = - min / (max - min)
//            Chart.Surface(data,labels,labels,Opacity=0.85,Contours=Contours.initXyz(Show=true,Color="#8a8a8a"),Colorscale=Colorscale.Custom [(0.,"#0B12B0");((zero/2.),"#5F7BEF");(zero,"#BEBDBD");((zero+1.)/2.,"#E6925D");(1.,"#B4101F")])
//    let diagonal =
//        getDiagonalData saRes
//        |> Seq.mapi (fun i x -> labels |> Seq.item i ,labels|> Seq.item i,x) 
//        |> fun data -> Chart.Scatter3d(data,mode=StyleParam.Lines_Markers_Text,Color = "#474747",Dash=DrawingStyle.DashDot,Width = 5.,Labels = labels)//,Color="#121111")

//    [
//    surfacePlot
//    diagonal
//    ]
//    |> Chart.Combine
//    |> Chart.withSize(1000.,1000.)
//    |> Chart.withX_Axis(xAxis())
//    |> Chart.withY_Axis(yAxis())
//    |> Chart.withZ_Axis(ZAxis())
//    |> Chart.withTitle title
//    |> Chart.Show