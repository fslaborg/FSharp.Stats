namespace Microsoft.FSharp.Math

[<AutoOpen>]
module MatrixTopLevelOperators = 

    let matrix ll = Matrix.ofSeq ll
    let vector l  = Vector.ofSeq  l
    let rowvec l  = RowVector.ofSeq l

