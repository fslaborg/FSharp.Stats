//namespace Microsoft.FSharp.Math // old namespace
namespace FSharp.Stats

[<AutoOpen>]
module MatrixTopLevelOperators = 

    let matrix ll = Matrix.ofJaggedSeq ll
    let vector l  = Vector.ofSeq  l
    let rowvec l  = RowVector.ofSeq l

