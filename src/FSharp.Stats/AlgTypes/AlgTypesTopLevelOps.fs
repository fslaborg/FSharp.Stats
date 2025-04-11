namespace FSharp.Stats


[<AutoOpen>]
module AlgTypesTopLevelOps = 

    let matrix ll = Matrix.ofRowSeq ll
    let vector l  = Vector.ofSeq  l
    //let rowvec l  = RowVector.ofSeq l
