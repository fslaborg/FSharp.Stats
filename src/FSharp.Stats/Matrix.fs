namespace FSharp.Stats

#nowarn "60" // implementations in augmentations
#nowarn "69" // implementations in augmentations

open System
open System.Globalization
open System.Collections
open System.Collections.Generic
open System.Diagnostics

//type matrix = Matrix<float>
//type vector = Vector<float>
//type rowvec = RowVector<float>

module MRandom =
    let randomGen = new System.Random()
    let float f = randomGen.NextDouble() * f

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix = 
     

    type Orientation =
        | RowWise
        | ColWise
        member this.Inverse =
            match this with
            | RowWise -> Orientation.ColWise
            | ColWise -> Orientation.RowWise
        static member inverse (o:Orientation) = 
            o.Inverse

    type DataSource = 
        |Sample
        |Population

    module Generic = 

        module MS = SpecializedGenericImpl

        // Accessors
        let get (matrix:Matrix<_>) indexRow indexCol = matrix.[indexRow,indexCol]
        let set (matrix:Matrix<_>) indexRow indexCol value = matrix.[indexRow,indexCol] <- value
        // Creation
        let ofList lists = MS.listM lists
        let ofSeq sources  = MS.seqM sources
        let ofColSeq sources  = MS.seqCM sources
        let init lengthRow lengthCol initializer = MS.initM  lengthRow lengthCol initializer
        let ofArray2D (array: 'T[,])  : Matrix<'T> = MS.arrayM array
        /// <summary>Creates a sparse matrix based on the CSR format</summary>
        /// <remarks></remarks>
        /// <param name="array"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let sparseOfArray2D (array: 'T[,]) : Matrix<'T> = MS.arraySM array
        let toArray2D (matrix:Matrix<_>) = Array2D.init matrix.NumRows matrix.NumCols (fun i j -> get matrix i j)
        let toJaggedArray (m:Matrix<_>) = [|for i=0 to m.NumRows-1 do yield (Array.init m.NumCols (fun j -> get m i j))|]
        let toJaggedColArray (m:Matrix<_>) = [|for i=0 to m.NumCols-1 do yield (Array.init m.NumRows (fun j -> get m j i))|]
        let toJaggedSeq (m: Matrix<'a>) = Seq.init m.NumRows (fun i -> Seq.init m.NumCols (fun j -> get m i j))
        let toJaggedColSeq (m: Matrix<'a>) = Seq.init m.NumCols (fun i -> Seq.init m.NumRows (fun j -> get m j i))
        let initNumeric lengthRow lengthCol initializer = MS.initNumericM lengthRow lengthCol initializer
        [<Obsolete("Use zeroCreate instead.")>]
        let zero lengthRow lengthCol = MS.zeroM lengthRow lengthCol
        let zeroCreate lengthRow lengthCol = MS.zeroM lengthRow lengthCol
        let identity m = MS.identityM m
        let create lengthRow lengthCol value = MS.constM lengthRow lengthCol value
        let ofScalar scalar = MS.scalarM scalar
        let diag v = MS.diagM v
        let initDiagonal v = MS.diagM v
        let constDiag n x = MS.constDiagM n x

        // Operators
        /// <summary>Performs a element wise addition of matrices matrix1 and matrix2 (matrix1 + matrix2).<br />Only usable if both matrices have the same dimensions.</summary>
        /// <remarks></remarks>
        /// <param name="matrix1"></param>
        /// <param name="matrix2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let add matrix1 matrix2 = MS.addM matrix1 matrix2
        /// <summary>Performs a element wise substraction of matrices matrix1 and matrix2 (matrix1 - matrix2).<br />Only usable if both matrices have the same dimensions.</summary>
        /// <remarks></remarks>
        /// <param name="matrix1"></param>
        /// <param name="matrix2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let sub matrix1 matrix2 = MS.subM matrix1 matrix2
        /// <summary>Performs a left sided matrix multiplication of matrices matrix1 and matrix2 (matrix1 * matrix2).<br />Only usable if both matrices have the same dimensions.</summary>
        /// <remarks></remarks>
        /// <param name="matrix1"></param>
        /// <param name="matrix2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let mul matrix1 matrix2 = MS.mulM matrix1 matrix2
        /// <summary>Performs a matrix multiplication of the 1*n rowVector and the m*n matrix (rowVector*matrix).<br />Only usable if column number (n) of the vector equals the row number (m) of the matrix.</summary>
        /// <remarks></remarks>
        /// <param name="rowVector"></param>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let mulRV rowVector matrix = MS.mulRVM rowVector matrix
        /// <summary>Performs a matrix multiplication of a m*n matrix and the m*1 vector (matrix*vector).<br />Only usable if column number (n) of the matrix equals the row number (m) of the vector.</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <param name="vector"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let mulV matrix vector = MS.mulMV matrix vector
        /// <summary>Performs a element wise multiplication of matrices matrix1 and matrix2 (matrix1 * matrix2, Hadamard-Product).<br />Only usable if both matrices have the same dimensions.</summary>
        /// <remarks></remarks>
        /// <param name="matrix1"></param>
        /// <param name="matrix2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let cptMul matrix1 matrix2 = MS.cptMulM matrix1 matrix2
        /// <summary>Performs a element wise comparison of matrices matrix1 and matrix2 always preserving the greater value.<br />Only usable if both matrices have the same dimensions.</summary>
        /// <remarks></remarks>
        /// <param name="matrix1"></param>
        /// <param name="matrix2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let cptMax matrix1 matrix2 = MS.cptMaxM matrix1 matrix2
        /// <summary>Performs a element wise comparison of matrices matrix1 and matrix2 always preserving the smaller value.<br />Only usable if both matrices have the same dimensions.</summary>
        /// <remarks></remarks>
        /// <param name="matrix1"></param>
        /// <param name="matrix2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let cptMin matrix1 matrix2 = MS.cptMinM matrix1 matrix2
        /// <summary>Builds a new matrix where the elements are the result of multiplying every element of the given matrix with the given value</summary>
        /// <remarks></remarks>
        /// <param name="value"></param>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let scale value matrix = MS.scaleM value matrix
        /// <summary>Performs a dot product of matrices matrix1 and matrix2.<br />Only usable if both matrices have the same dimensions.</summary>
        /// <remarks></remarks>
        /// <param name="matrix1"></param>
        /// <param name="matrix2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let dot matrix1 matrix2 = MS.dotM matrix1 matrix2
        /// <summary>Scales the matrix by element wise mulitplication with minus 1.</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let neg matrix = MS.negM matrix
        /// <summary>Computes the trace of the matrix by summing elements of the diagonal.<br />Only usable if the matrix is a square matrix (m*m).</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let trace matrix = MS.traceM matrix
        /// <summary>Computes the sum of all matrix elements.</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let sum matrix = MS.sumM matrix
        /// <summary>Computes the product of all matrix elements.</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let prod matrix = MS.prodM matrix
        /// <summary>Frobenius matrix norm</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let norm matrix = MS.normM matrix
        /// <summary>Returns the transpose of the matrix.</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let transpose matrix = MS.transM matrix
        /// <summary>Performs an element wise addition of matrices matrix1 and matrix2 (matrix1 + matrix2).<br />Attention: the output overrides matrix1.<br />Only usable if both matrices have the same dimensions.</summary>
        /// <remarks></remarks>
        /// <param name="matrix1"></param>
        /// <param name="matrix2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inplaceAdd matrix1 matrix2 = MS.inplaceAddM matrix1 matrix2
        /// <summary>Performs an element wise substraction of matrices matrix1 and matrix2 (matrix1 - matrix2).<br />Attention: the output overrides matrix1.<br />Only usable if both matrices have the same dimensions.</summary>
        /// <remarks></remarks>
        /// <param name="matrix1"></param>
        /// <param name="matrix2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inplaceSub matrix1 matrix2 = MS.inplaceSubM matrix1 matrix2
        /// <summary>Iterates the given Matrix row wise and applies the function predicate element wise.<br />The iteration stops and returns true if an element satisfies the condition or false when the end of<br />the matrix is reached.</summary>
        /// <remarks></remarks>
        /// <param name="predicate"></param>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let exists predicate matrix = MS.existsM  predicate matrix
        /// <summary>Iterates the given Matrix row wise and applies the function predicate element wise.<br />The iteration stops and returns false if an element fails the condition or true when the end of<br />the matrix is reached.</summary>
        /// <remarks></remarks>
        /// <param name="predicate"></param>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let forall predicate matrix = MS.forallM  predicate matrix
        /// <summary>Iterates the given Matrix row wise and applies the function predicate element wise.<br />The iteration stops and returns true if an element satisfies the condition or false when the end of<br />the matrix is reached.</summary>
        /// <remarks></remarks>
        /// <param name="predicate"></param>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let existsi predicate matrix = MS.existsiM  predicate matrix
        /// <summary>Iterates the given Matrix row wise and applies the function predicate element wise.<br />The iteration stops and returns false if an element fails the condition or true when the end of<br />the matrix is reached.</summary>
        /// <remarks></remarks>
        /// <param name="predicate"></param>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let foralli predicate matrix = MS.foralliM predicate matrix
        ///
        let map mapping matrix = MS.mapM mapping matrix
        ///
        let copy matrix = MS.copyM matrix
        ///
        let mapi mapping matrix = MS.mapiM mapping matrix
        ///
        //TO_DO: refactor to take an Direction union case and use more descriptive name
        let getDiagN a n = MS.getDiagnM a n
        ///
        let getDiag matrix = MS.getDiagnM matrix 0
        ///
        let toDense matrix = MS.toDenseM matrix
        /// <summary>Creates a sparse matrix based on the CSR format</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let toSparse matrix = MS.toSparseM matrix
        ///
        let initDense lengthRow lengthCol source = MS.initDenseM lengthRow lengthCol source
        ///
        let initSparse lengthRow lengthCol source = MS.initSparseM lengthRow lengthCol source
        ///
        let fold folder state matrix = MS.foldM folder state matrix
        ///
        let foldi folder state matrix = MS.foldiM folder state matrix
        ///
        let compare matrix1 matrix2 = MS.compareM LanguagePrimitives.GenericComparer matrix1 matrix2
        ///
        let hash matrix = MS.hashM LanguagePrimitives.GenericEqualityComparer matrix
        /// <summary>Returns row of given index of a matrix</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <param name="index"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let getRow matrix index = MS.getRowM matrix index
        /// <summary>Replaces row of given index of a matrix with values of a vector, if vector length matches rowsize</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <param name="index"></param>
        /// <param name="vector"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let setRow (matrix:Matrix<_>) index (vector:Vector<_>) = MS.setRowM matrix index vector
        /// <summary>Returns col of given index of a matrix</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <param name="index"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let getCol matrix index = MS.getColM matrix index
        /// <summary>Replaces column of given index of a matrix with values of a vector, if vector length matches columnsize</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <param name="index"></param>
        /// <param name="vector"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let setCol (matrix:Matrix<_>) index (vector:Vector<_>) = MS.setColM matrix index vector
        ///
        let getCols matrix column1 column2 = MS.getColsM matrix (column1,column1+column2-1)
        ///
        let getRows matrix row1 row2 = MS.getRowsM matrix (row1,row1+row2-1)
        ///
        let getRegion matrix column1 row1 column2 row2 = MS.getRegionM matrix (column1,column1+column2-1) (row1,row1+row2-1)
        ///
        let ofRowVector rowVector = MS.rowvecM rowVector
        ///
        let ofVector vector = MS.vectorM vector
        /// <summary>takes the first column of the matrix as vector</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let toVector matrix = MS.toVectorM matrix
        /// <summary>takes the first row of the matrix as rowvector</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let toRowVector matrix = MS.toRowVectorM matrix
        ///
        let toScalar matrix = MS.toScalarM matrix
        /// <summary>reads matrix from delimiter separated file</summary>
        /// <remarks></remarks>
        /// <param name="path"></param>
        /// <param name="separator"></param>
        /// <param name="removeHeaderRow"></param>
        /// <param name="removeHeaderCol"></param>
        /// <param name="transform"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let readCSV (path: string) (separator: char) (removeHeaderRow: bool) (removeHeaderCol: bool) (transform: string -> 'a) = 
            IO.File.ReadAllLines(path)
            |> fun x -> 
                if removeHeaderRow then Array.tail x else x
            |> Array.map (fun x -> 
                let tmp = x.Split separator
                if removeHeaderCol then 
                    tmp.[1..] |> Array.map transform
                else tmp |> Array.map transform
                )
            |> ofSeq

        let inplace_assign f matrix                = MS.inplaceAssignM  f matrix
        let inplace_cptMul matrix1 matrix2         = MS.inplaceCptMulM matrix1 matrix2
        let inplace_scale value matrix             = MS.inplaceScaleM value matrix
        let inplace_map mapping matrix             = MS.inplace_mapM mapping matrix
        let inplace_mapi mapping matrix            = MS.inplace_mapiM mapping matrix
        
        [<Obsolete("Use ofRowVector instead.")>]
        let of_rowvec rowVector                    = ofRowVector rowVector
        [<Obsolete("Use ofVector instead.")>]
        let of_vector vector                       = ofVector vector
        [<Obsolete("Use toVector instead.")>]
        let to_vector matrix                       = toVector matrix
        [<Obsolete("Use toRowVector instead.")>]
        let to_rowvec matrix                       = toRowVector matrix
        [<Obsolete("Use toScalar instead.")>]
        let to_scalar matrix                       = toScalar matrix
        [<Obsolete("Use inplaceAdd instead.")>]
        let inplace_add matrix1 matrix2            = inplaceAdd matrix1 matrix2
        [<Obsolete("Use inplaceSub instead.")>]
        let inplace_sub matrix1 matrix2            = inplaceSub matrix1 matrix2
        [<Obsolete("Use ofScalar instead.")>]
        let of_scalar scalar                       = ofScalar scalar
        [<Obsolete("Use ofList instead.")>]
        let of_list lists                          = ofList lists
        [<Obsolete("Use ofSeq instead.")>]
        let of_seq sources                         = ofSeq sources
        [<Obsolete("Use ofArray2D instead.")>]
        let inline of_array2D arrays               = ofArray2D arrays
        [<Obsolete("Use sparseOfArray2D instead.")>]
        let inline sparse_of_array2D arrays        = sparseOfArray2D arrays
        [<Obsolete("Use toArray2D instead.")>]
        let inline to_array2D matrix               = toArray2D matrix
        [<Obsolete("Use initDiagonal instead.")>]
        let init_diagonal vector                   = initDiagonal vector
        [<Obsolete("Use toDense instead.")>]
        let to_dense matrix                        = toDense matrix
        [<Obsolete("Use toSparse instead.")>]
        let to_sparse matrix                       = toSparse matrix
        [<Obsolete("Use initDense instead.")>]
        let init_dense lengthRow lengthCol source  = initDense lengthRow lengthCol source
        [<Obsolete("Use initSparse instead.")>]
        let init_sparse lengthRow lengthCol source = initSparse lengthRow lengthCol source
        let nonzeroEntries matrix                 = MS.nonZeroEntriesM matrix
        [<Obsolete("Use nonzeroEntries instead.")>]
        let nonzero_entries matrix                 = nonzeroEntries matrix


        // TM
        [<Obsolete("Use Matrix.mapRows instead.")>]
        let enumerateRowWise f (matrix:Matrix<'a>) =
            seq [
                for rowi=0 to matrix.NumRows-1 do
                yield f (seq [for coli=0 to matrix.NumCols-1 do yield matrix.[rowi,coli]])
            ]
        /// <summary>Applies mapping function along row axis</summary>
        /// <remarks></remarks>
        /// <param name="mapping"></param>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let mapRows (mapping: RowVector<'a> -> 'b) (matrix: Matrix<'a>) =
            Vector.Generic.init matrix.NumRows (fun rowi ->
                mapping (getRow matrix rowi)
            )

        /// <summary>Maps every matrix row using the position dependent function</summary>
        /// <remarks></remarks>
        /// <param name="mapping"></param>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let mapiRows (mapping: int -> RowVector<'a> -> 'b) (matrix: Matrix<'a>) =
            Vector.Generic.init matrix.NumRows (fun rowi ->
                mapping rowi (getRow matrix rowi)
            )

        // TM
        /// Applies function f along column axis
        [<Obsolete("Use Matrix.mapCols instead.")>]
        let enumerateColumnWise f (matrix:Matrix<'a>) =
            seq [
                for coli=0 to matrix.NumCols-1 do
                yield f (seq [for rowi=0 to matrix.NumRows-1 do yield matrix.[rowi,coli]])
            ]
        /// <summary>Applies mapping function along column axis</summary>
        /// <remarks></remarks>
        /// <param name="mapping"></param>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let mapCols (mapping: Vector<'a> -> 'b) (matrix: Matrix<'a>) =
            RowVector.Generic.init matrix.NumCols (fun coli ->
                 mapping (getCol matrix coli)
            )
        /// <summary>Maps every matrix column using the position dependant function</summary>
        /// <remarks></remarks>
        /// <param name="mapping"></param>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let mapiCols (mapping: int -> Vector<'a> -> 'b) (matrix: Matrix<'a>) =
            RowVector.Generic.init matrix.NumCols (fun coli ->
                mapping coli (getCol matrix coli)
            )
        /// <summary>Iterates the given Matrix row wise and places every element in a new vector with length n*m.</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let flattenRowWise (matrix: Matrix<'a>) =
            let tmp = FSharp.Stats.Vector.Generic.zeroCreate (matrix.NumRows*matrix.NumCols)
            for m = 0 to matrix.NumRows-1 do
                for n = 0 to matrix.NumCols-1 do
                    tmp.[m*matrix.NumCols+n] <- matrix.[m,n]
            tmp

        /// <summary>Iterates the given Matrix column wise and places every element in a new vector with length n*m.</summary>
        /// <remarks></remarks>
        /// <param name="matrix"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let flattenColWise (matrix: Matrix<'a>) =
            matrix.Transpose |> flattenRowWise
        

    module MG = Generic
    module DS = DoubleImpl
    module GU = GenericImpl
    module MS = SpecializedGenericImpl

    // Element type OpsData
    type elem = float

    // Accessors
    let get (a:Matrix<_>) i j   = MG.get a i j
    let set (a:Matrix<_>) i j x = MG.set a i j x
    // Creation
    ///returns a dense matrix with m rows and n rows, applying the init function with the two indices as arguments
    let init  m n f = DS.initDenseMatrixDS  m n f |> MS.dense
    ///returns a dense matrix with the inner lists of the input jagged list as its rows
    let ofJaggedList     xss   = DS.listDenseMatrixDS    xss |> MS.dense
    ///returns a dense matrix with the inner lists of the input jagged list as its columns
    let ofJaggedColList  xss   = DS.colListDenseMatrixDS    xss |> MS.dense
    ///returns a dense matrix with the inner sequences of the input jagged sequences as its rows
    let ofJaggedSeq      xss   = DS.seqDenseMatrixDS    xss |> MS.dense
    ///returns a dense matrix with the inner sequences of the input jagged sequence as its columns
    let ofJaggedColSeq   xss   = DS.colSeqDenseMatrixDS    xss |> MS.dense
    ///returns a dense matrix with the inner arrays of the input jagged array as its rows
    let ofJaggedArray    xss   = DS.arrayDenseMatrixDS    xss |> MS.dense
    /// <summary>returns a dense matrix with the inner arrays of the input jagged array as its columns</summary>
    /// <remarks></remarks>
    /// <param name="xss   "></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let ofJaggedColArray xss   = DS.colArrayDenseMatrixDS    xss |> MS.dense
    /// <summary>returns a dense matrix with the inner rowvectors of the input vector as its rows</summary>
    /// <remarks></remarks>
    /// <param name="rows"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let ofRows (rows: Vector<RowVector<_>>) = DS.seqDenseMatrixDS rows |> MS.dense
    /// <summary>returns a dense matrix with the inner vectors of the input rowvector as its columns</summary>
    /// <remarks></remarks>
    /// <param name="cols"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let ofCols (cols: RowVector<Vector<_>>) = DS.colSeqDenseMatrixDS cols |> MS.dense
    /// <summary>reads matrix from delimiter separated file</summary>
    /// <remarks></remarks>
    /// <param name="path"></param>
    /// <param name="separator"></param>
    /// <param name="removeHeaderRow"></param>
    /// <param name="removeHeaderCol"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let readCSV (path: string) (separator: char) (removeHeaderRow: bool) (removeHeaderCol: bool) = 
        IO.File.ReadAllLines(path)
        |> fun x -> 
            if removeHeaderRow then Array.tail x else x
        |> Array.map (fun x -> 
            let tmp = x.Split separator
            if removeHeaderCol then 
                tmp.[1..] |> Array.map float
            else tmp |> Array.map float
            )
        |> DS.arrayDenseMatrixDS 
        |> MS.dense

    ///
    let diag  (v:vector)   = MG.diag v
    ///
    //TO-DO: this should do something else as Matrix.diag. E.g. int -> (int -> float) -> Matrix<float>
    let initDiagonal  (v:vector)   = MG.diag v
    ///
    let constDiag  n x : matrix  = MG.constDiag n x
    ///
    //TO-DO: maybe rename to constCreate
    let create  m n x  = DS.constDenseMatrixDS  m n x |> MS.dense
    ///
    let ofScalar x     = DS.scalarDenseMatrixDS x |> MS.dense
    ///
    let ofArray2D arr : matrix = MG.ofArray2D arr
    /// <summary>Creates a sparse matrix based on the CSR format</summary>
    /// <remarks></remarks>
    /// <param name="arr"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let sparseOfArray2D arr : matrix = MG.sparseOfArray2D arr
    ///
    let toArray2D (m : matrix) = MG.toArray2D m
    ///
    let toJaggedArray (m: matrix) = MG.toJaggedArray m
    /// <summary>converts the matrix into an array of column arrays</summary>
    /// <remarks></remarks>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let toJaggedColArray (m: matrix) = MG.toJaggedColArray m
    /// <summary>converts the matrix into an seq of row seqs</summary>
    /// <remarks></remarks>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let toJaggedSeq (m: matrix) = MG.toJaggedSeq m
    /// <summary>converts the matrix into an seq of column seqs</summary>
    /// <remarks></remarks>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let toJaggedColSeq (m: matrix) = MG.toJaggedColSeq m
    ///
    let getDiagN  (a:matrix) n = MG.getDiagN a n
    ///
    let getDiag  (a:matrix) = MG.getDiag a

    // Operators
    /// <summary>Performs a element wise addition of matrices a and b (a+b).<br />Only usable if both matrices have the same dimensions.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let add (a:matrix) (b:matrix) = MS.addM a b
    /// <summary>Performs a element wise substraction of matrices a and b (a-b).<br />Only usable if both matrices have the same dimensions.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let sub (a:matrix) (b:matrix) = MS.subM a b
    /// <summary>Performs a left sided matrix multiplication of a and b (a*b).<br />Only usable if both matrices have the same dimensions.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mul (a:matrix) (b:matrix) = MS.mulM a b
    /// <summary>Performs a matrix multiplication m*n matrix a and the m*1 vector b (a*b).<br />Only usable if column number (n) of the matrix equals the row number (m) of the vector.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mulV (a:matrix) (b:vector) = MS.mulMV a b
    /// <summary>Performs a matrix multiplication of the 1*n rowvector a and the m*n matrix b (a*b).<br />Only usable if column number (n) of the vector equals the row number (m) of the matrix.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mulRV (a:rowvec) (b:matrix) = MS.mulRVM a b
    /// <summary>Performs a element wise multiplication of matrices a and b (a+b, Hadamard-Product).<br />Only usable if both matrices have the same dimensions.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let cptMul (a:matrix) (b:matrix) = MS.cptMulM a b
    /// <summary>Performs a element wise comparison of matrices a and b always preserving the greater value.<br />Only usable if both matrices have the same dimensions.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let cptMax (a:matrix) (b:matrix) = MS.cptMaxM a b
    /// <summary>Performs a element wise comparison of matrices a and b always preserving the smaller value.<br />Only usable if both matrices have the same dimensions.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let cptMin (a:matrix) (b:matrix) = MS.cptMinM a b
    /// <summary>Builds a new matrix where the elements are the result of multiplying every element of the given matrix with the given value</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let scale a (b:matrix) = MS.scaleM a b
    /// <summary>Scales matrix a by element wise mulitplication with minus 1.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let neg (a:matrix) = MS.negM a
    /// <summary>Computes the trace of matrix a by summing elements of the diagonal.<br />Only usable if matrices a is a square matrix (m*m).</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let trace (a:matrix) = MS.traceM a
    /// <summary>Returns the transpose of matrix a</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let transpose (a:matrix) = MG.transpose a
    /// <summary>Iterates the given Matrix row wise and applies function f element wise.<br />The iteration stops and returns false if an element fails the condition or true when the end of<br />the matrix is reached.</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let forall f (a:matrix) = MG.forall f a
    /// <summary>Iterates the given Matrix row wise and applies function f element wise.<br />The iteration stops and returns true if an element satisfies the condition or false when the end of<br />the matrix is reached.</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let exists f (a:matrix) = MG.exists f a
    /// <summary>Iterates the given Matrix row wise and applies function f element wise.<br />The iteration stops and returns false if an element fails the condition or true when the end of<br />the matrix is reached.</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let foralli f (a:matrix) = MG.foralli f a
    /// <summary>Iterates the given Matrix row wise and applies function f element wise.<br />The iteration stops and returns true if an element satisfies the condition or false when the end of<br />the matrix is reached.</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let existsi f (a:matrix) = MG.existsi f a
    let x = List.fold
    /// <summary>Builds a new matrix whose elements are the result of row wise applying the given function on each element of a.</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let map f (a:matrix) = MG.map f a
    /// <summary>Builds a new matrix whose elements are identical to the elements of a.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let copy (a:matrix) = MG.copy a
    /// <summary>Builds a new matrix whose elements are the result of row wise applying the given function on each element of a. The integer index<br />passed to the function indicates the index (from 0) the of the element being transformed.</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mapi f (a:matrix) : matrix = MG.mapi f a
    /// <summary>Applies a function f row wise to each element of the matrix, threading an accumulator argument through the computation.<br />The fold function takes the second argument, and applies the function f to it and the first element of the matrix.<br />Then, it feeds this result into the function f along with the second element, and so on. It returns the final result.<br />If the input function is f and the elements are i0...iN, then this function computes f (... (f s i0) i1 ...) iN.</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="z"></param>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let fold f z (a:matrix) = MG.fold f z a
    /// <summary>Applies a function f row wise to each element of the matrix, threading an accumulator argument through the computation.<br />The fold function takes the second argument, and applies the function f to it and the first element of the matrix.<br />Then, it feeds this result into the function f along with the second element, and so on. It returns the final result.<br />If the input function is f and the elements are i0...iN, then this function computes f (... (f s i0) i1 ...) iN.<br />The integers indicies passed to the function indicate row and column position (from 0) the of the element being transformed.</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="z"></param>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let foldi f z (a:matrix) = MG.foldi f z a
    /// <summary>Transforms the matrix a to a dense matrix representation</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let toDense (a:matrix) = MG.toDense a
    /// <summary>Transforms the matrix a to a sparse matrix representation</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let toSparse (a:matrix) = MG.toSparse a
    /// <summary>Creates a dense matrix with i rows and j columns. All values are initialized to the value of a.</summary>
    /// <remarks></remarks>
    /// <param name="i"></param>
    /// <param name="j"></param>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let initDense i j a : matrix = MG.initDense i j a
    /// <summary>Creates a sparse matrix with i rows and j columns. All values are initialized to the value of a.</summary>
    /// <remarks></remarks>
    /// <param name="i"></param>
    /// <param name="j"></param>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let initSparse i j a : matrix = MG.initSparse i j a
    /// <summary>Iterates the m*n matrix a row wise and returns a list of tuples (mi,ni,v) containing non zero elements of a<br />and their row (m) and column (n) indicies.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let nonzero_entries (a:matrix) = MG.nonzeroEntries a
    /// <summary>Creates a dense matrix with i rows and j columns. All values are initialized to yero (0.).</summary>
    /// <remarks></remarks>
    /// <param name="m"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let zero m n = DS.zeroDenseMatrixDS m n |> MS.dense
    /// <summary>Creates a dense identiy m*m matrix. A identity matrix is always squared and the elements are set to zero exept elements<br />on the diagonal, which are set to 1.<br />e.g.<br />[[1.;0.;0.]<br /> [0.;1.;0.]<br /> [0.;0.;1.]]</summary>
    /// <remarks></remarks>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let identity m : matrix = MG.identity m
    /// <summary>Creates a dense matrix with i rows and j columns. All values are initialized to one (1.).</summary>
    /// <remarks></remarks>
    /// <param name="m"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let ones m n = create m n 1.0
    /// <summary>Returns row of index i of matrix a</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="i"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getRow (a:matrix) i = MG.getRow a i
    /// <summary>Replaces row of index i of matrix a with values of vector v, if vector length matches rowsize</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="i"></param>
    /// <param name="v"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let setRow (a:Matrix<_>) i (v:Vector<_>) = MG.setRow a i v
    /// <summary>Returns col of index j of matrix a</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="j"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getCol (a:matrix) j = MG.getCol a j
    /// <summary>Replaces column of index j of matrix a with values of vector v, if vector length matches columnsize</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="j"></param>
    /// <param name="v"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let setCol (a:Matrix<_>) j (v:Vector<_>) = MG.setCol a j v
    /// <summary>Accesses the m*n matrix a and returns a total of j2 Columns starting from column index j1. The Result is a new<br />m*j2 matrix.<br />Only usable if (j1+j2-1) does not exceed n.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="j1"></param>
    /// <param name="j2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getCols (a:matrix) j1 j2 = MG.getCols a j1 j2
    /// <summary>Accesses the m*n matrix a and returns a total of i2 rows starting from row index i1. The Result is a new<br />i2*n matrix.<br />Only usable if (i1+i2-1) does not exceed m.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="i1"></param>
    /// <param name="i2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getRows (a:matrix) i1 i2 = MG.getRows a i1 i2

    let countBy f (a:matrix) =
        let n = a.NumCols * a.NumRows
        let (r,c) = a.Dimensions
        let rec loop ir ic acc =
            if ir = r then
                if ic = c then
                    [true,acc;false,n - acc]
                else loop ir (ic+1) (if f a.[ir,ic] then acc + 1 else acc)
            else
                if ic = c then
                    loop (ir+1) 0 acc
                else loop (ir+1) (ic+1) (if f a.[ir,ic] then acc + 1 else acc)
        loop 0 0 0

    /// <summary>Accesses the m*n matrix a and returns a total of i2 rows and j2 columns starting from row index i1 and colum index j1. The Result is a new<br />i2*j2 matrix.<br />Only usable if (i1+i2-1) does not exceed m and (j1+j2-1) does not exceed n.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="i1"></param>
    /// <param name="j1"></param>
    /// <param name="i2"></param>
    /// <param name="j2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getRegion (a:matrix) i1 j1 i2 j2 = MG.getRegion a i1 j1 i2 j2
    let rowRange (a:Matrix<_>) = (0,a.NumRows - 1)
    let colRange (a:Matrix<_>) = (0,a.NumCols - 1)
    let wholeRegion a = (colRange a, rowRange a)
    let foldByRow f (z:Vector<'T>) (a:Matrix<_>) =
        colRange a |> GU.foldR (fun z j -> MS.mapiV (fun i z -> f z (get a i j)) z) z
    let foldByCol f (z:RowVector<'T>) (a:Matrix<_>) =
        rowRange a |> GU.foldR (fun z i -> MS.mapiRV (fun j z -> f z (get a i j)) z) z
    let foldRow f (z:'T) (a:Matrix<_>) i =
        colRange a |> GU.foldR (fun (z:'T) j -> f z (get a i j)) z
    let foldCol f (z:'T) (a:Matrix<_>) j =
        rowRange a |> GU.foldR (fun (z:'T) i -> f z (get a i j)) z
    let sum (a:matrix)  = MS.sumM a
    let prod (a:matrix)  = MS.prodM a
    let norm  (a:matrix) = MS.normM  a
    let dot (a:matrix) b = MS.dotM a b
    let cptPow  a y = map (fun x -> x ** y) a
    // Functions that only make sense on this type
    let randomize v = map (fun vij -> MRandom.float vij) v      (* res_ij = random [0,vij] values *)
    let ofRowVector x : matrix = MS.rowvecM x
    let ofVector    x : matrix = MS.vectorM x
    let toVector    x : vector = MS.toVectorM x
    let toRowVector x : rowvec = MS.toRowVectorM x
    let toScalar    x : float  = MS.toScalarM x
    let inplaceAdd  (a:matrix) b = MS.inplaceAddM a b
    let inplaceSub  (a:matrix) b = MS.inplaceSubM a b
    // Mutation
    let inplace_assign  f (a:matrix) = MG.inplace_assign f a
    let inplace_mapi  f (a:matrix) = MG.inplace_mapi f a
    let inplace_cptMul (a:matrix) b = MS.inplaceCptMulM a b
    let inplace_scale  a (b:matrix) = MS.inplaceScaleM a b
    [<Obsolete("Use inplaceAdd instead.")>]
    let inplace_add  a b = inplaceAdd a b
    [<Obsolete("Use inplaceSub instead.")>]
    let inplace_sub  a b = inplaceSub a b
    [<Obsolete("Use ofRowVector instead.")>]
    let of_rowvec x = ofRowVector x
    [<Obsolete("Use ofVector instead.")>]
    let of_vector x = ofVector x
    [<Obsolete("Use toVector instead.")>]
    let to_vector x = toVector x
    [<Obsolete("Use toRowVector instead.")>]
    let to_rowvec x = toRowVector x
    [<Obsolete("Use toScalar instead.")>]
    let to_scalar x = toScalar x
    [<Obsolete("Use ofArray2D instead.")>]
    let inline of_array2D arr  = ofArray2D arr
    [<Obsolete("Use sparseOfArray2D instead.")>]
    let inline sparse_of_array2D arr = sparseOfArray2D arr
    [<Obsolete("Use toArray2D instead.")>]
    let inline to_array2D m = toArray2D m
    [<Obsolete("Use ofJaggedList instead.")>]
    let of_list    xss   = ofJaggedList xss
    [<Obsolete("Use ofJaggedSeq instead.")>]
    let of_seq     xss   = ofJaggedSeq xss
    [<Obsolete("Use initDiagonal instead.")>]
    let init_diagonal v   = initDiagonal   v
    [<Obsolete("Use ofScalar instead.")>]
    let of_scalar x     = ofScalar x
    [<Obsolete("Use toDense instead.")>]
    let to_dense x = toDense x
    [<Obsolete("Use toSparse instead.")>]
    let to_sparse x = toSparse x
    [<Obsolete("Use initDense instead.")>]
    let init_dense i j a = initDense i j a
    [<Obsolete("Use initSparse instead.")>]
    let init_sparse i j a = initSparse i j a

    //----------------------------------------------------------------------------
    // Stats

    /// <summary>Returns upper triangular Matrix by setting all values beneath the diagonal to Zero.<br />Warning: triangular matrices can only be computed for square input matrices.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getUpperTriangular (a:Matrix<float>) =
        let nA = a.NumCols
        let mA = a.NumRows
        if nA<>mA then invalidArg "a" "expected a square matrix";
        else
        a
        |> mapi (fun n m x -> if n > m then 0. else x )

    /// <summary>Returns lower triangular Matrix by setting all values beneath the diagonal to Zero.<br />Warning: triangular matrices can only be computed for square input matrices.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getLowerTriangular (a:Matrix<float>)  =
        let nA = a.NumCols
        let mA = a.NumRows
        if nA<>mA then invalidArg "a" "expected a square matrix";
        else
        a
        |> mapi (fun n m x -> if n < m then 0. else x )

    /// <summary>Returns diagonal matrix by setting all values beneath and above the diagonal to Zero.<br />Warning: diagonal matrices can only be computed for square input matrices.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let toDiagonal (a:Matrix<float>) =
        getDiag a
        |> diag

    /// <summary>Computes the row wise sums of a Matrix</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let sumRows (a:matrix) =
        a
        |> foldByRow (fun acc r -> acc + r ) (Vector.zeroCreate a.NumRows)

    /// <summary>Computes the column wise sums of a Matrix</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let sumColumns (a:matrix) =
        a
        |> foldByCol (fun acc r -> acc + r ) (RowVector.zero a.NumCols)

    /// <summary>Computes the row wise mean of a Matrix</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let meanRowWise (a:matrix) =
        a
        |> sumRows
        |> Vector.map (fun sum -> sum / (a.NumCols |> float))

    /// <summary>Computes the Column wise mean of a Matrix</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let meanColumnWise (a:matrix) =
        a
        |> sumColumns
        |> RowVector.map (fun sum -> sum / (a.NumRows |> float))
    
    ///Computes mean in the specified orientation
    /// <summary>orientation - "RowWise" or "ColWise"</summary>
    /// <remarks></remarks>
    /// <param name="orientation"></param>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let meanAsSeq (orientation:Orientation) (a:matrix) = 
        match orientation with
        | RowWise -> meanRowWise a    |> seq
        | ColWise -> meanColumnWise a |> seq

    /// computes the column specific covariance matrix of a data matrix as described at:
    // http://stattrek.com/matrix-algebra/covariance-matrix.aspx
    let columnCovarianceMatrixOf df (dataMatrix:Matrix<float>) =
        /// Step 1:<br />contains the deviation scores for the data matrix
        let devMatrix =
            let ident = ones dataMatrix.NumRows dataMatrix.NumRows
            dataMatrix - (ident * dataMatrix |> map (fun elem -> elem / float dataMatrix.NumRows))
        /// Step 2:<br />Compute devMatrix' * devMatrix, the k x k deviation sums of squares and cross products matrix for x.
        let devMTdevM =
            devMatrix.Transpose * devMatrix
        /// Step 3:<br />Then, divide each term in the deviation sums of squares and cross product matrix by n to create the variance-covariance matrix. That is:
        devMTdevM |> map (fun elem -> elem / (float df))

    /// <summary>computes the column specific population covariance matrix of a data matrix</summary>
    /// <remarks></remarks>
    /// <param name="dataMatrix"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let columnPopulationCovarianceMatrixOf (dataMatrix:Matrix<float>) =
        columnCovarianceMatrixOf dataMatrix.NumRows dataMatrix

    /// <summary>computes the column specific sample covariance matrix of a data matrix</summary>
    /// <remarks></remarks>
    /// <param name="dataMatrix"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let columnSampleCovarianceMatrixOf (dataMatrix:Matrix<float>) =
        columnCovarianceMatrixOf (dataMatrix.NumRows-1) dataMatrix

    /// <summary>computes the row specific population covariance matrix of a data matrix</summary>
    /// <remarks></remarks>
    /// <param name="dataMatrix"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let rowPopulationCovarianceMatrixOf (dataMatrix:Matrix<float>) =
        columnCovarianceMatrixOf dataMatrix.Transpose.NumRows dataMatrix.Transpose

    /// <summary>computes the row specific sample covariance matrix of a data matrix</summary>
    /// <remarks></remarks>
    /// <param name="dataMatrix"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let rowSampleCovarianceMatrixOf (dataMatrix:Matrix<float>) =
        columnCovarianceMatrixOf (dataMatrix.Transpose.NumRows-1) dataMatrix.Transpose

    /// <summary>computes the orientation and dataSource specific covariance matrix of a dataMatrix\<br />dataSource - "Sample" or "Population". \<br />orientation - "RowWise" or "ColWise" </summary>
    /// <remarks></remarks>
    /// <param name="dataSource"></param>
    /// <param name="orientation"></param>
    /// <param name="dataMatrix"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let covarianceMatrixOf (dataSource:DataSource) (orientation:Orientation) (dataMatrix:matrix) :matrix =
        match dataSource with
        |Sample ->
            match orientation with
            |RowWise ->rowSampleCovarianceMatrixOf dataMatrix
            |ColWise ->columnSampleCovarianceMatrixOf dataMatrix
        |Population ->
            match orientation with
            |RowWise ->rowPopulationCovarianceMatrixOf dataMatrix
            |ColWise ->columnPopulationCovarianceMatrixOf dataMatrix
    //----------------------------------------------------------------------------

    /// <summary>Applies function f along row axis</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mapRows f (m:matrix) = Generic.mapRows f m
    /// Applies function f along row axis
    [<Obsolete("Use Matrix.mapRows instead")>]
    let enumerateRowWise f (m:matrix) = Generic.mapRows f m

    /// <summary>Maps every matrix row using the position dependant function</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mapiRows (f: int -> rowvec -> 'b) (m:matrix) = Generic.mapiRows f m
    
    /// <summary>Applies function f along column axis</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mapCols f (m:matrix) = Generic.mapCols f m
    /// Applies function f along column axis
    [<Obsolete("Use Matrix.mapCols instead")>]
    let enumerateColumnWise f (m:matrix) = Generic.mapCols f m

    /// <summary>Maps every matrix column using the position dependant function</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mapiCols (f: int -> vector -> 'b) (m:matrix) = Generic.mapiCols f m

    /// <summary>Iterates the given Matrix row wise and places every element in a new vector with length n*m.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let flattenRowWise (a: matrix) = Generic.flattenRowWise a

    /// <summary>Iterates the given Matrix column wise and places every element in a new vector with length n*m.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let flattenColWise (a: matrix) = Generic.flattenColWise a

    /// <summary>Removes a row at a given index</summary>
    /// <remarks></remarks>
    /// <param name="index"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let removeRowAt (index:int) (m:Matrix<'T>) : Matrix<'T> =
        let nRows,nCols = m.Dimensions
        //let nm = Matrix.Generic.zero (nRows-1) nCols
        let nm = MG.zeroCreate (nRows-1) nCols
        let rec loop nRowI rowI =
            if rowI < 0 then
                nm
            else
                if rowI <> index then
                    for colI=0 to nCols-1 do
                        nm.[nRowI,colI] <- m.[rowI,colI]
                    loop (nRowI-1) (rowI-1)
                else
                    loop (nRowI) (rowI-1)

        loop (nRows-2) (nRows-1)

    /// <summary>
    /// <summary>Returns a matrix without the rows for which the given predicate returns false<br /></summary><br /><param name="rowPredicate">The predicate function based on which the rows should be filtered. The resulting matrix will only contain rows for which this function returns true </param><br /><param name="m">The matrix to filter rows from</param></summary>
    /// <remarks></remarks>
    /// <param name="rowPredicate"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let filterRows (rowPredicate: (RowVector<'T> -> bool)) (m:Matrix<'T>) : Matrix<'T> =
        let validRows =
            [|
                for rowIndex in 0..m.NumRows - 1 do
                    let row = MG.getRow m rowIndex
                    if rowPredicate row then yield rowIndex
            |]
        MG.init validRows.Length m.NumCols (fun r c ->
            m.[validRows.[r],c]
        )

    /// <summary>
    /// <summary>Returns a matrix without the cols for which the given predicate returns false<br /></summary><br /><param name="colPredicate">The predicate function based on which the cols should be filtered. The resulting matrix will only contain rows for which this function returns true </param><br /><param name="m">The matrix to filter cols from</param></summary>
    /// <remarks></remarks>
    /// <param name="colPredicate"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let filterCols (colPredicate: (Vector<'T> -> bool)) (m:Matrix<'T>) : Matrix<'T> =
        let validCols =
            [|
                for colIndex in 0..m.NumCols - 1 do
                    let col = MG.getCol m colIndex
                    if colPredicate col then yield colIndex
            |]
        MG.init m.NumRows validCols.Length (fun r c ->
            m.[r,validCols.[c]]
        )

    /// <summary>Removes a column at a given index</summary>
    /// <remarks></remarks>
    /// <param name="index"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let removeColAt index (m:Matrix<_>) =
        let nRows,nCols = m.Dimensions
        //let nm = Matrix.Generic.zero nRows (nCols-1)
        let nm = MG.zeroCreate nRows (nCols-1)
        let rec loop nColI colI =
            if nColI < 0 then
                nm
            else
                if colI <> index then
                    for rowI=0 to nRows-1 do
                        nm.[rowI,nColI] <- m.[rowI,colI]
                    loop (nColI-1) (colI-1)
                else
                    loop (nColI) (colI-1)

        loop (nRows-2) (nRows-1)

    /// <summary>Splits a matrix along row direction according to given indices. Returns (matrix including rows according to indices, rest)</summary>
    /// <remarks></remarks>
    /// <param name="indices"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let splitRows (indices:int[]) (m:Matrix<_>) =

        let nRows,nCols = m.Dimensions
        //let nm  = Matrix.Generic.zero (nRows-indices.Length) nCols
        //let nmi = Matrix.Generic.zero indices.Length nCols
        let nm  = MG.zeroCreate (nRows-indices.Length) nCols
        let nmi = MG.zeroCreate indices.Length nCols
        indices |> Array.sortInPlace
        let rec loop nRowI nRowIi rowI =
            match rowI with
            | i as rowI when rowI < 0 -> nmi,nm
            | i as rowI when nRowIi >= 0 && rowI = indices.[nRowIi] ->
                for colI=0 to nCols-1 do
                    nmi.[nRowIi,colI] <- m.[rowI,colI]
                loop (nRowI) (nRowIi-1) (rowI-1)
            | _ -> //i as rowI when rowI <> indices.[ii] ->
                for colI=0 to nCols-1 do
                    nm.[nRowI,colI] <- m.[rowI,colI]
                loop (nRowI-1) (nRowIi) (rowI-1)

        loop (nRows-1-indices.Length) (indices.Length-1) (nRows-1)

    /// <summary>Splits a matrix along column direction according to given indices. Returns (matrix including cols according to indices, rest)</summary>
    /// <remarks></remarks>
    /// <param name="indices"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let splitCols (indices:int[]) (m:Matrix<_>) =
        let nRows,nCols = m.Dimensions
        //let nm  = Matrix.Generic.zero nRows (nCols-indices.Length)
        //let nmi = Matrix.Generic.zero nRows indices.Length
        let nm  = MG.zeroCreate nRows (nCols-indices.Length)
        let nmi = MG.zeroCreate nRows indices.Length
        indices |> Array.sortInPlace
        let rec loop nColI nColIi colI =
            match colI with
            | i as colI when colI < 0 -> nmi,nm
            | i as colI when nColIi >= 0 && colI = indices.[nColIi] ->
                for rowI=0 to nRows-1 do
                    nmi.[rowI,nColIi] <- m.[rowI,colI]
                loop (nColI) (nColIi-1) (colI-1)
            | _ -> //i as rowI when rowI <> indices.[ii] ->
                for rowI=0 to nRows-1 do
                    nm.[rowI,nColI] <- m.[rowI,colI]
                loop (nColI-1) (nColIi) (colI-1)

        loop (nCols-1-indices.Length) (indices.Length-1) (nCols-1)


[<AutoOpen>]
module MatrixExtension =

    type Matrix<'T> with
        member x.ToArray2()        = Matrix.Generic.toArray2D x
        member x.ToArray2D()        = Matrix.Generic.toArray2D x

        /// Reads generic matrix from file. Requires a function to transform the input strings to the desired type
        static member ReadCSV(path,transformValues,?Separator,?RemoveHeaderRow,?RemoveHeaderCol): Matrix<'T> = 
            
            let sep = defaultArg Separator '\t'
            let rmr = defaultArg RemoveHeaderRow false
            let rmc = defaultArg RemoveHeaderCol false

            Matrix.Generic.readCSV path sep rmr rmc transformValues

#if FX_NO_DEBUG_DISPLAYS
#else
        [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif

        member x.NonZeroEntries    = Matrix.Generic.nonzeroEntries x
        member x.ToScalar()        = Matrix.Generic.toScalar x
        member x.ToRowVector()     = Matrix.Generic.toRowVector x
        member x.ToVector()        = Matrix.Generic.toVector x

#if FX_NO_DEBUG_DISPLAYS
#else
        [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif
        member x.Norm              = Matrix.Generic.norm x
        member x.Column(n)         = Matrix.Generic.getCol x n
        member x.Row(n)            = Matrix.Generic.getRow x n
        member x.Columns (i,ni)    = Matrix.Generic.getCols x i ni
        member x.Rows (j,nj)       = Matrix.Generic.getRows x j nj
        member x.Region(i,j,ni,nj) = Matrix.Generic.getRegion x i j ni nj
        member x.GetDiagonal(i)    = Matrix.Generic.getDiagN x i

#if FX_NO_DEBUG_DISPLAYS
#else
        [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif
        member x.Diagonal          = Matrix.Generic.getDiag x

        member x.Copy () = Matrix.Generic.copy x

//    [<AutoOpen>]
//    module MatrixTopLevelOperators =
//
//        let matrix ll = Matrix.ofSeq ll
//        let vector l  = Vector.ofSeq  l
//        let rowvec l  = RowVector.ofSeq l
