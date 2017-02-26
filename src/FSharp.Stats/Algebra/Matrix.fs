namespace Microsoft.FSharp.Math

#nowarn "60" // implementations in augmentations
#nowarn "69" // implementations in augmentations

open Microsoft.FSharp.Math
open System
open System.Globalization
open System.Collections
open System.Collections.Generic
open System.Diagnostics

//----------------------------------------------------------------------------
// type Matrix<'T> augmentation
//--------------------------------------------------------------------------*)
type Matrix<'T> with
    static member ( +  )(a: Matrix<'T>,b) = SpecializedGenericImpl.addM a b
    static member ( -  )(a: Matrix<'T>,b) = SpecializedGenericImpl.subM a b
    static member ( *  )(a: Matrix<'T>,b) = SpecializedGenericImpl.mulM a b
    static member ( *  )(a: Matrix<'T>,b : Vector<'T>) = SpecializedGenericImpl.mulMV a b

    static member ( * )((m: Matrix<'T>),k : 'T) = SpecializedGenericImpl.scaleM k m

    static member ( .* )(a: Matrix<'T>,b) = SpecializedGenericImpl.cptMulM a b
    static member ( * )(k,m: Matrix<'T>) = SpecializedGenericImpl.scaleM k m
    static member ( ~- )(m: Matrix<'T>)     = SpecializedGenericImpl.negM m
    static member ( ~+ )(m: Matrix<'T>)     = m




//----------------------------------------------------------------------------
// type Vector<'T> augmentation
//--------------------------------------------------------------------------*)
    [<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
    type Vector<'T> with
        static member ( +  )(a: Vector<'T>,b) = SpecializedGenericImpl.addV a b
        static member ( -  )(a: Vector<'T>,b) = SpecializedGenericImpl.subV a b
        static member ( .* )(a: Vector<'T>,b) = SpecializedGenericImpl.cptMulV a b
        
        static member ( * )(k,m: Vector<'T>) = SpecializedGenericImpl.scaleV k m
        
        static member ( * )(a: Vector<'T>,b) = SpecializedGenericImpl.mulVRV a b
        
        static member ( * )(m: Vector<'T>,k) = SpecializedGenericImpl.scaleV k m
        
        static member ( ~- )(m: Vector<'T>)     = SpecializedGenericImpl.negV m
        static member ( ~+ )(m: Vector<'T>)     = m

        member m.GetSlice (start,finish) = 
            let start = match start with None -> 0 | Some v -> v 
            let finish = match finish with None -> m.NumRows - 1 | Some v -> v 
            SpecializedGenericImpl.getRegionV m (start,finish)

        member m.SetSlice (start,finish,vs:Vector<_>) = 
            let start = match start with None -> 0 | Some v -> v 
            let finish = match finish with None -> m.NumRows - 1 | Some v -> v 
            for i = start to finish  do 
                   m.[i] <- vs.[i-start]


        override m.ToString() = GenericImpl.showVecGU "vector" m

        member m.DebugDisplay = 
            let txt = GenericImpl.showVecGU "vector" m
            new System.Text.StringBuilder(txt)  // return an object with a ToString with the right value, rather than a string. (strings get shown using quotes)

        member m.StructuredDisplayAsArray =  Array.init m.NumRows (fun i -> m.[i])

        member m.Details = m.Values

        member m.Transpose = SpecializedGenericImpl.transV m
        
        member m.Permute (p:permutation) = SpecializedGenericImpl.permuteV p m
      
        interface System.IComparable with 
             member m.CompareTo(y:obj) = SpecializedGenericImpl.compareV LanguagePrimitives.GenericComparer m (y :?> Vector<'T>)
        
        interface IStructuralComparable with
            member m.CompareTo(y:obj,comp:System.Collections.IComparer) = SpecializedGenericImpl.compareV comp m (y :?> Vector<'T>)

        interface IStructuralEquatable with
            member x.GetHashCode(comp) = SpecializedGenericImpl.hashV comp x
            member x.Equals(yobj,comp) = 
                match yobj with 
                | :? Vector<'T> as v2 -> SpecializedGenericImpl.equalsV comp x v2
                | _ -> false

        override x.GetHashCode() = 
            SpecializedGenericImpl.hashV LanguagePrimitives.GenericEqualityComparer x

        override x.Equals(yobj) = 
            match yobj with 
            | :? Vector<'T> as v2 -> SpecializedGenericImpl.equalsV LanguagePrimitives.GenericEqualityComparer x v2
            | _ -> false

//----------------------------------------------------------------------------
// type RowVector<'T> augmentation
//--------------------------------------------------------------------------*)

    type RowVector<'T> with
        static member ( +  )(a: RowVector<'T>,b) = SpecializedGenericImpl.addRV a b
        static member ( -  )(a: RowVector<'T>,b) = SpecializedGenericImpl.subRV a b
        static member ( .* )(a: RowVector<'T>,b) = SpecializedGenericImpl.cptMulRV a b
        static member ( * )(k,v: RowVector<'T>) = SpecializedGenericImpl.scaleRV k v
        
        static member ( * )(a: RowVector<'T>,b: Matrix<'T>) = SpecializedGenericImpl.mulRVM a b
        static member ( * )(a: RowVector<'T>,b:Vector<'T>) = SpecializedGenericImpl.mulRVV a b
        static member ( * )(v: RowVector<'T>,k:'T) = SpecializedGenericImpl.scaleRV k v
        
        static member ( ~- )(v: RowVector<'T>)     = SpecializedGenericImpl.negRV v
        static member ( ~+ )(v: RowVector<'T>)     = v

        member m.GetSlice (start,finish) = 
            let start = match start with None -> 0 | Some v -> v
            let finish = match finish with None -> m.NumCols - 1 | Some v -> v 
            SpecializedGenericImpl.getRegionRV m (start,finish)

        member m.SetSlice (start,finish,vs:RowVector<_>) = 
            let start = match start with None -> 0 | Some v -> v 
            let finish = match finish with None -> m.NumCols - 1 | Some v -> v 
            for i = start to finish  do 
                   m.[i] <- vs.[i-start]

        override m.ToString() = GenericImpl.showRowVecGU "rowvec" m

        member m.DebugDisplay = 
            let txt = GenericImpl.showRowVecGU "rowvec" m
            new System.Text.StringBuilder(txt)  // return an object with a ToString with the right value, rather than a string. (strings get shown using quotes)

        member m.StructuredDisplayAsArray =  Array.init m.NumCols (fun i -> m.[i])

        member m.Details = m.Values

        member m.Transpose = SpecializedGenericImpl.transRV m
        
        member m.Permute (p:permutation) = SpecializedGenericImpl.permuteRV p m
      
        interface System.IComparable with 
            member m.CompareTo(y) = SpecializedGenericImpl.compareRV LanguagePrimitives.GenericComparer m (y :?> RowVector<'T>)
        
        interface IStructuralComparable with
            member m.CompareTo(y,comp) = SpecializedGenericImpl.compareRV comp m (y :?> RowVector<'T>)

        interface IStructuralEquatable with
            member x.GetHashCode(comp) = SpecializedGenericImpl.hashRV comp x
            member x.Equals(yobj,comp) = 
                match yobj with 
                | :? RowVector<'T> as rv2 -> SpecializedGenericImpl.equalsRV comp x rv2
                | _ -> false

        override x.GetHashCode() = 
            SpecializedGenericImpl.hashRV LanguagePrimitives.GenericEqualityComparer x

        override x.Equals(yobj) = 
            match yobj with 
            | :? RowVector<'T> as rv2 -> SpecializedGenericImpl.equalsRV LanguagePrimitives.GenericEqualityComparer x rv2
            | _ -> false

    type matrix = Matrix<float>
    type vector = Vector<float>
    type rowvec = RowVector<float>

    module MRandom = 
        let seed = 99
        let randomGen = new System.Random(seed)
        let float f = randomGen.NextDouble() * f 


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Matrix = begin
        
        module Generic = begin

            module MS = SpecializedGenericImpl

            // Accessors
            let get (a:Matrix<_>) i j   = a.[i,j]
            let set (a:Matrix<_>) i j x = a.[i,j] <- x
            
            // Creation
            let ofList    xss      = MS.listM  xss
            let ofSeq     xss      = MS.seqM  xss
            let init  m n f       = MS.initM  m n f
            let ofArray2D (arr: 'T[,])  : Matrix<'T>       = MS.arrayM arr
            let toArray2D (m:Matrix<_>) = Array2D.init m.NumRows m.NumCols (fun i j -> get m i j)
            let initNumeric m n f = MS.initNumericM m n f
            let zero m n            = MS.zeroM m n
            let identity m          = MS.identityM m
            let create  m n x       = MS.constM m n x

            let ofScalar   x        = MS.scalarM x

            let diag v              = MS.diagM v
            let initDiagonal v      = MS.diagM v
            let constDiag   n x     = MS.constDiagM n x
          
            // Operators
            let add a b = MS.addM a b
            let sub a b = MS.subM a b
            let mul a b = MS.mulM a b
            let mulRV a b = MS.mulRVM a b
            let mulV a b = MS.mulMV a b
            let cptMul a b = MS.cptMulM a b
            let cptMax a b = MS.cptMaxM a b
            let cptMin a b = MS.cptMinM a b
            let scale a b = MS.scaleM a b
            let dot a b = MS.dotM a b
            let neg a = MS.negM a 
            let trace a = MS.traceM a
            let sum a = MS.sumM a
            let prod a = MS.prodM a
            let norm a = MS.normM a
            let transpose a = MS.transM a
            let inplaceAdd a b = MS.inplaceAddM a b
            let inplaceSub a b = MS.inplaceSubM a b

            let exists  f a = MS.existsM  f a
            let forall  f a = MS.forallM  f a
            let existsi  f a = MS.existsiM  f a
            let foralli  f a = MS.foralliM  f a
            let map  f a = MS.mapM f a
            let copy a = MS.copyM a
            let mapi  f a = MS.mapiM f a
            let getDiagN  a n = MS.getDiagnM a n
            let getDiag  a = MS.getDiagnM a 0
            let toDense a = MS.toDenseM a 

            let initDense i j a = MS.initDenseM i j a 
            let initSparse i j a = MS.initSparseM i j a 

            let fold  f z a = MS.foldM f z a
            let foldi f z a = MS.foldiM f z a
          
            let compare a b = MS.compareM LanguagePrimitives.GenericComparer a b
            let hash a      = MS.hashM LanguagePrimitives.GenericEqualityComparer a
            let getRow    a i           = MS.getRowM a i
            let getCol    a j           = MS.selColM a j
            let getCols   a i1 i2       = MS.getColsM a (i1,i1+i2-1)
            let getRows   a j1 j2       = MS.getRowsM a (j1,j1+j2-1)
            let getRegion a i1 j1 i2 j2 = MS.getRegionM a (i1,i1+i2-1) (j1,j1+j2-1)
            
            let ofRowVector x = MS.rowvecM x
            let ofVector    x = MS.vectorM x
            let toVector    x = MS.toVectorM x
            let toRowVector x = MS.toRowVectorM x
            let toScalar    x = MS.toScalarM x

            let inplace_assign f a  = MS.inplaceAssignM  f a
            let inplace_cptMul a b = MS.inplaceCptMulM a b
            let inplace_scale a b = MS.inplaceScaleM a b
            let inplace_mapi  f a = MS.inplace_mapiM f a
            let of_rowvec x           = ofRowVector x
            let of_vector x           = ofVector x
            let to_vector x           = toVector x
            let to_rowvec x           = toRowVector x
            let to_scalar x           = toScalar x
            let inplace_add a b       = inplaceAdd a b
            let inplace_sub a b       = inplaceSub a b
            let of_scalar   x         = ofScalar x
            let of_list    xss        = ofList xss
            let of_seq     xss        = ofSeq xss
            let inline of_array2D arr = ofArray2D arr
            let inline to_array2D m   = toArray2D m
            let init_diagonal v       = initDiagonal v
            let to_dense a            = toDense a
            let init_dense i j a      = initDense i j a
            let init_sparse i j a     = initSparse i j a
            let nonzero_entries a     = MS.nonZeroEntriesM a 
         
        end

        module MG = Generic
        module DS = DoubleImpl
        module GU = GenericImpl
        module MS = SpecializedGenericImpl

        // Element type OpsData
        type elem = float

        // Accessors
        let get (a:matrix) i j   = MG.get a i j
        let set (a:matrix) i j x = MG.set a i j x
        
        // Creation
        let init  m n f = DS.initDenseMatrixDS  m n f |> MS.dense 
        let ofList    xss   = DS.listDenseMatrixDS    xss |> MS.dense
        let ofSeq     xss   = DS.seqDenseMatrixDS    xss |> MS.dense
        let diag  (v:vector)   = MG.diag v 
        let initDiagonal  (v:vector)   = MG.diag v 
        let constDiag  n x : matrix  = MG.constDiag n x 
        let create  m n x  = DS.constDenseMatrixDS  m n x |> MS.dense
        let ofScalar x     = DS.scalarDenseMatrixDS x |> MS.dense

        let ofArray2D arr : matrix = MG.ofArray2D arr
        let toArray2D (m : matrix) = MG.toArray2D m

        let getDiagN  (a:matrix) n = MG.getDiagN a n
        let getDiag  (a:matrix) = MG.getDiag a

        // Operators
        let add (a:matrix) (b:matrix) = MS.addM   a b
        let sub (a:matrix) (b:matrix) = MS.subM   a b
        let mul (a:matrix) (b:matrix) = MS.mulM   a b
        let mulV (a:matrix) (b:vector) = MS.mulMV   a b
        let mulRV (a:rowvec) (b:matrix) = MS.mulRVM   a b
        let cptMul (a:matrix) (b:matrix) = MS.cptMulM   a b
        let cptMax (a:matrix) (b:matrix) = MS.cptMaxM a b
        let cptMin (a:matrix) (b:matrix) = MS.cptMinM a b
        let scale a (b:matrix) = MS.scaleM   a b
        let neg (a:matrix)  = MS.negM a
        let trace (a:matrix)  = MS.traceM a
        let transpose  (a:matrix) = MG.transpose a
        let forall f (a:matrix) = MG.forall f a
        let exists  f (a:matrix) = MG.exists f a
        let foralli f (a:matrix) = MG.foralli f a
        let existsi  f (a:matrix) = MG.existsi f a
        let map  f (a:matrix) = MG.map f a
        let copy  (a:matrix) = MG.copy a
        let mapi  f (a:matrix) : matrix = MG.mapi f a
        let fold  f z (a:matrix) = MG.fold f z a
        let foldi  f z (a:matrix) = MG.foldi f z a

        let toDense (a:matrix) = MG.toDense a 
        let initDense i j a : matrix = MG.initDense i j a 
        let initSparse i j a : matrix = MG.initSparse i j a 
        let nonzero_entries (a:matrix) = MG.nonzero_entries a 

        let zero m n  = DS.zeroDenseMatrixDS m n |> MS.dense
        let identity m  : matrix = MG.identity m 
        
        let ones m n  = create m n 1.0
        
        let getRow (a:matrix) i      = MG.getRow a i
        let getCol (a:matrix) j      = MG.getCol a j
        let getCols (a:matrix) i1 i2    = MG.getCols a i1 i2
        let getRows (a:matrix) j1 j2    = MG.getRows a j1 j2
        let getRegion (a:matrix) i1 j1 i2 j2    = MG.getRegion a i1 j1 i2 j2

        let rowRange (a:Matrix<_>) = (0,a.NumRows - 1)
        let colRange (a:Matrix<_>) = (0,a.NumCols - 1)
        let wholeRegion a = (colRange a, rowRange a)
        
        let foldByRow f (z:Vector<'T>) (a:matrix) = 
          colRange a |> GU.foldR (fun z j -> MS.mapiV (fun i z -> f z (get a i j)) z) z
        let foldByCol f (z:RowVector<'T>) (a:matrix) = 
          rowRange a |> GU.foldR (fun z i -> MS.mapiRV (fun j z -> f z (get a i j)) z) z

        let foldRow f (z:'T) (a:matrix) i = 
          colRange a |> GU.foldR (fun (z:'T) j -> f z (get a i j)) z
        let foldCol f (z:'T) (a:matrix) j = 
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

        let inplace_add  a b = inplaceAdd a b
        let inplace_sub  a b = inplaceSub a b
        let of_rowvec x = ofRowVector x
        let of_vector x = ofVector x
        let to_vector x = toVector x
        let to_rowvec x = toRowVector x
        let to_scalar x = toScalar x
        let inline of_array2D arr  = ofArray2D arr
        let inline to_array2D m = toArray2D m
        let of_list    xss   = ofList xss
        let of_seq     xss   = ofSeq xss
        let init_diagonal v   = initDiagonal   v
        let of_scalar x     = ofScalar x
        let to_dense x = toDense x
        let init_dense i j a = initDense i j a
        let init_sparse i j a = initSparse i j a


    end


//----------------------------------------------------------------------------
// module Vector
//--------------------------------------------------------------------------*)
      
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Vector = 

        module Generic = 

            module OpsS = SpecializedGenericImpl

            let get (a:Vector<_>) i   = a.[i]
            let set (a:Vector<_>) i x = a.[i] <- x
            let length (v:Vector<_>) = v.Length
            let ofList    xss   = OpsS.listV xss
            let ofSeq    xss   = OpsS.seqV xss
            let init  m   f = OpsS.initV m f
            let initNumeric  m   f = OpsS.createNumericV m f
            let ofArray arr       = OpsS.arrayV arr
            let toArray (v:Vector<_>) = Array.init v.Length (get v)

            let create  m x   = OpsS.constV m x
            let zero n = OpsS.zeroV n
            let ones n = OpsS.createNumericV n (fun ops _ -> ops.One)
            let ofScalar   x = OpsS.scalarV x
            let add a b = OpsS.addV a b
            let sub a b = OpsS.subV a b
            let mulRVV a b = OpsS.mulRVV a b
            let mulVRV a b = OpsS.mulVRV a b
            let cptMul a b = OpsS.cptMulV a b
            let cptMax a b = OpsS.cptMaxV a b
            let cptMin a b = OpsS.cptMinV a b
            let scale a b = OpsS.scaleV a b
            let dot a b = OpsS.dotV a b
            let neg a = OpsS.negV a 
            let transpose a = OpsS.transV a 
            let inplaceAdd a b = OpsS.inplaceAddV a b
            let inplaceSub a b = OpsS.inplaceSubV a b
            let inplace_cptMul a b = OpsS.inplaceCptMulV a b
            let inplace_scale a b = OpsS.inplaceScaleV a b



            let exists  f a = OpsS.existsV  f a
            let forall  f a = OpsS.forallV  f a
            let existsi  f a = OpsS.existsiV  f a
            let foralli  f a = OpsS.foralliV  f a
            let map  f a = OpsS.mapV f a
            let mapi f a = OpsS.mapiV f a
            let copy a = OpsS.copyV a
            let inplace_mapi  f a = OpsS.inplace_mapiV f a
            let fold  f z a = OpsS.foldV f z a
            let foldi  f z a = OpsS.foldiV f z a
            let compare a b = OpsS.compareV a b
            let hash a = OpsS.hashV a
            let inplace_assign  f a = OpsS.assignV f a
            let sum  (a:Vector<_>) = let ops = a.ElementOps in fold (fun x y -> ops.Add(x,y)) ops.Zero a
            let prod (a:Vector<_>) = let ops = a.ElementOps in fold (fun x y -> ops.Multiply(x,y)) ops.One a
            let norm (a:Vector<_>) = 
                let normOps = GenericImpl.getNormOps a.ElementOps 
                sqrt (fold (fun x y -> x + normOps.Norm(y)**2.0) 0.0 a)

            let of_list    xss  = ofList xss
            let of_seq    xss   = ofSeq xss
            let of_array arr    = ofArray arr
            let to_array v      = toArray v
            let of_scalar   x   = ofScalar x
            let inplace_add a b = inplaceAdd a b
            let inplace_sub a b = inplaceSub a b

        module VG = Generic
        module VecDS = DoubleImpl
        module VecGU = GenericImpl

        let get (a:vector) j   = VG.get a j 
        let set (a:vector) j x = VG.set a j x
        let length (a:vector)     = VG.length a
        let nrows (a:vector)   = VG.length a
        let init  m   f = VecDS.createVecDS  m   f
        let ofArray arr : vector = VG.ofArray arr
        let toArray (m : vector) = VG.toArray m

        type range = int * int
        let countR ((a,b) : range)   = (b-a)+1
        let idxR    ((a,_) : range) i = a+i
        type rangef = float * float * float // start, skip, end
        let countRF ((a,d,b) : rangef)   = System.Convert.ToInt32((b-a)/d) + 1
        //let countRF ((a,d,b) : rangef)   = Float.to_int((b-a)/d) + 1
        let idxRF  ((a,d,b) : rangef) i = System.Math.Min (a + d * float(i),b)

        let range n1 n2    = let r = (n1,n2)   in init (countR  r) (fun i -> float(idxR r i)) 

        let rangef a b c  = let r = (a,b,c) in init (countRF r) (fun i -> idxRF r i)

        let ofList    xs    = VecDS.listVecDS    xs
        let ofSeq    xs    = VecDS.seqVecDS    xs
        let create  m   x  = VecDS.constVecDS  m   x
        let ofScalar x     = VecDS.scalarVecDS x
        let add a b = VecDS.addVecDS   a b
        let sub a b = VecDS.subVecDS   a b
        let mulRVV a b = VecDS.mulRowVecVecDS   a b
        let mulVRV a b = VecDS.mulVecRowVecDS   a b 
        let cptMul a b = VecDS.cptMulVecDS   a b
        let cptMax a b = VecDS.cptMaxVecDS a b
        let cptMin a b = VecDS.cptMinVecDS a b
        let scale a b = VecDS.scaleVecDS   a b
        let neg a  = VecDS.negVecDS a
        let dot a b = VecDS.dotVecDS a b
        let transpose  (a:vector) = VG.transpose a
        let exists  f (a:vector) = VG.exists f a
        let forall  f (a:vector) = VG.forall f a
        let existsi  f (a:vector) = VG.existsi f a
        let foralli  f (a:vector) = VG.foralli f a
        let map  f (a:vector) = VG.map f a
        let copy (a:vector) = VG.copy a
        let mapi  f (a:vector) : vector = VG.mapi f a
        let fold  f z (a:vector) = VG.fold f z a
        let foldi  f z (a:vector) = VG.foldi f z a
        let zero n = create n 0.0
        let ones n = create n 1.0
        let sum a  = VecDS.sumVecDS a
        let prod a   = fold      (fun x y -> x * y) 1.0 a
        let norm  (a:vector) = sqrt (fold (fun x y -> x + y * y) 0.0 a) (* fixed *)
        let cptPow  a y = map  (fun x -> x ** y) a
        let inplace_assign  f (a:vector) = VG.inplace_assign f a
        let inplace_mapi f (a:vector) = VG.inplace_mapi f a
        let inplace_add a b = VecDS.inplaceAddVecDS a b
        let inplace_sub a b = VecDS.inplaceSubVecDS a b
        let inplace_cptMul a b = VecDS.inplaceCptMulVecDS a b
        let inplace_scale a b = VecDS.inplaceScaleVecDS a b  

        let of_array arr   = ofArray arr
        let to_array m     = toArray m
        let of_list    xs  = ofList xs
        let of_seq    xs   = ofSeq xs
        let of_scalar x    = ofScalar x



//----------------------------------------------------------------------------
// module RowVector
//--------------------------------------------------------------------------*)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RowVector = 

        module Generic = 

            module OpsS = SpecializedGenericImpl

            let get (a:RowVector<_>) i          = a.[i]
            let set (a:RowVector<_>) i x        = a.[i] <- x
            let zero n           = OpsS.zeroRV n
            let length (v:RowVector<_>) = v.Length
            let init m   f       = OpsS.initRV m   f
            let create  m x      = OpsS.constRV m x
            let transpose a      = OpsS.transRV a
            let copy a           = OpsS.copyRV a
            let ofList a         = OpsS.listRV a
            let ofArray a        = OpsS.arrayRV a
            let ofSeq a          = OpsS.seqRV a
            let toArray m        = Array.init (length m) (get m)

            let of_list a        = ofList a
            let of_array a       = ofArray a
            let of_seq a         = ofSeq a
            let to_array m       = toArray m


        module RVG = Generic

        let get (a:rowvec) i   = RVG.get a i 
        let set (a:rowvec) i x = RVG.set a i x
        let length (a:rowvec)  = RVG.length a
        let ncols (a:rowvec)   = RVG.length a
        let ofArray arr : rowvec = RVG.ofArray arr
        let toArray (m : rowvec) = RVG.toArray m
        
        let init m   f : rowvec      = RVG.init m   f
        let create m   f : rowvec    = RVG.create m   f
        let zero n = create n 0.0
        let ofList x : rowvec       = RVG.ofList x
        let ofSeq x : rowvec       = RVG.ofSeq x
        let transpose x : vector     = RVG.transpose x
        let copy x : rowvec          = RVG.copy x

        let of_list x    = ofList x
        let of_seq x     = ofSeq x
        let of_array arr = ofArray arr
        let to_array m   = toArray m


    type Matrix<'T> with 
        member x.ToArray2()        = Matrix.Generic.toArray2D x
        member x.ToArray2D()        = Matrix.Generic.toArray2D x

#if FX_NO_DEBUG_DISPLAYS
#else
        [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif

        member x.NonZeroEntries    = Matrix.Generic.nonzero_entries x
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


    type Vector<'T> with 
        member x.ToArray() = Vector.Generic.toArray x
        member x.Norm      = Vector.Generic.norm x
        member x.Copy ()   = Vector.Generic.copy x


    type RowVector<'T> with 
        member x.ToArray() = RowVector.Generic.toArray x
        member x.Copy ()   = RowVector.Generic.copy x

    [<AutoOpen>]
    module MatrixTopLevelOperators = 

        let matrix ll = Matrix.ofSeq ll
        let vector l  = Vector.ofSeq  l
        let rowvec l  = RowVector.ofSeq l

