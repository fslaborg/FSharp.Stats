namespace FSharp.Stats

#nowarn "60" // implementations in augmentations
#nowarn "69" // implementations in augmentations

open System
open System.Collections

//----------------------------------------------------------------------------
// module Vector
//----------------------------------------------------------------------------
      
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//Basic vector operations
module Vector = 

    module Generic = 
        
        module OpsS = SpecializedGenericImpl
        /// <summary>Returns the value of the vector a at the given index i</summary>
        /// <param name="vector">vector to get value for</param>
        /// <param name="index">index in the vector to get value for</param>
        /// <returns>the value at index</returns>
        let get (vector:Vector<'T>) index  = vector.[index]
        /// <summary>Sets the value to the vector a at the given index</summary>
        /// <param name="vector">vector to set value for</param>
        /// <param name="index">index in the vector to set value for</param>
        /// <param name="value">value to set in the vector</param>
        let set (vector:Vector<'T>) index value  = vector.[index] <- value
        /// <summary>Returns length of vector v</summary>
        /// <remarks></remarks>
        /// <param name="vector"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let length (vector:Vector<'T>) = vector.Length
        /// <summary>Creates vector from list xss</summary>
        /// <remarks></remarks>
        /// <param name="list"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let ofList list = OpsS.listV list
        /// <summary>Creates vector from seq xss</summary>
        /// <remarks></remarks>
        /// <param name="source"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let ofSeq source = OpsS.seqV source
        /// <summary>Initializes vector with count members, based on function f</summary>
        /// <remarks></remarks>
        /// <param name="count"></param>
        /// <param name="initializer"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let init count initializer = OpsS.initV count initializer
        let initNumeric count f = OpsS.createNumericV count f
        let ofArray array = OpsS.arrayV array
        let toArray (vector:Vector<'T>) = Array.init vector.Length (get vector)
        /// <summary>Creates vector of length count and fills it with value</summary>
        /// <remarks></remarks>
        /// <param name="count"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let create count value = OpsS.constV count value
        /// <summary>Creates a vector of length count and fills it with zeros</summary>
        /// <remarks></remarks>
        /// <param name="count"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let zeroCreate count = OpsS.zeroV count
        /// <summary>Creates a vector of length count and fills it with ones</summary>
        /// <remarks></remarks>
        /// <param name="count"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let oneCreate count = OpsS.createNumericV count (fun ops _ -> ops.One)
        [<Obsolete("Use zeroCreate instead.")>]
        let zero count = OpsS.zeroV count
        [<Obsolete("Use oneCreate instead.")>]
        let ones count = OpsS.createNumericV count (fun ops _ -> ops.One)
        let ofScalar x = OpsS.scalarV x
        let add vector1 vector2 = OpsS.addV vector1 vector2
        let sub vector1 vector2 = OpsS.subV vector1 vector2
        let mulRVV vector1 vector2 = OpsS.mulRVV vector1 vector2
        let mulVRV vector1 vector2 = OpsS.mulVRV vector1 vector2
        let cptMul vector1 vector2 = OpsS.cptMulV vector1 vector2
        let cptMax vector1 vector2 = OpsS.cptMaxV vector1 vector2
        let cptMin vector1 vector2 = OpsS.cptMinV vector1 vector2
        let scale a b = OpsS.scaleV a b
        /// <summary>Dot product of the two vectors</summary>
        /// <remarks></remarks>
        /// <param name="vector1"></param>
        /// <param name="vector2"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let dot vector1 vector2 = OpsS.dotV vector1 vector2
        let neg vector = OpsS.negV vector 
        let transpose vector = OpsS.transV vector 
        let inplaceAdd vector1 vector2 = OpsS.inplaceAddV vector1 vector2
        let inplaceSub vector1 vector2 = OpsS.inplaceSubV vector1 vector2
        let inplaceCptMul vector1 vector2 = OpsS.inplaceCptMulV vector1 vector2
        let inplaceScale vector1 vector2 = OpsS.inplaceScaleV vector1 vector2
        [<Obsolete("Use inplaceCptMul instead.")>]
        let inplace_cptMul v1 v2 = OpsS.inplaceCptMulV v1 v2
        [<Obsolete("Use inplaceScale instead.")>]
        let inplace_scale v1 v2 = OpsS.inplaceScaleV v1 v2


        let exists predicate (vector:Vector<'T>) = OpsS.existsV  predicate vector
        let forall predicate (vector:Vector<'T>) = OpsS.forallV  predicate vector
        let existsi predicate (vector:Vector<'T>) = OpsS.existsiV  predicate vector
        let foralli predicate (vector:Vector<'T>) = OpsS.foralliV  predicate vector
        let map mapping vector = OpsS.mapV mapping vector 
        let map2 mapping vector1 vector2 = OpsS.map2V mapping vector1 vector2
        let map3 mapping vector1 vector2 vector3 = OpsS.map3V mapping vector1 vector2 vector3
        let zip vector1 vector2 = OpsS.zipV vector1 vector2
        let unzip vector = OpsS.unzipV vector
        let mapi mapping vector = OpsS.mapiV mapping vector
        let copy vector = OpsS.copyV vector
        let inplaceMap f v = OpsS.inplace_mapV f v
        let inplaceMapi f v = OpsS.inplace_mapiV f v
        [<Obsolete("Use inplaceMap instead.")>]
        let inplace_map  f a = OpsS.inplace_mapV f a
        [<Obsolete("Use inplaceMapi instead.")>]
        let inplace_mapi  f a = OpsS.inplace_mapiV f a
        let fold (folder:'State -> 'T -> 'State) (state:'State) vector = OpsS.foldV folder state vector
        let foldi (folder:int -> 'State -> 'T -> 'State) (state:'State) vector = OpsS.foldiV folder state vector
        let compare comparer vector = OpsS.compareV comparer vector
        let hash a = OpsS.hashV a
        let inplaceAssign f vector = OpsS.assignV f vector
        [<Obsolete("Use inplaceAssign instead.")>]
        let inplace_assign f a = OpsS.assignV f a
        /// <summary>Sum of all elements of the vector a</summary>
        /// <remarks></remarks>
        /// <param name="a"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let sum (a:Vector<_>) = let ops = a.ElementOps in fold (fun x y -> ops.Add(x,y)) ops.Zero a
        let prod (a:Vector<_>) = let ops = a.ElementOps in fold (fun x y -> ops.Multiply(x,y)) ops.One a

        let norm (a:Vector<_>) = 
            let normOps = GenericImpl.getNormOps a.ElementOps 
            sqrt (fold (fun x y -> x + normOps.Norm(y)**2.0) 0.0 a)

        [<Obsolete("Use ofList instead.")>]
        let of_list xss     = ofList xss
        [<Obsolete("Use ofSeq instead.")>]
        let of_seq xss      = ofSeq xss
        [<Obsolete("Use ofArr instead.")>]
        let of_array arr    = ofArray arr
        [<Obsolete("Use toArr instead.")>]
        let to_array v      = toArray v
        [<Obsolete("Use ofScalar instead.")>]
        let of_scalar x     = ofScalar x
        [<Obsolete("Use inplaceAdd instead.")>]
        let inplace_add a b = inplaceAdd a b
        [<Obsolete("Use inplaceSub instead.")>]
        let inplace_sub a b = inplaceSub a b

    module VG = Generic
    module VecDS = DoubleImpl
    module VecGU = GenericImpl
    /// <summary>Returns the value of the vector at the given index </summary>
    /// <remarks></remarks>
    /// <param name="vector"></param>
    /// <param name="index"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let get (vector:vector) index = VG.get vector index 
    /// <summary>Sets the value to the vector at the given index </summary>
    /// <remarks></remarks>
    /// <param name="vector"></param>
    /// <param name="index"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let set (vector:vector) index value = VG.set vector index value
    /// <summary>Returns length of vector</summary>
    /// <remarks></remarks>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let length (vector:vector) = VG.length vector
    /// <summary>Returns length of vector</summary>
    /// <remarks></remarks>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let nRows (vector:vector) = VG.length vector
    ///Returns length of vector
    [<Obsolete("Use length instead.")>]
    let nrows (vector:vector) = VG.length vector
    /// <summary>Initiates vector of length count and fills it by applying initializer function on indices</summary>
    /// <remarks></remarks>
    /// <param name="count"></param>
    /// <param name="initializer"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let init count initializer = VecDS.createVecDS count initializer
    /// <summary>Creates vector with values of array</summary>
    /// <remarks></remarks>
    /// <param name="array"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let ofArray array : vector = VG.ofArray array
    /// <summary>Creates array with values of vector</summary>
    /// <remarks></remarks>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let toArray (vector : vector) = VG.toArray vector

    type range = int * int
    let countR ((a,b) : range)   = (b-a)+1
    let idxR ((a,_) : range) i = a+i
    type rangef = float * float * float // start, skip, end
    let countRF ((a,d,b) : rangef)   = System.Convert.ToInt32((b-a)/d) + 1
    let countBy projection (vector:Vector<_>) =
        let n = vector.Length
        let rec loop i acc =
            if i = n then
                [true,acc;false,n - acc]
            else 
                if projection vector.[i] then 
                    loop (i+1) (acc+1)
                else loop (i+1) acc
        loop 0 0
    //let countRF ((a,d,b) : rangef)   = Float.to_int((b-a)/d) + 1
    let idxRF  ((a,d,b) : rangef) i = System.Math.Min (a + d * float(i),b)

    let range n1 n2 = let r = (n1,n2) in init (countR  r) (fun i -> float(idxR r i)) 

    let rangef a b c = let r = (a,b,c) in init (countRF r) (fun i -> idxRF r i)
    /// <summary>Creates vector with values of list </summary>
    /// <remarks></remarks>
    /// <param name="list"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let ofList list = VecDS.listVecDS list
    /// <summary>Creates vector with values of sequence</summary>
    /// <remarks></remarks>
    /// <param name="source"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let ofSeq source = VecDS.seqVecDS source
    ///Creates vector of length count and fills it with value 
    let create  count value  = VecDS.constVecDS count value
    /// <summary>Creates one dimensional vector of value</summary>
    /// <remarks></remarks>
    /// <param name="value"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let ofScalar value = VecDS.scalarVecDS value
    /// <summary>Builds a new vector whose elements are the results of adding the corresponding elements of the two vectors pairwise. The two input vectors must have the same lengths, otherwise ArgumentException is raised.</summary>
    /// <remarks></remarks>
    /// <param name="vector1"></param>
    /// <param name="vector2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let add vector1 vector2 = VecDS.addVecDS vector1 vector2
    /// <summary>Builds a new vector whose elements are the results of substracting the corresponding elements of vector2 from vector1. The two input vectors must have the same lengths, otherwise ArgumentException is raised.</summary>
    /// <remarks></remarks>
    /// <param name="vector1"></param>
    /// <param name="vector2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let sub vector1 vector2 = VecDS.subVecDS vector1 vector2
    let mulRVV vector1 vector2 = VecDS.mulRowVecVecDS vector1 vector2
    let mulVRV vector1 vector2 = VecDS.mulVecRowVecDS vector1 vector2
    /// <summary>Builds a new vector whose elements are the results of multiplying the corresponding elements of the given vectors. The two input vectors must have the same lengths, otherwise ArgumentException is raised.</summary>
    /// <remarks></remarks>
    /// <param name="vector1"></param>
    /// <param name="vector2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let cptMul vector1 vector2 = VecDS.cptMulVecDS vector1 vector2
    let cptMax vector1 vector2 = VecDS.cptMaxVecDS vector1 vector2
    let cptMin vector1 vector2 = VecDS.cptMinVecDS vector1 vector2
    /// <summary>Builds a new vector whose elements are the results of multiplying the given scalar with each of the elements of the vector.</summary>
    /// <remarks></remarks>
    /// <param name="scalar"></param>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let scale scalar vector = VecDS.scaleVecDS scalar vector
    /// <summary>Builds a new vector whose elements are the results of multiplying -1 with each of the elements of the vector.</summary>
    /// <remarks></remarks>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let neg vector = VecDS.negVecDS vector
    /// <summary>Dot product of the two vectors</summary>
    /// <remarks></remarks>
    /// <param name="vector1"></param>
    /// <param name="vector2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let dot vector1 vector2 = VecDS.dotVecDS vector1 vector2
    let transpose (vector:vector) = VG.transpose vector
    let exists predicate (vector:vector) = VG.exists predicate vector
    let forall predicate (vector:vector) = VG.forall predicate vector
    let existsi predicate (vector:vector) = VG.existsi predicate vector
    let foralli predicate (vector:vector) = VG.foralli predicate vector
    /// <summary>Builds a new vector whose elements are the results of applying the given function to each of the elements of the vector.</summary>
    /// <remarks></remarks>
    /// <param name="mapping"></param>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let map mapping (vector:vector) = VG.map mapping vector
    /// <summary>Builds a new vector whose elements are the results of applying the given function to the corresponding elements of the two vectors pairwise. The two input vectors must have the same lengths, otherwise ArgumentException is raised.</summary>
    /// <remarks></remarks>
    /// <param name="mapping"></param>
    /// <param name="vector1"></param>
    /// <param name="vector2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let map2 mapping (vector1:vector) (vector2:vector) = VG.map2 mapping vector1 vector2
    /// <summary>Builds a new vector whose elements are the results of applying the given function to the corresponding elements of the two vectors pairwise. The two input vectors must have the same lengths, otherwise ArgumentException is raised.</summary>
    /// <remarks></remarks>
    /// <param name="mapping"></param>
    /// <param name="vector1"></param>
    /// <param name="vector2"></param>
    /// <param name="vector3"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let map3 mapping (vector1:vector) (vector2:vector) (vector3:vector) = VG.map3 mapping vector1 vector2 vector3
    /// <summary>Builds a new vector that contains the elements of the given vector.</summary>
    /// <remarks></remarks>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let copy (vector:vector) = VG.copy vector
    /// <summary>Builds a new vector whose elements are the results of applying the given function to each of the elements of the vector and their corresponding index.</summary>
    /// <remarks></remarks>
    /// <param name="mapping"></param>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mapi mapping (vector:vector) : vector = VG.mapi mapping vector
    /// <summary>Applies a function to each element of the vector, threading an accumulator argument through the computation.</summary>
    /// <remarks></remarks>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let fold folder (state:'State) (vector:vector) = VG.fold folder state vector
    /// <summary>Applies a function to each element of the vector and their corresponding index, threading an accumulator argument through the computation.</summary>
    /// <remarks></remarks>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let foldi folder (state:'State) (vector:vector) = VG.foldi folder state vector
    /// <summary>Creates a vector of length count and fills it with zeros</summary>
    /// <remarks></remarks>
    /// <param name="count"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let zeroCreate count = create count 0.0
    /// <summary>Creates a vector of length count and fills it with ones</summary>
    /// <remarks></remarks>
    /// <param name="count"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let oneCreate count = create count 1.0
    [<Obsolete("Use zeroCreate instead.")>]
    let zero count = create count 0.0
    [<Obsolete("Use oneCreate instead.")>]
    let ones count = create count 1.0
    /// <summary>Sum of all elements of the vector</summary>
    /// <remarks></remarks>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let sum vector = VecDS.sumVecDS vector
    /// <summary>Product of all elements of the vector</summary>
    /// <remarks></remarks>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let prod vector = fold (fun x y -> x * y) 1.0 vector
    /// <summary>Euklidian norm of the vector</summary>
    /// <remarks></remarks>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let norm (vector:vector) = sqrt (fold (fun x y -> x + y * y) 0.0 vector) (* fixed *)
    /// <summary>Builds a new vector whose elements are the results of exponentiating each of the elements of the vector with n.</summary>
    /// <remarks></remarks>
    /// <param name="n"></param>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let toThePower n vector = map (fun x -> x ** n) vector
    [<Obsolete("Use toThePower instead.")>]
    let cptPow vector y = map (fun x -> x ** y) vector
    /// <summary>Applies the given function to each of the indexes of the vector. No new vector is created.</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inplaceAssign f (vector:vector) = VG.inplaceAssign f vector
    /// <summary>Applies the given function to each of the elements of the vector. No new vector is created.</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inplaceMap f (vector:vector) = VG.inplaceMap f vector
    /// <summary>Applies the given function to each of the elements of the vector and their corresponding index. No new vector is created.</summary>
    /// <remarks></remarks>
    /// <param name="f"></param>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inplaceMapi f (vector:vector) = VG.inplaceMapi f vector
    /// <summary>Add values of vector2 to values of vector1. Vector2 stays unchanged.</summary>
    /// <remarks></remarks>
    /// <param name="vector1"></param>
    /// <param name="vector2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inplaceAdd vector1 vector2 = VecDS.inplaceAddVecDS vector1 vector2
    /// <summary>Substract values of vector2 from values of vector1. Vector2 stays unchanged.</summary>
    /// <remarks></remarks>
    /// <param name="vector1"></param>
    /// <param name="vector2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inplaceSub vector1 vector2 = VecDS.inplaceSubVecDS vector1 vector2
    /// <summary>Multiply values of vector1 with values of vector2. Vector2 stays unchanged.</summary>
    /// <remarks></remarks>
    /// <param name="vector1"></param>
    /// <param name="vector2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inplaceCptMul vector1 vector2 = VecDS.inplaceCptMulVecDS vector1 vector2
    /// <summary>Multiply values of vector with scalar.</summary>
    /// <remarks></remarks>
    /// <param name="scalar"></param>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inplaceScale scalar vector = VecDS.inplaceScaleVecDS scalar vector
    ///Applies the given function to each of the indexes of the vector.
    /// <summary>Builds vector of Length 1 from value x</summary>
    /// <remarks></remarks>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let singleton x = ofScalar x
    [<Obsolete("Use inplaceAssign instead.")>]
    let inplace_assign  f (v:vector) = VG.inplaceAssign f v
    ///Applies the given function to each of the elements of the vector.
    [<Obsolete("Use inplaceMap instead.")>]
    let inplace_map f (v:vector) = VG.inplaceMap f v
    ///Applies the given function to each of the elements of the vector and their corresponding index.
    [<Obsolete("Use inplaceMapi instead.")>]
    let inplace_mapi f (v:vector) = VG.inplaceMapi f v
    ///Add values of vector v2 to values of vector v1. Vector v2 stays unchanged
    [<Obsolete("Use inplaceAdd instead.")>]
    let inplace_add v1 v2 = VecDS.inplaceAddVecDS v1 v2
    ///Substract values of vector v2 from values of vector v1. Vector v2 stays unchanged
    [<Obsolete("Use inplaceSub instead.")>]
    let inplace_sub v1 v2 = VecDS.inplaceSubVecDS v1 v2
    ///Multiply values of vector v1 with values of vector v2. Vector v2 stays unchanged.
    [<Obsolete("Use inplaceCptMul instead.")>]
    let inplace_cptMul v1 v2 = VecDS.inplaceCptMulVecDS v1 v2
    ///Multiply values of vector v1 with scalar.
    [<Obsolete("Use inplaceScale instead.")>]
    let inplace_scale x v = VecDS.inplaceScaleVecDS x v
    ///Builds vector from array
    [<Obsolete("Use ofArr instead.")>]
    let of_array arr   = ofArray arr
    ///Builds array from vector
    [<Obsolete("Use toArr instead.")>]
    let to_array v     = toArray v
    ///Builds vector from list
    [<Obsolete("Use ofList instead.")>]
    let of_list    xs  = ofList xs
    ///Builds vector from sequence
    [<Obsolete("Use ofSeq instead.")>]
    let of_seq    xs   = ofSeq xs
    ///Builds one dimensional vector from scalar
    [<Obsolete("Use ofScalar instead.")>]
    let of_scalar x    = ofScalar x


    
    //----------------------------------------------------------------------------
    // Stats
    //----------------------------------------------------------------------------
    
    /// <summary>Returns the raw data array without copy</summary>
    /// <remarks></remarks>
    /// <param name="vector"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let raw (vector:Vector<'T>) = vector.Values

    //
    let interval (items:Vector<'T>) =
        let rec loop index (minimum) (maximum) =
            if index < items.Length then
                let current = items.[index]
                loop (index+1) (min current minimum) (max current maximum)
            else
                Interval.CreateClosed<_> (minimum,maximum)
        //Init by fist value
        if items.Length > 1 then
            loop 1 items.[0] items.[0] 
        else
            Interval.Empty

    /// <summary>Computes the population mean (Normalized by N)            </summary>
    /// <remarks></remarks>
    /// <param name="items"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline mean (items:Vector<'T>) = 
        let ops  = items.ElementOps
        let zero = ops.Zero
        let one  = ops.One
        let rec loop i c acc =            
            if i < items.Length then
                loop (i+1) (ops.Add(c,one)) (ops.Add(acc,items.[i]))
            else
                acc / c
        loop 0 zero zero


    /// <summary>Computes the sample median</summary>
    /// <remarks></remarks>
    /// <param name="items"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline median (items:Vector<'T>) =
        items.Values |> Array.median
        
    /// <summary>Median absolute deviation (MAD)</summary>
    /// <remarks></remarks>
    /// <param name="items"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let medianAbsoluteDev (items : vector) =       
        items.Values |> Array.medianAbsoluteDev

    
    /// <summary>Returns SummaryStats of vector with N, mean, sum-of-squares, minimum and maximum</summary>
    /// <remarks></remarks>
    /// <param name="items"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline stats (items:Vector<'T>) =
        let zero = LanguagePrimitives.GenericZero< 'T > 
        let one = LanguagePrimitives.GenericOne< 'T >        
        
        let rec loop index n (minimum) (maximum) m1 m2 =
            if index < items.Length then            
                let current  = items.[index]
                let delta    = current - m1               
                let deltaN  = (delta / n)
                //let delta_n2 = deltaN * deltaN
                let m1'    = m1 + deltaN            
                let m2' = m2 + delta * deltaN * (n-one)
                loop (index+1) (n + one) (min current minimum) (max current maximum) m1' m2'
            else
                SummaryStats.createSummaryStats (n-one) m1 m2 minimum maximum
        //Init by fist value
        if items.Length > 1 then
            loop 0 one items.[0] items.[0] zero zero 
        else
            let uNan = zero / zero 
            SummaryStats.createSummaryStats zero uNan uNan uNan uNan


    /// <summary>Returns an estimator of the population covariance of two random variables v1 and v2 </summary>
    /// <remarks></remarks>
    /// <param name="v1"></param>
    /// <param name="v2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let covPopulation (v1:vector) (v2:vector) = 
        Seq.covPopulation v1 v2

    /// <summary>Returns the sample covariance of two random variables v1 and v2. (Bessel's correction by N-1) </summary>
    /// <remarks></remarks>
    /// <param name="v1"></param>
    /// <param name="v2"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let cov (v1:vector) (v2:vector) = 
        Seq.cov v1 v2

    /// <summary>calculates the sample means with a given number of replicates present in the sequence</summary>
    /// <remarks></remarks>
    /// <param name="rep"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getMeanOfReplicates rep (data:vector) =
        Seq.getMeanOfReplicates rep data
        |> ofSeq 

    /// <summary>calculates the sample standard deviations with a given number of replicates present in the sequence</summary>
    /// <remarks></remarks>
    /// <param name="rep"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getStDevOfReplicates rep (data:vector) =
        Seq.getStDevOfReplicates rep data
        |> ofSeq 

    /// <summary>calculates the coefficient of variation based on the sample standard deviations with a given number of replicates present in the sequence</summary>
    /// <remarks></remarks>
    /// <param name="rep"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getCvOfReplicates rep (data:vector) =
        Seq.getCvOfReplicates rep data
        |> ofSeq 

    /// <summary>Splits a vector according to given indices. Returns (vector including values according to indices, rest)</summary>
    /// <remarks></remarks>
    /// <param name="indices"></param>
    /// <param name="v"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let splitVector (indices:int[]) (v:Vector<_>) =
        let len = v.Length
        //let nv  = Vector.Generic.zero (len-indices.Length)
        //let nvi = Vector.Generic.zero indices.Length
        let nv  = VG.zeroCreate (len-indices.Length)
        let nvi = VG.zeroCreate indices.Length
        indices |> Array.sortInPlace
        let rec loop ni nii i =
            match i with
            | i when i < 0 -> nvi,nv
            | i when nii >= 0 && i = indices.[nii] ->            
                nvi.[nii] <- v.[i]                
                loop (ni) (nii-1) (i-1)                       
            | _ -> 
                nv.[ni] <- v.[i]
                loop (ni-1) (nii) (i-1) 
    
        loop (len-1-indices.Length) (indices.Length-1) (len-1)


    /// Module to compute common statistical measure on 
    module SummaryStats = 

        /// <summary>Returns SummaryStats of vector with N, mean, sum-of-squares, minimum and maximum</summary>
        /// <remarks></remarks>
        /// <param name="a"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let ofVector (a:Vector<'a>) = stats a









[<AutoOpen>]
module VectorExtension =

    type Vector<'T> with 
        member x.ToArray() = Vector.Generic.toArray x
        member x.Norm      = Vector.Generic.norm x
        member x.Copy ()   = Vector.Generic.copy x

        /// <summary>
        /// Creates an vector with values between a given interval
        /// </summary>
        /// <param name="start">start value (is included)</param>
        /// <param name="stop">end value (by default is included)</param>
        /// <param name="num">sets the number of elements in the vector. If not set, stepsize = 1.</param>
        /// <param name="IncludeEndpoint">If false, the vector does not contain the stop value</param>
        static member linspace(start:float,stop:float,num:int,?IncludeEndpoint:bool) : vector = 
        
            let includeEndpoint = defaultArg IncludeEndpoint true
 
            Seq.linspace(start,stop,num,includeEndpoint) |> Vector.ofSeq

        /// <summary>
        /// Creates a geometric vector of floats with values between a given interval.
        /// </summary>
        /// <param name="start">start value (is included)</param>
        /// <param name="stop">end value (by default is included)</param>
        /// <param name="num">sets the number of elements in the vector. Defaults to 50.</param>
        /// <param name="IncludeEndpoint">If false, the vector does not contain the stop value. Defaults to true.</param>
        static member geomspace(start:float,stop:float,num:int,?IncludeEndpoint:bool) : vector = 
            let includeEndpoint = defaultArg IncludeEndpoint true

            Seq.geomspace (start, stop ,num, includeEndpoint)
            |> Vector.ofSeq
   