//namespace Microsoft.FSharp.Math // old namespace
namespace FSharp.Stats

#nowarn "60" // implementations in augmentations
#nowarn "69" // implementations in augmentations

//open Microsoft.FSharp.Math
open System
open System.Globalization
open System.Collections
open System.Collections.Generic
open System.Diagnostics


//----------------------------------------------------------------------------
// module Vector
//--------------------------------------------------------------------------*)
      
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
///Basic vector operations
module Vector = 

    module Generic = 
        
        module OpsS = SpecializedGenericImpl
        ///Returns the value of the vector a at the given index i
        let get (vector:Vector<'T>) index  = vector.[index]
        ///Sets the value x to the vector a at the given index i
        let set (vector:Vector<'T>) index value  = vector.[index] <- value
        ///Returns length of vector v
        let length (vector:Vector<'T>) = vector.Length
        ///Creates vector from list xss
        let ofList list = OpsS.listV list
        ///Creates vector from seq xss
        let ofSeq source = OpsS.seqV source
        ///Initializes vector with count members, based on function f
        let init count initializer = OpsS.initV count initializer
        let initNumeric count f = OpsS.createNumericV count f
        let ofArray array = OpsS.arrayV array
        let toArray (vector:Vector<'T>) = Array.init vector.Length (get vector)
        ///Creates vector of length count and fills it with value
        let create count value = OpsS.constV count value
        ///Creates a vector of length count and fills it with zeros
        let zeroCreate count = OpsS.zeroV count
        ///Creates a vector of length count and fills it with ones
        let oneCreate count = OpsS.createNumericV count (fun ops _ -> ops.One)
        [<Obsolete("Do not use. Use [zeroCreate] instead.")>]
        let zero count = OpsS.zeroV count
        [<Obsolete("Do not use. Use [oneCreate] instead.")>]
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
        ///Dot product of the two vectors
        let dot vector1 vector2 = OpsS.dotV vector1 vector2
        let neg vector = OpsS.negV vector 
        let transpose vector = OpsS.transV vector 
        let inplaceAdd vector1 vector2 = OpsS.inplaceAddV vector1 vector2
        let inplaceSub vector1 vector2 = OpsS.inplaceSubV vector1 vector2
        let inplaceCptMul vector1 vector2 = OpsS.inplaceCptMulV vector1 vector2
        let inplaceScale vector1 vector2 = OpsS.inplaceScaleV vector1 vector2
        [<Obsolete("Do not use. Use [inplaceCptMul] instead.")>]
        let inplace_cptMul v1 v2 = OpsS.inplaceCptMulV v1 v2
        [<Obsolete("Do not use. Use [inplaceScale] instead.")>]
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
        [<Obsolete("Do not use. Use [inplaceMap] instead.")>]
        let inplace_map  f a = OpsS.inplace_mapV f a
        [<Obsolete("Do not use. Use [inplaceMapi] instead.")>]
        let inplace_mapi  f a = OpsS.inplace_mapiV f a
        let fold (folder:'State -> 'T -> 'State) (state:'State) vector = OpsS.foldV folder state vector
        let foldi (folder:int -> 'State -> 'T -> 'State) (state:'State) vector = OpsS.foldiV folder state vector
        let compare comparer vector = OpsS.compareV comparer vector
        let hash a = OpsS.hashV a
        let inplaceAssign f vector = OpsS.assignV f vector
        [<Obsolete("Do not use. Use [inplaceAssign] instead.")>]
        let inplace_assign f a = OpsS.assignV f a
        ///Sum of all elements of the vector a
        let sum (a:Vector<_>) = let ops = a.ElementOps in fold (fun x y -> ops.Add(x,y)) ops.Zero a
        let prod (a:Vector<_>) = let ops = a.ElementOps in fold (fun x y -> ops.Multiply(x,y)) ops.One a

        let norm (a:Vector<_>) = 
            let normOps = GenericImpl.getNormOps a.ElementOps 
            sqrt (fold (fun x y -> x + normOps.Norm(y)**2.0) 0.0 a)

        [<Obsolete("Do not use. Use [ofList] instead.")>]
        let of_list    xss  = ofList xss
        [<Obsolete("Do not use. Use [ofSeq] instead.")>]
        let of_seq    xss   = ofSeq xss
        [<Obsolete("Do not use. Use [ofArr] instead.")>]
        let of_array arr    = ofArray arr
        [<Obsolete("Do not use. Use [toArr] instead.")>]
        let to_array v      = toArray v
        [<Obsolete("Do not use. Use [ofScalar] instead.")>]
        let of_scalar   x   = ofScalar x
        [<Obsolete("Do not use. Use [inplaceAdd] instead.")>]
        let inplace_add a b = inplaceAdd a b
        [<Obsolete("Do not use. Use [inplaceSub] instead.")>]
        let inplace_sub a b = inplaceSub a b

    module VG = Generic
    module VecDS = DoubleImpl
    module VecGU = GenericImpl
    ///Returns the value of the vector at the given index 
    let get (vector:vector) index = VG.get vector index 
    ///Sets the value to the vector at the given index 
    let set (vector:vector) index value = VG.set vector index value
    ///Returns length of vector
    let length (vector:vector) = VG.length vector
    ///Returns length of vector
    let nRows (vector:vector) = VG.length vector
    ///Returns length of vector
    [<Obsolete("Do not use. Use [length] instead.")>]
    let nrows (vector:vector) = VG.length vector
    ///Initiates vector of length count and fills it by applying initializer function on indices
    let init count initializer = VecDS.createVecDS count initializer
    ///Creates vector with values of array
    let ofArray array : vector = VG.ofArray array
    ///Creates array with values of vector
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
    ///Creates vector with values of list 
    let ofList list = VecDS.listVecDS list
    ///Creates vector with values of sequence
    let ofSeq source = VecDS.seqVecDS source
    ///Creates vector of length count and fills it with value 
    let create  count value  = VecDS.constVecDS count value
    ///Creates one dimensional vector of value
    let ofScalar value = VecDS.scalarVecDS value
    ///Builds a new vector whose elements are the results of adding the corresponding elements of the two vectors pairwise. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let add vector1 vector2 = VecDS.addVecDS vector1 vector2
    ///Builds a new vector whose elements are the results of substracting the corresponding elements of vector1 from vector2. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let sub vector1 vector2 = VecDS.subVecDS vector1 vector2
    let mulRVV vector1 vector2 = VecDS.mulRowVecVecDS vector1 vector2
    let mulVRV vector1 vector2 = VecDS.mulVecRowVecDS vector1 vector2
    ///Builds a new vector whose elements are the results of multiplying the corresponding elements of the given vectors. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let cptMul vector1 vector2 = VecDS.cptMulVecDS vector1 vector2
    let cptMax vector1 vector2 = VecDS.cptMaxVecDS vector1 vector2
    let cptMin vector1 vector2 = VecDS.cptMinVecDS vector1 vector2
    ///Builds a new vector whose elements are the results of multiplying the given scalar with each of the elements of the vector.
    let scale scalar vector = VecDS.scaleVecDS scalar vector
    ///Builds a new vector whose elements are the results of multiplying -1 with each of the elements of the vector.
    let neg vector = VecDS.negVecDS vector
    ///Dot product of the two vectors
    let dot vector1 vector2 = VecDS.dotVecDS vector1 vector2
    let transpose (vector:vector) = VG.transpose vector
    let exists predicate (vector:vector) = VG.exists predicate vector
    let forall predicate (vector:vector) = VG.forall predicate vector
    let existsi predicate (vector:vector) = VG.existsi predicate vector
    let foralli predicate (vector:vector) = VG.foralli predicate vector
    ///Builds a new vector whose elements are the results of applying the given function to each of the elements of the vector.
    let map mapping (vector:vector) = VG.map mapping vector
    ///Builds a new vector whose elements are the results of applying the given function to the corresponding elements of the two vectors pairwise. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let map2 mapping (vector1:vector) (vector2:vector) = VG.map2 mapping vector1 vector2
    ///Builds a new vector whose elements are the results of applying the given function to the corresponding elements of the two vectors pairwise. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let map3 mapping (vector1:vector) (vector2:vector) (vector3:vector) = VG.map3 mapping vector1 vector2 vector3
    ///Builds a new vector that contains the elements of the given vector.
    let copy (vector:vector) = VG.copy vector
    ///Builds a new vector whose elements are the results of applying the given function to each of the elements of the vector and their corresponding index.
    let mapi mapping (vector:vector) : vector = VG.mapi mapping vector
    ///Applies a function to each element of the vector, threading an accumulator argument through the computation.
    let fold folder (state:'State) (vector:vector) = VG.fold folder state vector
    ///Applies a function to each element of the vector and their corresponding index, threading an accumulator argument through the computation.
    let foldi folder (state:'State) (vector:vector) = VG.foldi folder state vector
    ///Creates a vector of length count and fills it with zeros
    let zeroCreate count = create count 0.0
    ///Creates a vector of length count and fills it with ones
    let oneCreate count = create count 1.0
    [<Obsolete("Do not use. Use [zeroCreate] instead.")>]
    let zero count = create count 0.0
    [<Obsolete("Do not use. Use [oneCreate] instead.")>]
    let ones count = create count 1.0
    ///Sum of all elements of the vector
    let sum vector = VecDS.sumVecDS vector
    ///Product of all elements of the vector
    let prod vector = fold (fun x y -> x * y) 1.0 vector
    ///Euklidian norm of the vector
    let norm (vector:vector) = sqrt (fold (fun x y -> x + y * y) 0.0 vector) (* fixed *)
    ///Builds a new vector whose elements are the results of exponentiating each of the elements of the vector with n.
    let toThePower n vector = map (fun x -> x ** n) vector
    [<Obsolete("Do not use. Use [toThePower] instead.")>]
    let cptPow vector y = map (fun x -> x ** y) vector
    ///Applies the given function to each of the indexes of the vector. No new vector is created.
    let inplaceAssign f (vector:vector) = VG.inplaceAssign f vector
    ///Applies the given function to each of the elements of the vector. No new vector is created.
    let inplaceMap f (vector:vector) = VG.inplaceMap f vector
    ///Applies the given function to each of the elements of the vector and their corresponding index. No new vector is created.
    let inplaceMapi f (vector:vector) = VG.inplaceMapi f vector
    ///Add values of vector2 to values of vector1. Vector2 stays unchanged.
    let inplaceAdd vector1 vector2 = VecDS.inplaceAddVecDS vector1 vector2
    ///Substract values of vector2 from values of vector1. Vector2 stays unchanged.
    let inplaceSub vector1 vector2 = VecDS.inplaceSubVecDS vector1 vector2
    ///Multiply values of vector1 with values of vector2. Vector2 stays unchanged.
    let inplaceCptMul vector1 vector2 = VecDS.inplaceCptMulVecDS vector1 vector2
    ///Multiply values of vector with scalar.
    let inplaceScale scalar vector = VecDS.inplaceScaleVecDS scalar vector
    ///Applies the given function to each of the indexes of the vector.
    ///Builds vector of Length 1 from value x
    let singleton x = ofScalar x
    [<Obsolete("Do not use. Use [inplaceAssign] instead.")>]
    let inplace_assign  f (v:vector) = VG.inplaceAssign f v
    ///Applies the given function to each of the elements of the vector.
    [<Obsolete("Do not use. Use [inplaceMap] instead.")>]
    let inplace_map f (v:vector) = VG.inplaceMap f v
    ///Applies the given function to each of the elements of the vector and their corresponding index.
    [<Obsolete("Do not use. Use [inplaceMapi] instead.")>]
    let inplace_mapi f (v:vector) = VG.inplaceMapi f v
    ///Add values of vector v2 to values of vector v1. Vector v2 stays unchanged
    [<Obsolete("Do not use. Use [inplaceAdd] instead.")>]
    let inplace_add v1 v2 = VecDS.inplaceAddVecDS v1 v2
    ///Substract values of vector v2 from values of vector v1. Vector v2 stays unchanged
    [<Obsolete("Do not use. Use [inplaceSub] instead.")>]
    let inplace_sub v1 v2 = VecDS.inplaceSubVecDS v1 v2
    ///Multiply values of vector v1 with values of vector v2. Vector v2 stays unchanged.
    [<Obsolete("Do not use. Use [inplaceCptMul] instead.")>]
    let inplace_cptMul v1 v2 = VecDS.inplaceCptMulVecDS v1 v2
    ///Multiply values of vector v1 with scalar.
    [<Obsolete("Do not use. Use [inplaceScale] instead.")>]
    let inplace_scale x v = VecDS.inplaceScaleVecDS x v
    ///Builds vector from array
    [<Obsolete("Do not use. Use [ofArr] instead.")>]
    let of_array arr   = ofArray arr
    ///Builds array from vector
    [<Obsolete("Do not use. Use [toArr] instead.")>]
    let to_array v     = toArray v
    ///Builds vector from list
    [<Obsolete("Do not use. Use [ofList] instead.")>]
    let of_list    xs  = ofList xs
    ///Builds vector from sequence
    [<Obsolete("Do not use. Use [ofSeq] instead.")>]
    let of_seq    xs   = ofSeq xs
    ///Builds one dimensional vector from scalar
    [<Obsolete("Do not use. Use [ofScalar] instead.")>]
    let of_scalar x    = ofScalar x


    
    //----------------------------------------------------------------------------
    // Stats
    //----------------------------------------------------------------------------
    
    /// Returns the raw data array without copy
    let raw (vector:Vector<'T>) = vector.Values

    ///
    let interval (items:Vector<'T>) =
        let rec loop index (minimum) (maximum) =
            if index < items.Length then
                let current = items.[index]
                loop (index+1) (min current minimum) (max current maximum)
            else
                Intervals.create minimum maximum          
        //Init by fist value
        if items.Length > 1 then
            loop 1 items.[0] items.[0] 
        else
            Intervals.Interval.Empty

    /// Computes the population mean (Normalized by N)            
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


    /// Computes the sample median
    let inline median (items:Vector<'T>) =
        items.Values |> Array.median
        
    /// Median absolute deviation (MAD)
    let medianAbsoluteDev (items : vector) =       
        items.Values |> Array.medianAbsoluteDev

    
    /// Returns SummaryStats of vector with N, mean, sum-of-squares, minimum and maximum
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


    /// Returns an estimator of the population covariance of two random variables v1 and v2 
    let inline covPopulation (v1:vector) (v2:vector) = 
        if v1.Length <> v2.Length then failwith "Vectors need to have the same length." 
        let rec loop n sumMul sumX sumY = 
            if n = v1.Length then
                sumMul,sumX,sumY 
            else 
                loop (n+1) (sumMul + (v1.[n]*v2.[n])) (sumX+v1.[n]) (sumY+v2.[n]) 
        let (mul,sumX,sumY) = loop 0 0. 0. 0.
        (mul - (sumX * sumY) / (float v1.Length) )/(float v1.Length) 

    /// Returns the sample covariance of two random variables v1 and v2. (Bessel's correction by N-1) 
    let inline cov (v1:vector) (v2:vector) = 
        if v1.Length <> v2.Length then failwith "Vectors need to have the same length." 
        let rec loop n sumMul sumX sumY = 
            if n = v1.Length then
                sumMul,sumX,sumY 
            else 
                loop (n+1) (sumMul + (v1.[n]*v2.[n])) (sumX+v1.[n]) (sumY+v2.[n]) 
        let (mul,sumX,sumY) = loop 0 0. 0. 0.
        (mul - (sumX * sumY)/(float v1.Length)) / (float v1.Length - 1.) 

    /// calculates the sample means with a given number of replicates present in the sequence
    let getMeanOfReplicates rep (data:vector) =
        Seq.getMeanOfReplicates rep data
        |> ofSeq 

    /// calculates the sample standard deviations with a given number of replicates present in the sequence
    let getStDevOfReplicates rep (data:vector) =
        Seq.getStDevOfReplicates rep data
        |> ofSeq 

    /// calculates the coefficient of variation based on the sample standard deviations with a given number of replicates present in the sequence
    let getCvOfReplicates rep (data:vector) =
        Seq.getCvOfReplicates rep data
        |> ofSeq 

    /// Splits a vector according to given indices. Returns (vector including values according to indices, rest)
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

        /// Returns SummaryStats of vector with N, mean, sum-of-squares, minimum and maximum
        let ofVector (a:Vector<'a>) = stats a









[<AutoOpen>]
module VectorExtension =

    type Vector<'T> with 
        member x.ToArray() = Vector.Generic.toArray x
        member x.Norm      = Vector.Generic.norm x
        member x.Copy ()   = Vector.Generic.copy x

   