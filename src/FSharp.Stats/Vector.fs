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
        let get (v:Vector<_>) i   = v.[i]
        ///Sets the value x to the vector a at the given index i
        let set (v:Vector<_>) i x = v.[i] <- x
        ///Returns length of vector v
        let length (v:Vector<_>) = v.Length
        ///Creates vector from list xss
        let ofList    l   = OpsS.listV l
        ///Creates vector from seq xss
        let ofSeq    s   = OpsS.seqV s
        let init  count   f = OpsS.initV count f
        let initNumeric  count   f = OpsS.createNumericV count f
        let ofArray arr       = OpsS.arrayV arr
        let toArray (v:Vector<_>) = Array.init v.Length (get v)

        let create  count x   = OpsS.constV count x
        let zero count = OpsS.zeroV count
        let ones count = OpsS.createNumericV count (fun ops _ -> ops.One)
        let ofScalar   x = OpsS.scalarV x
        let add v1 v2 = OpsS.addV v1 v2
        let sub v1 v2 = OpsS.subV v1 v2
        let mulRVV v1 v2 = OpsS.mulRVV v1 v2
        let mulVRV v1 v2 = OpsS.mulVRV v1 v2
        let cptMul v1 v2 = OpsS.cptMulV v1 v2
        let cptMax v1 v2 = OpsS.cptMaxV v1 v2
        let cptMin v1 v2 = OpsS.cptMinV v1 v2
        let scale a b = OpsS.scaleV a b
        ///Dot product of the two vectors
        let dot v1 v2 = OpsS.dotV v1 v2
        let neg v = OpsS.negV v 
        let transpose v = OpsS.transV v 
        let inplaceAdd v1 v2 = OpsS.inplaceAddV v1 v2
        let inplaceSub v1 v2 = OpsS.inplaceSubV v1 v2
        let inplace_cptMul v1 v2 = OpsS.inplaceCptMulV v1 v2
        let inplace_scale v1 v2 = OpsS.inplaceScaleV v1 v2


        let exists  f v = OpsS.existsV  f v
        let forall  f v = OpsS.forallV  f v
        let existsi  f v = OpsS.existsiV  f v
        let foralli  f v = OpsS.foralliV  f v
        let map  f v = OpsS.mapV f v
        let map2  f v1 v2 = OpsS.map2V f v1 v2
        let map3  f v1 v2 v3 = OpsS.map3V f v1 v2 v3
        let zip v1 v2 = OpsS.zipV v1 v2
        let unzip v = OpsS.unzipV v
        let mapi f a = OpsS.mapiV f a
        let copy a = OpsS.copyV a
        let inplace_map  f a = OpsS.inplace_mapV f a
        let inplace_mapi  f a = OpsS.inplace_mapiV f a
        let fold  f z a = OpsS.foldV f z a
        let foldi  f z a = OpsS.foldiV f z a
        let compare a b = OpsS.compareV a b
        let hash a = OpsS.hashV a
        let inplace_assign  f a = OpsS.assignV f a
        ///Sum of all elements of the vector a
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
    ///Returns the value of the vector a at the given index j
    let get (v:vector) j   = VG.get v j 
    ///Sets the value x to the vector a at the given index j
    let set (v:vector) j x = VG.set v j x
    ///Returns length of vector a
    let length (v:vector)     = VG.length v
    ///Returns length of vector a
    let nrows (v:vector)   = VG.length v
    ///Initiates vector of length m and fills it by applying function f on indices
    let init  i   f = VecDS.createVecDS  i   f
    ///Creates vector with values of array arr
    let ofArray arr : vector = VG.ofArray arr
    ///Creates array with values of vector m
    let toArray (v : vector) = VG.toArray v

    type range = int * int
    let countR ((a,b) : range)   = (b-a)+1
    let idxR    ((a,_) : range) i = a+i
    type rangef = float * float * float // start, skip, end
    let countRF ((a,d,b) : rangef)   = System.Convert.ToInt32((b-a)/d) + 1
    //let countRF ((a,d,b) : rangef)   = Float.to_int((b-a)/d) + 1
    let idxRF  ((a,d,b) : rangef) i = System.Math.Min (a + d * float(i),b)

    let range n1 n2    = let r = (n1,n2)   in init (countR  r) (fun i -> float(idxR r i)) 

    let rangef a b c  = let r = (a,b,c) in init (countRF r) (fun i -> idxRF r i)
    ///Creates vector with values of list xs
    let ofList    xs    = VecDS.listVecDS    xs
    ///Creates vector with values of sequence xs
    let ofSeq    xs    = VecDS.seqVecDS    xs
    ///Creates vector of length i and fills it with value x
    let create  m   x  = VecDS.constVecDS  m   x
    ///Creates one dimensional vector of value x
    let ofScalar x     = VecDS.scalarVecDS x
    ///Builds a new vector whose elements are the results of adding the corresponding elements of the two vectors pairwise. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let add a b = VecDS.addVecDS   a b
    ///Builds a new vector whose elements are the results of substracting the corresponding elements of vector b from vector a. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let sub v1 v2 = VecDS.subVecDS   v1 v2
    let mulRVV v1 v2 = VecDS.mulRowVecVecDS   v1 v2
    let mulVRV v1 v2 = VecDS.mulVecRowVecDS   v1 v2
    ///Builds a new vector whose elements are the results of multiplying the corresponding elements of the given vectors. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let cptMul v1 v2 = VecDS.cptMulVecDS   v1 v2
    let cptMax v1 v2 = VecDS.cptMaxVecDS v1 v2
    let cptMin v1 v2 = VecDS.cptMinVecDS v1 v2
    ///Builds a new vector whose elements are the results of multiplying the given scalar with each of the elements of the vector.
    let scale x v = VecDS.scaleVecDS   x v
    ///Builds a new vector whose elements are the results of multiplying -1 with each of the elements of the vector.
    let neg v  = VecDS.negVecDS v
    ///Dot product of the two vectors
    let dot v1 v2 = VecDS.dotVecDS v1 v2
    let transpose  (v:vector) = VG.transpose v
    let exists  f (v:vector) = VG.exists f v
    let forall  f (v:vector) = VG.forall f v
    let existsi  f (v:vector) = VG.existsi f v
    let foralli  f (v:vector) = VG.foralli f v
    ///Builds a new vector whose elements are the results of applying the given function to each of the elements of the vector.
    let map  f (v:vector) = VG.map f v
    ///Builds a new vector whose elements are the results of applying the given function to the corresponding elements of the two vectors pairwise. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let map2  f (v1:vector) (v2:vector) = VG.map2 f v1 v2
    ///Builds a new vector whose elements are the results of applying the given function to the corresponding elements of the two vectors pairwise. The two input vectors must have the same lengths, otherwise ArgumentException is raised.
    let map3  f (v1:vector) (v2:vector) (v3:vector) = VG.map3 f v1 v2 v3
    ///Builds a new vector that contains the elements of the given vector.
    let copy (v:vector) = VG.copy v
    ///Builds a new vector whose elements are the results of applying the given function to each of the elements of the vector and their corresponding index.
    let mapi  f (v:vector) : vector = VG.mapi f v
    ///Applies a function to each element of the vector, threading an accumulator argument through the computation.
    let fold  f (acc:'State) (v:vector) = VG.fold f acc v
    ///Applies a function to each element of the vector and their corresponding index, threading an accumulator argument through the computation.
    let foldi  f (acc:'State) (v:vector) = VG.foldi f acc v
    ///Creates a vector of length n and fills it with zeros
    let zero n = create n 0.0
    ///Creates a vector of length n and fills it with ones
    let ones n = create n 1.0
    ///Sum of all elements of the vector
    let sum v  = VecDS.sumVecDS v
    ///Product of all elements of the vector
    let prod v   = fold      (fun x y -> x * y) 1.0 v
    ///Euklidian norm of the vector
    let norm  (v:vector) = sqrt (fold (fun x y -> x + y * y) 0.0 v) (* fixed *)
    ///Builds a new vector whose elements are the results of exponentiating each of the elements of the vector.
    let cptPow  v y = map  (fun x -> x ** y) v
    ///Applies the given function to each of the indexes of the vector.
    let inplace_assign  f (v:vector) = VG.inplace_assign f v
    ///Applies the given function to each of the elements of the vector.
    let inplace_map f (v:vector) = VG.inplace_map f v
    ///Applies the given function to each of the elements of the vector and their corresponding index.
    let inplace_mapi f (v:vector) = VG.inplace_mapi f v
    ///Add values of vector v2 to values of vector v1. Vector v2 stays unchanged.
    let inplace_add v1 v2 = VecDS.inplaceAddVecDS v1 v2
    ///Substract values of vector v2 from values of vector v1. Vector v2 stays unchanged.
    let inplace_sub v1 v2 = VecDS.inplaceSubVecDS v1 v2
    ///Multiply values of vector v1 with values of vector v2. Vector v2 stays unchanged.
    let inplace_cptMul v1 v2 = VecDS.inplaceCptMulVecDS v1 v2
    ///Multiply values of vector v1 with scalar.
    let inplace_scale x v = VecDS.inplaceScaleVecDS x v
    ///Builds vector from array
    let of_array arr   = ofArray arr
    ///Builds array from vector
    let to_array v     = toArray v
    ///Builds vector from list
    let of_list    xs  = ofList xs
    ///Builds vector from sequence
    let of_seq    xs   = ofSeq xs
    ///Builds one dimensional vector from scalar
    let of_scalar x    = ofScalar x
    ///Builds vector of Length 1 from value x
    let singleton x = of_scalar x
    
    //----------------------------------------------------------------------------
    // Stats
    //----------------------------------------------------------------------------
    
    /// Returns the raw data array without copy
    let raw (a:Vector<'T>) = a.Values

    ///
    let interval (a:Vector<'T>) =
        let rec loop index (minimum) (maximum) =
            if index < a.Length then
                let current = a.[index]
                loop (index+1) (min current minimum) (max current maximum)
            else
                Intervals.create minimum maximum          
        //Init by fist value
        if a.Length > 1 then
            loop 1 a.[0] a.[0] 
        else
            Intervals.Interval.Empty

    /// Computes the population mean (Normalized by N)            
    let inline mean (a:Vector<'T>) = 
        let ops  = a.ElementOps
        let zero = ops.Zero
        let one  = ops.One
        let rec loop i c acc =            
            if i < a.Length then
                loop (i+1) (ops.Add(c,one)) (ops.Add(acc,a.[i]))
            else
                acc / c
        loop 0 zero zero


    /// Computes the sample median
    let inline median (a:Vector<'T>) =
        a.Values |> Array.median
        

    
    /// Returns SummaryStats of vector with N, mean, sum-of-squares, minimum and maximum
    let inline stats (a:Vector<'T>) =
        let zero = LanguagePrimitives.GenericZero< 'T > 
        let one = LanguagePrimitives.GenericOne< 'T >        
        
        let rec loop index n (minimum) (maximum) m1 m2 =
            if index < a.Length then            
                let current  = a.[index]
                let delta    = current - m1               
                let delta_n  = (delta / n)
                //let delta_n2 = delta_n * delta_n
                let m1'    = m1 + delta_n            
                let m2' = m2 + delta * delta_n * (n-one)
                loop (index+1) (n + one) (min current minimum) (max current maximum) m1' m2'
            else
                SummaryStats.createSummaryStats (n-one) m1 m2 minimum maximum
        //Init by fist value
        if a.Length > 1 then
            loop 0 one a.[0] a.[0] zero zero 
        else
            let uNan = zero / zero 
            SummaryStats.createSummaryStats zero uNan uNan uNan uNan


    /// Returns an estimator of the population covariance of two random variables v1 and v2 
    let inline covPopulation (v1:vector) (v2:vector) = 
        if v1.Length <> v2.Length then failwith "Vectors need to have the same length." 
        let rec loop n sumMul sumX sumY = 
            if n = v1.Length-1 then
                sumMul,sumX,sumY 
            else 
                loop (n+1) (sumMul + (v1.[n]*v2.[n])) (sumX+v1.[n]) (sumY+v2.[n]) 
        let (mul,sumX,sumY) = loop 0 0. 0. 0.
        (mul - (sumX * sumY) / (float v1.Length) )/(float v1.Length) 

    /// Returns the sample covariance of two random variables v1 and v2. (Bessel's correction by N-1) 
    let inline cov (v1:vector) (v2:vector) = 
        if v1.Length <> v2.Length then failwith "Vectors need to have the same length." 
        let rec loop n sumMul sumX sumY = 
            if n = v1.Length-1 then
                sumMul,sumX,sumY 
            else 
                loop (n+1) (sumMul + (v1.[n]*v2.[n])) (sumX+v1.[n]) (sumY+v2.[n]) 
        let (mul,sumX,sumY) = loop 0 0. 0. 0.
        (mul - (sumX * sumY)/(float v1.Length)) / (float v1.Length - 1.) 

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

   