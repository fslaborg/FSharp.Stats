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
        let map2  f a b = OpsS.map2V f a b
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
    let map2  f (a:vector) (b:vector) = VG.map2 f a b
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

   