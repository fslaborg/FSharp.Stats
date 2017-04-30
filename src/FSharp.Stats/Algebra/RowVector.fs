namespace Microsoft.FSharp.Math

#nowarn "60" // implementations in augmentations
#nowarn "69" // implementations in augmentations

open Microsoft.FSharp.Math
open System
open System.Globalization
open System.Collections
open System.Collections.Generic
open System.Diagnostics

[<AutoOpen>]
module RowVectorExtensions = 
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


        member m.DebugDisplay = 
            let txt = GenericImpl.showRowVecGU "rowvec" m
            new System.Text.StringBuilder(txt)  // return an object with a ToString with the right value, rather than a string. (strings get shown using quotes)

        member m.StructuredDisplayAsArray =  Array.init m.NumCols (fun i -> m.[i])

        member m.Details = m.Values

        member m.Transpose = SpecializedGenericImpl.transRV m
        
        member m.Permute (p:permutation) = SpecializedGenericImpl.permuteRV p m


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



