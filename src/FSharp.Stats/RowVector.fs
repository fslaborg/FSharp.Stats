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




[<AutoOpen>]
module RowVectorExtension =

    type RowVector<'T> with 
        member x.ToArray() = RowVector.Generic.toArray x
        member x.Copy ()   = RowVector.Generic.copy x



