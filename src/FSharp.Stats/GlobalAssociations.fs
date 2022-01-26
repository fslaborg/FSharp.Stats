// (c) Microsoft Corporation 2005-2009. 

//namespace Microsoft.FSharp.Math // old namespace
namespace FSharp.Stats

module GlobalAssociations =

//    open Microsoft.FSharp.Math
//    open Microsoft.FSharp.Math.Instances
    open FSharp.Stats.Instances
    open System
    open System.Numerics

    let ht = 
        let ht = new System.Collections.Generic.Dictionary<Type,obj>() 
        let optab =
            [ typeof<float>,   (Some(FloatNumerics    :> INumeric<float>) :> obj);
              typeof<int32>,   (Some(Int32Numerics    :> INumeric<int32>) :> obj);
              typeof<int64>,   (Some(Int64Numerics    :> INumeric<int64>) :> obj);
              typeof<BigInteger>,  (Some(BigIntNumerics   :> INumeric<BigInteger>) :> obj);
              typeof<float32>, (Some(Float32Numerics  :> INumeric<float32>) :> obj);
              typeof<bignum>,  (Some(BigRationalNumerics   :> INumeric<bignum>) :> obj); ]
           
        List.iter (fun (ty,ops) -> ht.Add(ty,ops)) optab;
        ht
        
    let Put (ty: System.Type, d : obj)  =
        lock ht (fun () -> 
            if ht.ContainsKey(ty) then invalidArg "ty" ("the type "+ty.Name+" already has a registered numeric association");
            ht.Add(ty, d))
      
    let TryGetNumericAssociation<'a>() = 
        lock ht (fun () -> 
            let ty = typeof<'a>  
            if ht.ContainsKey(ty) then
                match ht.[ty] with
                | :? (INumeric<'a> option) as r -> r
                | _ -> invalidArg "ty" ("The type "+ty.Name+" has a numeric association but it was not of the correct type")
            else
                None)

    let GetNumericAssociation() = (TryGetNumericAssociation()).Value
    let RegisterNumericAssociation (d : INumeric<'a>)  = Put(typeof<'a>, box(Some d))


