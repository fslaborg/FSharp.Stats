namespace FSharp.Stats

module Rank =

    /// <summary>Comparer that sorts nan at the end of a collection</summary>
    /// <remarks></remarks>
    /// <param name="compNaNLast"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline internal compNaNLast<'T when 'T :> System.IComparable> =
        //if typeof<'T>.Name="Double"
        let comparison = 
            System.Comparison(fun (a : 'T when 'T :> System.IComparable) b -> 
                if nan.Equals(a) then 
                    1//if nan.Equals(b) then 0 else 1 
                elif nan.Equals(b) then 
                    -1
                else 
                    System.Collections.Generic.Comparer.Default.Compare(a,b)
            )
        System.Collections.Generic.Comparer<'T>.Create(comparison)

    /// <summary>Comparer that sorts nan at the start of a collection</summary>
    /// <remarks></remarks>
    /// <param name="compNaNFirst"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline internal compNaNFirst<'U when 'U :> System.IComparable> = 
        System.Collections.Generic.Comparer<'U>.Default
        
    /// <summary>Ranks each entry of the given unsorted data array. Use 'breakTies function to break ties</summary>
    /// <remarks></remarks>
    /// <param name="rank"></param>
    /// <param name="breakTies"></param>
    /// <param name="convert"></param>
    /// <param name="comparer"></param>
    /// <param name="rankNanWithNan"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline internal rank (breakTies: int -> int -> float) (convert: int -> float) (comparer: System.Collections.Generic.Comparer<'b>) (rankNanWithNan: bool) (data:'a []) : float [] =
        let data' = Array.copy data
        let ranks = Array.zeroCreate data.Length
        let index = Array.init data.Length id
        //let zero  = LanguagePrimitives.GenericZero< 'a >
        //let comparer,rankNanWithNan = if box zero :? float then comparer,rankNanWithNan else (compNaNFirst,false)
        let comparer,rankNanWithNan = if box data :? float [] then comparer,rankNanWithNan else (compNaNFirst,false)
        System.Array.Sort(data',index,comparer=comparer)
        
        let setTies a b =
            let tmp = breakTies a b
            for j = a to b-1 do
                ranks.[index.[j]] <- tmp
        
        let rec loop i pi =
            if i < data.Length then
                if rankNanWithNan && nan.Equals(data'.[pi]) then        //new
                    ranks.[index.[pi]] <- nan                           //new
                    loop (i+1) i                                        //new
                elif data'.[i] = data'.[pi] then
                    loop (i+1) pi
                else
                    if (i = pi + 1) then
                        ranks.[index.[pi]] <- convert i
                    else
                        //break ties
                        setTies pi i
        
                    loop (i+1) (i)
            else
                //break ties if left over                
                //setTies pi i
                if (i = pi + 1) then
                    if rankNanWithNan && nan.Equals(data'.[pi]) then    //new
                        ranks.[index.[pi]] <- nan                       //new
                        ()                                              //new 
                    else 
                        ranks.[index.[pi]] <- convert i
                else
                    //break ties
                    setTies pi i
        
        loop 1 0 |> ignore
        ranks
        

open System
open Rank

/// The rank of a number is its size relative to other values in a sequence
type Rank() = 
    /// Ranks each entry of the given unsorted data array.<br />Permutation with increasing values at each index of ties. Nans can be sorted as minimal or maximal values (default: maximal). Nans can be assigned to nan ranks (default: true)
    static member RankFirst(?NanIsMaximum,?RankNanWithNan) =
        let orderNanLast = defaultArg NanIsMaximum true
        let setNanToNan = defaultArg RankNanWithNan true
        fun (data:array<'b>) -> 
            let data' = Array.copy data
            //let comparer = if ties then compNaNLast else compNaNFirst
            let ranks  = Array.zeroCreate data.Length
            let index = Array.init data.Length id
            //System.Array.Sort(data',index,comparer=comparer)
            if orderNanLast then System.Array.Sort(data',index,comparer=compNaNLast) else System.Array.Sort(data',index)
            if setNanToNan && box data :? float [] then 
                for i=0 to ranks.Length-1 do
                    if nan.Equals data.[index.[i]] then 
                        ranks.[index.[i]] <- nan
                    else
                        ranks.[index.[i]] <- float (i + 1)
            else 
                for i=0 to ranks.Length-1 do
                    ranks.[index.[i]] <- float (i + 1)
                
            ranks
            
    /// Ranks each entry of the given unsorted data array.<br />Ties are replaced by their minimum. Nans can be sorted as minimal or maximal values (default: maximal). Nans can be assigned to nan ranks (default: true)
    static member RankMin(?NanIsMaximum,?RankNanWithNan) =
        let orderNanLast = defaultArg NanIsMaximum true
        let setNanToNan = defaultArg RankNanWithNan true
        let comparer = if orderNanLast then compNaNLast else compNaNFirst
        let minTies a _ =  (float a + 1.)
        
        //fun (data:'T[] when 'T :> System.IComparable and ^T : (static member get_Zero : ^T))  -> 
        fun (data)  -> 
            rank minTies float comparer setNanToNan data
            

    /// Ranks each entry of the given unsorted data array.<br />Ties are replaced by their maximum. Nans can be sorted as minimal or maximal values (default: maximal). Nans can be assigned to nan ranks (default: true)
    static member RankMax(?NanIsMaximum,?RankNanWithNan) =
        let orderNanLast = defaultArg NanIsMaximum true
        let setNanToNan = defaultArg RankNanWithNan true
        let comparer = if orderNanLast then compNaNLast else compNaNFirst
        let maxTies _ b = float b

        fun (data:array<_>) -> 
            rank maxTies float comparer setNanToNan data

    /// Ranks each entry of the given unsorted data array.<br />Ties are replaced by ther average ranks. Nans can be sorted as minimal or maximal values (default: maximal). Nans can be assigned to nan ranks (default: true)
    static member RankAverage(?NanIsMaximum,?RankNanWithNan) =
        let orderNanLast = defaultArg NanIsMaximum true
        let setNanToNan = defaultArg RankNanWithNan true
        let comparer = if orderNanLast then compNaNLast else compNaNFirst
        let averageTies a b = float (a + b + 1) / 2.//([(a + 1) .. b] |> List.sum) / float (b - a)
        
        fun (data:array<_>) -> 
            rank averageTies float comparer setNanToNan data

    [<Obsolete("Use Rank.RankAverage() instead")>]
    static member rankAverage = fun (x: float[]) -> Rank.RankAverage(false,false) x
    [<Obsolete("Use Rank.RankFirst() instead")>] 
    static member rankFirst = fun (x: float[]) -> Rank.RankFirst(false,false) x |> Array.map int
    [<Obsolete("Use Rank.RankMin() instead")>]
    static member rankMin = fun (x: float[]) -> Rank.RankMin(false,false) x
    [<Obsolete("Use Rank.RankMax() instead")>]
    static member rankMax = fun (x: float[]) -> Rank.RankMax(false,false) x



