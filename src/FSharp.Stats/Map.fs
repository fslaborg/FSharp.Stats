namespace FSharp.Stats


/// Module to strore specialised computations on maps
module Map =

    /// <summary>Merges two maps into a single map. If a key exists in both maps, the value is determined by f with the first value being from mapA and the second originating from mapB.</summary>
    /// <param name="f">Function to transform values if key is present in both histograms. `mapA-value &#8594; mapB-value &#8594; newValue`</param>
    /// <param name="mapA">Frequency map A</param>
    /// <param name="mapB">Frequency map B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (mergeBy f a b) is not equal to (mergeBy f b a)</remarks> 
    /// <returns>New frequency map that results from merged maps mapA and mapB. Values from keys that are present in both maps are handled by f</returns> 
    let mergeBy (f: 'value -> 'value -> 'value) (mapA: Map<'key,'value>) (mapB:Map<'key,'value>) = 
        mapB 
        |> Map.fold (fun (s: Map<'key,'value>) kB vB -> 
            let tmp = Map.tryFind kB s
            match tmp with
            | Some x -> Map.change kB (fun vA -> Some (f x vB)) s
            | None -> Map.add kB vB s
            ) 
            mapA

    /// <summary>Merges two maps into a single map. If a key exists in both maps, the value in mapA is superseded by the value in mapB.</summary>
    /// <param name="mapA">Frequency map A</param>
    /// <param name="mapB">Frequency map B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (merge a b) is not equal to (merge b a)</remarks> 
    /// <returns>New frequency map that results from merged maps mapA and mapB.</returns> 
    let merge (mapA: Map<'key,'value>) (mapB: Map<'key,'value>) = 
        mergeBy (fun a b -> b) mapA mapB
    
    /// <summary>Merges two maps into a single map. If a key exists in both maps, the value from mapB is subtracted from the value of mapA.</summary>
    /// <param name="mapA">Frequency map A</param>
    /// <param name="mapB">Frequency map B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (subtract a b) is not equal to (subtract b a)</remarks> 
    let inline mergeSubtract (mapA: Map<'key,'value>) (mapB: Map<'key,'value>) = 
        mergeBy (fun a b -> a - b) mapA mapB
    
    /// <summary>Merges two maps into a single map. If a key exists in both maps, the value from mapB is added to the value of mapA.</summary>
    /// <param name="mapA">Frequency map A</param>
    /// <param name="mapB">Frequency map B</param>
    /// <remarks>When applied to continuous data the bandwidths must be equal!</remarks> 
    /// <remarks>This function is not commutative! (add a b) is not equal to (add b a)</remarks> 
    /// <returns>New frequency map that results from merged maps mapA and mapB. Values from keys that are present in both maps are handled by f</returns> 
    let inline mergeAdd (mapA: Map<'key,'value>) (mapB: Map<'key,'value>) = 
        mergeBy (fun a b -> a + b) mapA mapB


