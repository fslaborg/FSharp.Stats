namespace Microsoft.FSharp.NativeInterop

open Microsoft.FSharp.NativeInterop
open FSharp.Stats


module NativeUtilities =


    let nativeArray_as_CMatrix_colvec (arr: 'T NativeArray) = new CMatrix<_>(arr.Ptr,arr.Length,1)
    let nativeArray_as_FortranMatrix_colvec (arr: 'T NativeArray) = new FortranMatrix<_>(arr.Ptr,arr.Length,1)
    let pinM m = PinnedArray2.of_matrix(m)
    let pinV v = PinnedArray.of_vector(v)
    let pinA arr = PinnedArray.of_array(arr)
    
    let pinA2 arr = PinnedArray2.of_array2D(arr)
    
    let pinMV m1 v2 = pinM m1,pinV v2
    let pinVV v1 v2 = pinV v1,pinV v2
    let pinAA v1 v2 = pinA v1,pinA v2
    let pinMVV m1 v2 m3 = pinM m1,pinV v2,pinV m3
    let pinMM m1 m2  = pinM m1,pinM m2
    let pinMMM m1 m2 m3 = pinM m1,pinM m2,pinM m3
    let freeM (pA: PinnedArray2<'T>) = pA.Free()
    let freeV (pA: PinnedArray<'T>) = pA.Free()
    let freeA (pA: PinnedArray<'T>) = pA.Free()
    
    let freeA2 a = freeM a
    
    let freeMV (pA: PinnedArray2<'T>,pB : PinnedArray<'T>) = pA.Free(); pB.Free()
    let freeVV (pA: PinnedArray<'T>,pB : PinnedArray<'T>) = pA.Free(); pB.Free()
    let freeAA (pA: PinnedArray<'T>,pB : PinnedArray<'T>) = pA.Free(); pB.Free()
    let freeMM (pA: PinnedArray2<'T>,(pB: PinnedArray2<'T>)) = pA.Free();pB.Free()
    let freeMMM (pA: PinnedArray2<'T>,(pB: PinnedArray2<'T>),(pC: PinnedArray2<'T>)) = pA.Free();pB.Free();pC.Free()
    let freeMVV (pA: PinnedArray2<'T>,(pB: PinnedArray<'T>),(pC: PinnedArray<'T>)) = pA.Free();pB.Free();pC.Free()
    
    let matrixDims (m:Matrix<_>) = m.NumRows, m.NumCols
    let matrixDim1 (m:Matrix<_>) = m.NumRows
    let matrixDim2 (m:Matrix<_>) = m.NumCols
    let vectorDim  (v:Vector<_>) = v.Length
    
    let assertDimensions functionName (aName,bName) (a,b) =
      if a=b then () else
      failwith (sprintf "Require %s = %s, but %s = %d and %s = %d in %s" aName bName aName a bName b functionName)





