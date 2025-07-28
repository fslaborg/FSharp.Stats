namespace FSharp.Stats.Algebra


open System
open FSharp.Stats
open FSharp.Stats.Acceleration

type Bidiagonalization() =

    static member inline bidiagonalizeInPlace<'T
        when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T : comparison
        and 'T :> ValueType
        and 'T :> Numerics.IRootFunctions<'T>>
        (A: Matrix<'T>) : unit =

        let m = A.NumCols
        let n = A.NumRows
        let minMN = min m n

        for k = 0 to minMN - 1 do
            // --- LEFT REFLECTION: Column k (zero below diagonal) ---
            let colLen = m - k
            let colVector = Array.init colLen (fun i -> A.[k + i, k])
            let hLeft = Householder.create colVector

            Householder.applyLeft(hLeft, A, k)

            // Overwrite A[k..,k] with Householder beta at top and zeros below
            A.[k, k] <- hLeft.Beta
            for i = k + 1 to m - 1 do
                A.[i, k] <- GenericMath.zero<'T>

            

            // --- RIGHT REFLECTION: Row k (zero right of superdiagonal) ---
            if k < n - 1 then
                let rowLen = n - (k + 1)
                let rowVector = Array.init rowLen (fun j -> A.[k, k + 1 + j])
                let hRight = Householder.create rowVector

                Householder.applyRight(hRight, A, k + 1)

                // Overwrite A[k,k+1..] with Householder beta at front, zeros right
                A.[k, k + 1] <- hRight.Beta
                for j = k + 2 to n - 1 do
                    A.[k, j] <- GenericMath.zero<'T>

                
[<Struct>]
type Bidiagonal<'T when 'T :> Numerics.INumber<'T>> = {
    D : Vector<'T>   // main diagonal
    E : Vector<'T>   // superdiagonal (length n-1)
}


/// Givens Rotation (Generic) 
module Givens =

    let inline compute<'T
        when 'T :> Numerics.INumber<'T>
        and 'T : comparison
        and 'T :> Numerics.IRootFunctions<'T>
        and 'T :> Numerics.IFloatingPointIeee754<'T>>
        (a: 'T) (b: 'T) : 'T * 'T =

        if b = GenericMath.zero then GenericMath.one, GenericMath.zero
        elif GenericMath.abs b > GenericMath.abs a then
            let t = a / b
            let s = GenericMath.one / GenericMath.sqrt(GenericMath.one + t * t)
            s * t, s
        else
            let t = b / a
            let c = GenericMath.one / GenericMath.sqrt(GenericMath.one + t * t)
            c, c * t

module GolubKahan =

    let inline diagonalize<'T
        when 'T :> Numerics.INumber<'T>
        and 'T :> Numerics.IRootFunctions<'T>
        and 'T :> Numerics.IFloatingPointIeee754<'T>
        and 'T : comparison
        and 'T : struct
        and 'T : (new: unit -> 'T)
        and 'T :> ValueType>
        (b: Bidiagonal<'T>) : Vector<'T> =

        let d = Array.copy b.D
        let e = Array.copy b.E
        let n = d.Length
        let eps = GenericMath.epsilon()
        let two = GenericMath.one + GenericMath.one

        let mutable iter = 0
        let maxIter = 1000
        let mutable doneIterating = false

       

        while iter < maxIter && not doneIterating do
            let mutable converged = true
            
            for i = 0 to n - 2 do
                let tolerance = eps * (GenericMath.abs d.[i] + GenericMath.abs d.[i + 1])
                if abs e.[i] > tolerance then
                    converged <- false

            if converged then
                doneIterating <- true
            else
                // Wilkinson shift
                let m = n - 1
                let dm1 = d.[m - 1]
                let dm  = d.[m]
                let em1 = e.[m - 1]

                let delta = (dm1 - dm) / two
                let sign =
                    if delta >= GenericMath.zero then GenericMath.one
                    else -GenericMath.one

                let denom = abs delta + sqrt (delta * delta + em1 * em1)
                let mu = dm - sign * (em1 * em1) / denom

                // Initial bulge
                let mutable x = d.[0] * d.[0] - mu * mu
                let mutable z = d.[0] * e.[0]

                for k = 0 to n - 2 do
                    let c, s = Givens.compute x z

                    let dk  = d.[k]
                    let ek  = e.[k]
                    let dk1 = d.[k + 1]

                    let tau1 = c * dk + s * ek
                    let tau2 = -s * dk1

                    d.[k]     <- c * tau1 + s * tau2
                    e.[k]     <- c * ek - s * dk1
                    d.[k + 1] <- s * tau1 - c * tau2

                    if k < n - 2 then
                        x <- e.[k]
                        z <- -s * e.[k + 1]
                        e.[k + 1] <- c * e.[k + 1]

                iter <- iter + 1

        d



module SVD =

    type private SvdCase =
        // 1 if s(p) and e[k-1] are negligible and k<p
        | Sp
        // 2 if s(k) is negligible and k<p
        | Sk
        // 3 if e[k-1] is negligible, k<p, and s(k), ..., s(p) are not negligible (qr step).
        | QrStep
        // 4 if e(p-1) is negligible (convergence).
        | Convergence

    exception BreakException 

    let private transpose (a:float[,]) =
        Array2D.init (a.GetLength(1)) (a.GetLength(0)) (fun i j -> a.[j,i])

    let computeInPlace (a:float[,]) =
        let transposeBeforeSVD = 
            let m = a.GetLength(0)
            let n = a.GetLength(1)  
            m < n
        let a = if transposeBeforeSVD then transpose a else a   
        //let a = Array2D.copy A
        // number of rows in A
        let m = a.GetLength(0)
        // number of columns in A
        let n = a.GetLength(1)  
        // * Apparently the failing cases are only a proper subset of (m<n), so
        // * let's not throw error. Correct fix to come later? if (m<n) { throw
        // * new IllegalArgumentException("Jama SVD only works for m >= n") }
        let nu = min m n
        let s  = Array.zeroCreate (min (m+1) n)
        let umatrix = Array2D.zeroCreate m m
        let vmatrix = Array2D.zeroCreate n n
        let e       = Array.zeroCreate n
        let work  = Array.zeroCreate m
        let mutable wantu = true
        let mutable wantv = true

        // Reduce A to bidiagonal form, storing the diagonal elements
        // in s and the super-diagonal elements in e.

        let nct = min (m - 1) n
        let nrt = max 0 (min (n - 2) m)
        for k = 0 to (max nct nrt) do
            if (k < nct) then
                // Compute the transformation for the k-th column and
                // place the k-th diagonal in s.[k].
                // Compute 2-norm of k-th column without under/overflow.
                s.[k] <- 0.
                for i = k to m-1 do
                    s.[k] <- Geometry.hypot s.[k] a.[i,k]
           
                if (s.[k] <> 0.0) then
                    if (a.[k,k] < 0.0) then
                        s.[k] <- -s.[k]
                
                    for i = k to m-1 do
                        a.[i,k] <- a.[i,k] / s.[k]
                
                    a.[k,k] <- a.[k,k] + 1.0
            
                s.[k] <- -s.[k]
        
            for j = k+1 to n-1 do 
                if ((k < nct) && (s.[k] <> 0.0)) then
                    // Apply the transformation.

                    let mutable t = 0.
                    for i = k to m-1 do 
                        t <- t + a.[i,k] * a.[i,j]
                
                    t <- -t / a.[k,k]
                    for i = k to m-1 do
                        a.[i,j] <- a.[i,j] + t * a.[i,k]
                
            
                // Place the k-th row of A into e for the
                // subsequent calculation of the row transformation.

                e.[j] <- a.[k,j]
        
            if (wantu && (k < nct)) then
        
                // Place the transformation in U for subsequent back
                // multiplication.

                for i = k to m-1 do
                    umatrix.[i,k] <- a.[i,k]
        
            if (k < nrt) then
                // Compute the k-th row transformation and place the
                // k-th super-diagonal in e[k].
                // Compute 2-norm without under/overflow.
                e.[k] <- 0.
                for i = k+1 to n-1 do
                    e.[k] <- Geometry.hypot e.[k] e.[i]
            
                if (e.[k] <> 0.0) then
                    if (e.[k + 1] < 0.0) then
                        e.[k] <- -e.[k]
                
                    for i = k+1 to n-1 do
                        e.[i] <- e.[i] / e.[k]
                
                    e.[k + 1] <- e.[k + 1]  + 1.0
            
                e.[k] <- -e.[k]
                if ((k + 1 < m) && (e.[k] <> 0.0)) then
                    // Apply the transformation.
                    for i = k+1 to m-1 do
                        work.[i] <- 0.0
                
                    for j = k+1 to n-1 do
                        for i = k+1 to m-1 do
                            work.[i] <- work.[i] + e.[j] * a.[i,j]
                
                    for j = k+1 to n-1 do
                        let t = -e.[j] / e.[k + 1]
                        for i = k+1 to m-1 do
                            a.[i,j] <- a.[i,j] + t * work.[i]
                

                if (wantv) then
                    // Place the transformation in V for subsequent
                    // back multiplication.
                    for i = k+1 to n-1 do
                        vmatrix.[i,k] <- e.[i]


        // Set up the final bidiagonal matrix or order p.
        let mutable p = min n (m + 1)
        if (nct < n) then
            s.[nct] <- a.[nct,nct]

        if (m < p) then
            s.[p - 1] <- 0.0

        if (nrt + 1 < p) then
            e.[nrt] <- a.[nrt,p - 1]

        e.[p - 1] <- 0.0

        // If required, generate U.
        if (wantu) then
            for j = nct to m-1 do
                for i = 0 to m-1 do
                    umatrix.[i,j] <- 0.0
                umatrix.[j,j] <- 1.0
        
            for k = nct-1 downto 0 do
                if (s.[k] <> 0.0) then
                    for j = k+1 to m-1 do 
                        let mutable t = 0.
                        for i = k to m-1 do
                            t <- t + umatrix.[i,k] * umatrix.[i,j]
                    
                        t <- -t / umatrix.[k,k]
                        for i = k to m-1 do
                            umatrix.[i,j] <- umatrix.[i,j] + t * umatrix.[i,k]
                    
                    for i = k to m-1 do
                        umatrix.[i,k] <- -umatrix.[i,k]
                
                    umatrix.[k,k] <- 1.0 + umatrix.[k,k]
                    for i = 0 to k-2 do 
                        umatrix.[i,k] <- 0.0
            
                else
                    for i = 0 to m-1 do 
                        umatrix.[i,k] <- 0.0
                
                    umatrix.[k,k] <- 1.0


        // If required, generate V.

        if (wantv) then
            for k = n-1 downto 0 do
                if ((k < nrt) && (e.[k] <> 0.0)) then
                    for j = k+1 to nu-1 do
                        let mutable t = 0.
                        for i = k+1 to n-1 do
                            t <- t + vmatrix.[i,k] * vmatrix.[i,j]
                    
                        t <- -t / vmatrix.[k + 1,k]
                        for i = k+1 to n-1 do 
                            vmatrix.[i,j] <- vmatrix.[i,j] + t * vmatrix.[i,k]
                    
                for i = 0 to n-1 do
                    vmatrix.[i,k] <- 0.0
                vmatrix.[k,k] <- 1.0

        // Main iteration loop for the singular values.    
        // TODO: rec
        let mutable pp = p - 1
        let mutable iter = 0
        let eps = System.Math.Pow(2.0, -52.0)
        let tiny = System.Math.Pow(2.0, -966.0)
        while (p > 0) do
            let mutable k = -1
            let mutable case = -1

            // Here is where a test for too many iterations would go.

            // This section of the program inspects for
            // negligible elements in the s and e arrays. On
            // completion the variables case and k are set as follows.

            // case = 1 if s(p) and e[k-1] are negligible and k<p
            // case = 2 if s(k) is negligible and k<p
            // case = 3 if e[k-1] is negligible, k<p, and
            // s(k), ..., s(p) are not negligible (qr step).
            // case = 4 if e(p-1) is negligible (convergence).
            try
            for kk = p-2 downto -1 do
                k <- kk
                if (k = -1) then
                    raise BreakException
        
                if ((abs e.[k]) <= tiny + eps
                    * ((abs s.[k]) + (abs s.[k + 1]))) then
            
                    e.[k] <- 0.0
                    raise BreakException
            
            with BreakException -> ()
        
            if (k = p - 2) then
                case <- 4
            else        
                let mutable ks = p-1
                try
                for ks' = p-1 downto k do
                    ks <- ks'
                    if (ks = k) then
                        raise BreakException
                    let mutable t = (if ks <> p then (abs e.[ks]) else 0.0) +
                                        (if ks <> k+1 then (abs e.[ks-1]) else 0.0)
                    if ((abs s.[ks]) <= tiny + eps*t) then
                        s.[ks] <- 0.0
                        raise BreakException
            
                with BreakException -> ()

                if (ks = k) then case <- 3
                elif (ks = p-1) then case <- 1
                else 
                    case <- 2
                    k <- ks
            k <- k + 1
            // Perform the task indicated by case.

            match case with
            | 1 ->             
                // Deflate negligible s(p).
                //printfn "case 1"
                let mutable f = e.[p - 2]
                e.[p - 2] <- 0.0
                for j = p-2 downto k do
                    let mutable t = Geometry.hypot s.[j] f
                    printfn "t : %A - f: %A" t f
                    let cs = s.[j] / t
                    let sn = f / t
                    s.[j] <- t
                    if (j <> k) then
                        f <- -sn * e.[j - 1]
                        e.[j - 1] <- cs * e.[j - 1]
                
                    if (wantv) then
                        for i = 0 to n-1 do
                            t <- cs * vmatrix.[i,j] + sn*vmatrix.[i,p - 1]
                            vmatrix.[i,p - 1] <- -sn * vmatrix.[i,j] + cs * vmatrix.[i,p - 1]
                            vmatrix.[i,j] <- t
                
            | 2 ->
                // Split at negligible s(k).
                //printfn "case 2"
                let mutable f = e.[k - 1]
                e.[k - 1] <- 0.0
                for j = k to p-1 do
                    let mutable t = Geometry.hypot s.[j] f
                    let cs = s.[j]/t
                    let sn = f/t
                    s.[j] <- t
                    f <- -sn*e.[j]
                    e.[j] <- cs*e.[j]
                    if (wantu) then
                        for i = 0 to m-1 do
                            t <- cs*umatrix.[i,j] + sn*umatrix.[i,k - 1]
                            umatrix.[i,k - 1] <- -sn*umatrix.[i,j] + cs*umatrix.[i,k - 1]
                            umatrix.[i,j] <- t
                // Perform one qr step.
            | 3 ->            
                // Calculate the shift.
                //printfn "case 3"
                let scale = 
                    max (abs s.[p - 1]) (abs s.[p - 2])
                    |> max (abs e.[p - 2])
                    |> max (abs s.[k])
                    |> max (abs e.[k])

                let sp = s.[p - 1]/scale
                let spm1 = s.[p - 2]/scale
                let epm1 = e.[p - 2]/scale
                let sk = s.[k]/scale
                let ek = e.[k]/scale
                let b = ((spm1 + sp)*(spm1 - sp) + epm1*epm1)/2.0
                let c = (sp*epm1)*(sp*epm1)
                let mutable shift = 0.0
                if ((b <> 0.0) || (c <> 0.0)) then
                    shift <- sqrt (b*b + c)
                    if (b < 0.0) then
                        shift <- -shift
                    shift <- c/(b + shift)
            
                let mutable f = (sk + sp)*(sk - sp) + shift
                let mutable g = sk*ek

                // Chase zeros.
                for j = k to p-2 do
                    let mutable t = Geometry.hypot f g
                    let mutable cs = f/t
                    let mutable sn = g/t
                    if (j <> k) then
                        e.[j - 1] <- t
                
                    f <- cs*s.[j] + sn*e.[j]
                    e.[j] <- cs*e.[j] - sn*s.[j]
                    g <- sn*s.[j + 1]
                    s.[j + 1] <- cs*s.[j + 1]
                    if (wantv) then
                        for i = 0 to n-1 do
                            t <- cs*vmatrix.[i,j] + sn*vmatrix.[i,j + 1]
                            vmatrix.[i,j + 1] <- -sn*vmatrix.[i,j] + cs*vmatrix.[i,j + 1]
                            vmatrix.[i,j] <- t
                    
                
                    t <- Geometry.hypot f g
                    cs <- f/t
                    sn <- g/t
                    s.[j] <- t
                    f <- cs*e.[j] + sn*s.[j + 1]
                    s.[j + 1] <- -sn*e.[j] + cs*s.[j + 1]
                    g <- sn*e.[j + 1]
                    e.[j + 1] <- cs*e.[j + 1]
                    if (wantu && (j < m - 1)) then
                        for i = 0 to m-1 do
                            t <- cs*umatrix.[i,j] + sn*umatrix.[i,j + 1]
                            umatrix.[i,j + 1] <- -sn*umatrix.[i,j] + cs*umatrix.[i,j + 1]
                            umatrix.[i,j] <- t



                e.[p - 2] <- f
                iter <- iter + 1

            // Convergence.
            | 4 ->            
                // Make the singular values positive.
                //printfn "case 4"
                if (s.[k] <= 0.0) then
                    s.[k] <- (if s.[k] < 0.0 then -s.[k] else 0.0)
                    if (wantv) then
                        for i = 0 to pp do
                            vmatrix.[i,k] <- -vmatrix.[i,k]
                 


                // Order the singular values.
                try
                while (k < pp) do
             
                    if (s.[k] >= s.[k + 1]) then raise BreakException

                    let t = s.[k]
                    s.[k] <- s.[k + 1]
                    s.[k + 1] <- t
                    if (wantv && (k < n - 1)) then
                        for i = 0 to n-1 do
                            let t = vmatrix.[i,k + 1] // mutable
                            vmatrix.[i,k + 1] <- vmatrix.[i,k]
                            vmatrix.[i,k] <- t
                
                    if (wantu && (k < m - 1)) then
                        for i = 0 to m-1 do 
                            let t = umatrix.[i,k + 1]
                            umatrix.[i,k + 1] <- umatrix.[i,k]
                            umatrix.[i,k] <- t

                    k <- k+1

                with BreakException -> ()

                iter <- 0
                p <- p - 1
            | _ -> failwithf "case %i does not exist" case

        if transposeBeforeSVD then 
            (vmatrix,s,transpose umatrix)
        else 
            (umatrix,s,transpose vmatrix)



    let compute (a:float[,]) =
        Array2D.copy a
        |> computeInPlace

    