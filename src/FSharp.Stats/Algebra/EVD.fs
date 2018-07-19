namespace FSharp.Stats.Algebra

open FSharp.Stats

module EVD =
    
    /// Symmetric Householder reduction to tridiagonal form.
    let private tred2Inplace (d:float[]) (e:float[]) (v:float [,]) =
    
        // This is derived from the Algol procedures tred2 by
        // Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
        // Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
        // Fortran subroutine in EISPACK.    
    
        let n = v.GetLength(1) // cols count v

        for i = n - 1 downto 1 do
            // Scale to avoid under/overflow.

            let mutable scale = 0.0;
            let mutable  h = 0.0;
            for k = 0 to i-1 do 
                scale <- scale + abs (d.[k])

            if scale = 0.0 then
                e.[i] <- d.[i - 1]
                for j = 0 to i-1 do
                    d.[j] <- v.[i - 1,j]
                    v.[i,j] <- 0.0
                    v.[j,i] <- 0.0
            else
                // Generate Householder vector.

                for k = 0 to i-1 do
                    d.[k] <- d.[k] / scale
                    h <- h + d.[k] * d.[k]
                let mutable f = d.[i - 1]
                let mutable g = if (f > 0.) then -sqrt(h) else sqrt(h)
                e.[i] <- scale*g
                h <- h - f*g
                d.[i - 1] <- f - g
                for j = 0 to i-1 do            
                    e.[j] <- 0.0
            
                // Apply similarity transformation to remaining columns.

                for j = 0 to i-1 do
                    f <- d.[j]
                    v.[j,i] <- f
                    g <- e.[j] + v.[j,j]*f
                    for k = j + 1 to i - 1 do                
                        g <- g + v.[k,j]*d.[k]
                        e.[k] <- e.[k] + v.[k,j]*f
                
                    e.[j] <- g

                f <- 0.0
                for j = 0 to i-1 do
                    e.[j] <- e.[j] / h
                    f <- f + e.[j] * d.[j]
                let hh = f/(h + h)
                for j = 0 to i-1 do
                    e.[j] <- e.[j] - hh*d.[j]
            
                for j = 0 to i-1 do
                    f <- d.[j]
                    g <- e.[j]
                    for k = j to i - 1 do
                        v.[k,j] <- v.[k,j] - (f*e.[k] + g*d.[k])
                
                    d.[j] <- v.[i - 1,j]
                    v.[i,j] <- 0.0
            d.[i] <- h

        // Accumulate transformations.
        for i = 0 to n - 2 do    
            v.[n - 1,i] <- v.[i,i]
            v.[i,i] <- 1.0;
            let h = d.[i + 1]
            if (h <> 0.0) then
                for k = 0 to i do
                    d.[k] <- v.[k,i + 1]/h
                for j = 0 to i do
                    let mutable g = 0.0
                    for k = 0 to i do
                        g <- g + v.[k,i + 1]*v.[k,j]
                    for k = 0 to i do
                        v.[k,j] <- v.[k,j] - g*d.[k]
            for k = 0 to i do
                v.[k,i + 1] <- 0.0
    
        for j = 0 to n-1 do
            d.[j] <- v.[n - 1,j]
            v.[n - 1,j] <- 0.0
    
        v.[n - 1,n - 1] <- 1.0
        e.[0] <- 0.0


    /// Symmetric tridiagonal QL algorithm.
    let private tql2Inplace (d:float[]) (e:float[]) (v:float [,]) =

        // This is derived from the Algol procedures tql2, by
        // Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
        // Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
        // Fortran subroutine in EISPACK.

        let n = v.GetLength(1) // cols count v
        
        for i = 1 to n-1 do
            e.[i - 1] <- e.[i]
        e.[n - 1] <- 0.0

        let mutable f = 0.0
        let mutable tst1 = 0.0
        let mutable eps = 2.0**(-52.0)
        for l = 0 to n-1 do    
            // Find small subdiagonal element

            tst1 <- max tst1 (abs(d.[l]) + abs(e.[l]))
            let m = 
                let rec loop m =
                    if m > n-2 || (abs(e.[m]) <= eps*tst1) then
                        m
                    else 
                        loop (m+1)
                loop l

            // If m == l, d.[l] is an eigenvalue,
            // otherwise, iterate.

            if (m > l) then
                let rec loop iter =
                    // (Could check iteration count here.)

                    // Compute implicit shift

                    let mutable g = d.[l]
                    let mutable p = (d.[l + 1] - g)/(2.0*e.[l])
                    let mutable r = if (p < 0.) then -(Geometry.hypot p 1.0) else Geometry.hypot p 1.0
                    d.[l] <- e.[l]/(p + r)
                    d.[l + 1] <- e.[l]*(p + r)
                    let dl1 = d.[l + 1];
                    let mutable h = g - d.[l]
                    for i = l + 2 to n-1 do
                        d.[i] <- d.[i] - h;
                
                    f <- f + h

                    // Implicit QL transformation.

                    p <- d.[m];
                    let mutable c = 1.0
                    let mutable c2 = c
                    let mutable c3 = c
                    let mutable el1 = e.[l + 1]
                    let mutable s = 0.0
                    let mutable s2 = 0.0
                    for i = m-1 downto l do
                        c3 <- c2
                        c2 <- c
                        s2 <- s
                        g <- c*e.[i]
                        h <- c*p
                        r <- Geometry.hypot p e.[i]
                        e.[i + 1] <- s*r
                        s <- e.[i]/r
                        c <- p/r
                        p <- c*d.[i] - s*g
                        d.[i + 1] <- h + s*(c*g + s*d.[i])

                        // Accumulate transformation.

                        for k = 0 to n-1 do 
                            h <- v.[k,i + 1]
                            v.[k,i + 1] <- s*v.[k,i] + c*h
                            v.[k,i] <- c*v.[k,i] - s*h
                
                    p <- -s*s2*c3*el1*e.[l]/dl1
                    e.[l] <- s*p
                    d.[l] <- c*p
                
                    // Check for convergence.
                    if (abs(e.[l]) > eps*tst1) then
                        loop (iter+1)
                loop 0

            d.[l] <- d.[l] + f
            e.[l] <- 0.0


        // Sort eigenvalues and corresponding vectors.
        for i = 0 to n-2 do
            let mutable k = i
            let mutable p = d.[i]
            for j = i+1 to n-1 do
                if (d.[j] < p) then
                    k <- j
                    p <- d.[j]
        
            if (k <> i) then
                d.[k] <- d.[i];
                d.[i] <- p;
                for j = 0 to n-1 do
                    p <- v.[j,i]
                    v.[j,i] <- v.[j,k]
                    v.[j,k] <- p

    /// Computes the 
    let symmetricEvd (a:float[,]) =
        
        // number of columns in A
        let n = a.GetLength(1)
        // Init array for the real parts of the eigenvalues
        let d = Array.init n (fun j -> a.[n - 1,j])
        // Init array for the imaginary parts of the eigenvalues
        let e = Array.zeroCreate n                

        // Eigenvalue matrix
        let v = Array2D.copy a

        // Tridiagonalize.
        tred2Inplace d e v
        // Diagonalize.
        tql2Inplace d e v
        
        (e,v,d)

    let getRealEigenvalues (e,v,d) = d
    let getImaginaryEigenvalues (e,v,d) = e
    let getEigenvalueMAtrix (e,v,d) = v

