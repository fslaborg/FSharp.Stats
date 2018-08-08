namespace FSharp.Stats

#nowarn "51"

/// Warning:
/// IMPORTANT WARNING NOTICE:
/// INTEL MATH KERNEL LIBRARY 9.1 FOR WINDOWS IS THIRD PARTY TECHNOLOGY.
/// IT IS CLEARED ONLY FOR USE BY A SPECIFIC MSR RESEARCH TEAM.
/// DO NOT USE IT UNTIL YOU HAVE CLEARED ITS USE FOR YOUR PROJECT WITH YOUR LEGAL CONTACT.
/// 
/// The following stubs bind directly to Intel MKL functionality.
/// You should not use them without:
/// a) Intel MKL developer licenses.
/// b) Seeking local legal approval.
module LapackMKLStubs =

    // [<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)>]
    // [<StructAttribute>]
    type CBLASLAYOUT =
       | CblasRowMajor = 101 | CblasColMajor = 102

    //[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)>]
    type CBLASTRANSPOSE =
      | CblasNoTrans=111 | CblasTrans=112 | CblasConjTrans=113

           
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="cblas_dgemm")>]
    //extern void dgemm_(char *transa, char *transb, int *m, int *n, int *k, double *alpha, double *a, int *lda, double *b, int *ldb, double *beta, double *c, int *ldc)
    extern void dgemm_(CBLASLAYOUT *layout,CBLASTRANSPOSE *transa, CBLASTRANSPOSE *transb, int *m, int *n, int *k, double *alpha, double *a, int *lda, double *b, int *ldb, double *beta, double *c, int *ldc)

    // [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dtrsv")>]
    // extern void dtrsv_(char *uplo,char *trans,char *diag,int *n,double *a,int *lda,double *x,int *incx)

    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dtrsm")>]
    extern void dtrsm_(char *side,char *uplo,char *trans,char *diag,int *m,int *n,double *alpha,double *a,int *lda,double *b,int *ldb)
   
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dgesv")>]
    extern void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info)

    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dgeev")>]
    extern void dgeev_(char *jobvl, char *jobvr, int *n, double *a, int *lda, double *wr, double *wi, double *vl, int *ldvl, double *vr, int *ldvr, double *work, int *lwork, int *info)
    // [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dposv")>]
    // extern void dposv_(char *uplo, int *n, int *nrhs, double *a, int *lda, double *b, int *ldb, int *info)
  
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dgels")>]
    extern void dgels_(char *trans, int *m,int *n, int *nrhs, double *a, int *lda, double *b, int *ldb, double *work, int *lwork, int *info)
  
    // [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dgglse")>]
    // extern void dgglse_(int *m, int *n, int *p, double *a, int *lda, double *b, int *ldb, double *c, double *d, double *x, double *work, int *lwork, int *info)
  
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dsyev")>]
    extern void dsyev_(char *jobz, char *uplo, int *n, double *a,int *lda, double *w, double *work, int *lwork, int *info)
  
    // [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dsyevd")>]
    // extern void dsyevd_(char *jobz, char *uplo, int *n, double *a, int *lda, double *w, double *work, int *lwork, int *iwork, int *liwork, int *info)
    
    // Available
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dgesvd")>]
    extern void dgesvd_(char *jobu, char *jobvt, int  *m, int *n, double *a, int *lda, double *s, double *u, int *ldu, double *vt, int *ldvt, double *work, int *lwork, int *info)
  
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dgesdd")>]
    extern void dgesdd_(char *JOBZ, int  *m, int *n, double *a, int *lda, double *s, double *u, int *ldu, double *vt, int *ldvt, double *work, int *lwork,int *iwork, int *info)
  
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dsygv")>]
    extern void dsygv_(int *itype, char *jobz, char *uplo, int *n, double *a, int *lda, double *b, int *ldb, double *w, double *work, int *lwork, int *info)
  
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dsygvd")>]     
    extern void dsygvd_(int *itype, char *jobz, char *uplo, int *n, double *a, int *lda, double *b, int *ldb, double *w, double *work, int *lwork,int *iwork, int *liwork, int *info)
  
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dggev")>]     
    extern void dggev_( char *jobvl, char *jobvr, int *n, double *a, int *lda, double *b, int *ldb, double *alphar, double *alphai,double *beta,double *vl,int *ldvl,double *vr,int *ldvr,double *work, int *lwork,int *info)
  
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dgesvx")>]     
    extern void dgesvx_(char *fact, char *trans, int *n, int *nrhs, double *a, int *lda, double *af, int *ldaf, int *ipiv, char *equed, double *r, double *c, double *b, int *ldb, double *x, int *ldx, double *rcond, double *ferr, double *berr, double *work, int *iwork, int *info)
  
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dposvx")>]     
    extern void  dposvx_(char *fact, char *uplo, int *n, int *nrhs, double *a, int *lda, double *af, int *ldaf, char *equed, double *s, double *b, int *ldb, double *x, int *ldx, double *rcond, double  *ferr, double *berr, double *work, int *iwork, int *info)

    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dpotrf")>]     
    extern void  dpotrf_(char *uplo, int *n, double *a, int *lda, int *info)
  
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dgetrf")>]     
    extern void  dgetrf_(int *m, int *n, double *a, int *lda, int *ipiv, int *info)
  
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="dgeqrf")>]     
    extern void dgeqrf_(int  *m, int *n, double *a, int *lda, double *tau, double *work, int *lwork, int *info)
  
    [<System.Runtime.InteropServices.DllImport(@"mkl.dll",EntryPoint="cblas_dgemv")>]
    extern void dgemv_(int32* trans, int* m, int* n,double* alpha, double* A, int* lda,double* x, int* incx, double* beta,double* y, int* incy)


module BlasStubs =

    [<System.Runtime.InteropServices.DllImport(@"libblas.dll",EntryPoint="dgemm_")>]
    extern void dgemm_(char *TRANSA, char *TRANSB, int *M, int *N, int *K, double *ALPHA, double *A, int *LDA, double *B, int *LDB, double *BETA, double *C, int *LDC)


module LapackStubs =
    
    [<System.Runtime.InteropServices.DllImport(@"liblapack.dll",EntryPoint="dgesvd_")>]
    extern void dgesvd_(char* JOBU, char* JOBVT, int *M, int *N, double *A, int *LDA, double *S, double *U, int *LDU, double *VT, int *LDVT, double *WORK, int *LWORK, int *INFO)

    [<System.Runtime.InteropServices.DllImport(@"liblapack.dll",EntryPoint="dgesdd_")>]
    extern void dgesdd_(char *JOBZ, int *M, int *N, double *A, int *LDA, double *S, double *U, int *LDU, double *VT, int *LDVT, double *WORK, int *LWORK, int *IWORK, int *INFO)

    [<System.Runtime.InteropServices.DllImport(@"liblapack.dll",EntryPoint="dgeev_")>]
    extern void dgeev_(char *JOBVL, char *JOBVR, int *N, double *A, int *LDA, double *WR, double *WI, double *VL, int *LDVL, double *VR, int *LDVR, double *WORK, int *LWORK, int *INFO);

    [<System.Runtime.InteropServices.DllImport(@"liblapack.dll",EntryPoint="dsyevd_")>]
    extern void dsyevd_(char *jobz, char *uplo, int *n, double *a, int *lda, double *w, double *work, int *lwork, int *iwork, int *liwork, int *info);

open Microsoft.FSharp.NativeInterop
open LapackMKLStubs
open FSharp.Stats.Algebra
  
/// Internal provider of MKL functionality, not for direct user usage.
type LinearAlgebraMKL() =
    interface ILinearAlgebra with 
    
        ///Matrix-Matrix Multiplication
        member this.dgemm_ (a,b) = //(a:matrix) (b:matrix) = 
            // input copies
            let a = Matrix.copy a
            let b = Matrix.copy b
            // dimensions
            let m = NativeUtilities.matrixDim1 a in
            let k = NativeUtilities.matrixDim2 a in
            NativeUtilities.assertDimensions "dgemm_" ("k","Dim1(b)") (k,NativeUtilities.matrixDim1 b);
            let n = NativeUtilities.matrixDim2 b in
            // allocate results
            let c = Matrix.zero (m) (n)
            // transpose
            let c = Matrix.transpose c
            // setup actuals
            let mutable arg_layout = CBLASLAYOUT.CblasColMajor //'t'
            let mutable arg_transa = CBLASTRANSPOSE.CblasTrans//'t'
            let mutable arg_transb = CBLASTRANSPOSE.CblasNoTrans//'t'
            let mutable arg_m = m
            let mutable arg_n = n
            let mutable arg_k = k
            let mutable arg_alpha = 1.0
            let arg_a = NativeUtilities.pinM a
            let mutable arg_ldk = k
            let arg_b = NativeUtilities.pinM b
            let mutable arg_ldn = n
            let mutable arg_beta = 1.0
            let arg_c = NativeUtilities.pinM c
            let mutable arg_ldm = m
            // call function
            try
                LapackMKLStubs.dgemm_(&&arg_layout,&&arg_transa,&&arg_transb,&&arg_m,&&arg_n,&&arg_k,&&arg_alpha,arg_a.Ptr,&&arg_ldk,arg_b.Ptr,&&arg_ldn,&&arg_beta,arg_c.Ptr,&&arg_ldm)
            finally
                NativeUtilities.freeM arg_a
                NativeUtilities.freeM arg_b
                NativeUtilities.freeM arg_c
            // INFO
            // fixups
            let c = Matrix.transpose c
            // result tuple
            c




        ///Singular Value Decomposition Divide- Conquer
        member this.dgesdd_ (a:matrix) = 
            // input copies
            let a = Matrix.copy a
            // dimensions
            let m = NativeUtilities.matrixDim1 a in
            let n = NativeUtilities.matrixDim2 a in
            // allocate results
            let s = Array.zeroCreate  (min m n)
            let u = Matrix.zero (m) (m)
            let vt = Matrix.zero (n) (n)
            let work = Array.zeroCreate  (1)
            let iwork = Array.zeroCreate  (8*(min m n))
            // transpose
            let a = Matrix.transpose a
            let u = Matrix.transpose u
            let vt = Matrix.transpose vt
            // setup actuals
            let mutable arg_JOBZ = 'A'
            let mutable arg_m = m
            let mutable arg_n = n
            let arg_a = NativeUtilities.pinM a
            let mutable arg_lda = max 1 m
            let arg_s = NativeUtilities.pinA s
            let arg_u = NativeUtilities.pinM u
            let mutable arg_ldu = m
            let arg_vt = NativeUtilities.pinM vt
            let mutable arg_ldvt = n
            let arg_work = NativeUtilities.pinA work
            let mutable arg_lwork = -1
            let arg_iwork = NativeUtilities.pinA iwork
            let mutable arg_info = 0
            // ask for work array size
            try
                LapackMKLStubs.dgesdd_(&&arg_JOBZ,&&arg_m,&&arg_n,arg_a.Ptr,&&arg_lda,arg_s.Ptr,arg_u.Ptr,&&arg_ldu,arg_vt.Ptr,&&arg_ldvt,arg_work.Ptr,&&arg_lwork,arg_iwork.Ptr,&&arg_info)
            finally
                NativeUtilities.freeA arg_work
       
            if arg_info = 0   || arg_info=(-12) then
                arg_lwork <- int32 work.[0]
            else assert(false)
        
            let arg_work = NativeUtilities.pinA (Array.zeroCreate arg_lwork : float[])
            // call function
            try
                LapackMKLStubs.dgesdd_(&&arg_JOBZ,&&arg_m,&&arg_n,arg_a.Ptr,&&arg_lda,arg_s.Ptr,arg_u.Ptr,&&arg_ldu,arg_vt.Ptr,&&arg_ldvt,arg_work.Ptr,&&arg_lwork,arg_iwork.Ptr,&&arg_info)
            finally
                NativeUtilities.freeM arg_a
                NativeUtilities.freeA arg_s
                NativeUtilities.freeM arg_u
                NativeUtilities.freeM arg_vt
                NativeUtilities.freeA arg_work
                NativeUtilities.freeA arg_iwork
            // INFO
            match arg_info with
            | -1  -> failwith "dgesdd_: JOBZ (argument 1)"
            | -2  -> failwith "dgesdd_: m (argument 2)"
            | -3  -> failwith "dgesdd_: n (argument 3)"
            | -4  -> failwith "dgesdd_: a (argument 4)"
            | -5  -> failwith "dgesdd_: lda (argument 5)"
            | -6  -> failwith "dgesdd_: s (argument 6)"
            | -7  -> failwith "dgesdd_: u (argument 7)"
            | -8  -> failwith "dgesdd_: ldu (argument 8)"
            | -9  -> failwith "dgesdd_: vt (argument 9)"
            | -10 -> failwith "dgesdd_: ldvt (argument 10)"
            | -11 -> failwith "dgesdd_: work (argument 11)"
            | -12 -> failwith "dgesdd_: lwork (argument 12)"
            | -13 -> failwith "dgesdd_: iwork (argument 13)"
            | -14 -> failwith "dgesdd_: info (argument 14)"
            | 0   -> ()
            | n   -> failwith (sprintf "dgesdd_ : returned %d. The computation failed." n)
            // fixups
            let u = Matrix.transpose u
            let vt = Matrix.transpose vt
            // result tuple
            s,u,vt

        member this.dgesdd_thin_(a:matrix) = 
            printfn "Function not implemented yet, use the lapack provider"
            ([||],matrix[||],matrix[||])

        member this.dsyevd_(a:matrix) = 
            printfn "Function not implemented yet, use the lapack provider"
            (matrix[||],[||])

        //member this.dgeev_(a:Matrix<float>) =
        //    Array.zeroCreate 0, Array.zeroCreate 0, matrix [||]


/// Internal provider of Lapack functionality, not for direct user usage.
type LinearAlgebraLAPACK() =
    interface ILinearAlgebra with 
        //Matrix-Matrix Multiplication
        member this.dgemm_ (a:Matrix<float>,b:Matrix<float>) = 

            // input copies
            let a = Matrix.copy a
            let b = Matrix.copy b

            // dimensions
            let m = NativeUtilities.matrixDim1 a in
            let k = NativeUtilities.matrixDim2 a in
            NativeUtilities.assertDimensions "dgemm_" ("k","Dim1(b)") (k,NativeUtilities.matrixDim1 b);
            let n = NativeUtilities.matrixDim2 b in

            // allocate results
            let c = Matrix.zero (m) (n)

            // transpose
            let c = Matrix.transpose c
            //setup actuals ------------------------------------------------------------
            //computation type and scalars        
            let mutable arg_TRANSA = 'T'
            let mutable arg_TRANSB = 'T'
            let mutable arg_alpha = 1.
            let mutable arg_beta = 0.

            //dimensions of input arrays
            let mutable arg_m = m
            let mutable arg_n = n 
            let mutable arg_k = k

            let mutable arg_lda = max 1 k
            let mutable arg_ldb = max 1 n
            let mutable arg_ldc = max 1 m

            let arg_a = NativeUtilities.pinM a
            let arg_b = NativeUtilities.pinM b
            let arg_c = NativeUtilities.pinM c

            try 
                printfn "calling dgemm ..."
                BlasStubs.dgemm_(&&arg_TRANSA,&&arg_TRANSB,&&arg_m,&&arg_n,&&arg_k,&&arg_alpha,arg_a.Ptr,&&arg_lda,arg_b.Ptr,&&arg_ldb,&&arg_beta,arg_c.Ptr,&&arg_ldc)
            finally
                NativeUtilities.freeM arg_a
                NativeUtilities.freeM arg_b
                NativeUtilities.freeM arg_c

            let c = Matrix.transpose c
            // result tuple
            c
        //Full SVD
        member this.dgesdd_(a:Matrix<float>) =

            // input copies
            let a = Matrix.copy a
            // dimensions
            let m = NativeUtilities.matrixDim1 a in
            let n = NativeUtilities.matrixDim2 a in

            let s = Array.zeroCreate  (min m n)
            let u = Matrix.zero (m) (m)
            let vt = Matrix.zero (n) (n)
            let work = Array.zeroCreate  (1)
            let iwork = Array.zeroCreate  (8*(min m n))

            // transpose
            let a = Matrix.transpose a
            let u = Matrix.transpose u
            let vt = Matrix.transpose vt

            //setup actuals ------------------------------------------------------------
            //computation type
            let mutable arg_JOBZ = 'A'

            //dimensions of input array a
            let mutable arg_m = m
            let mutable arg_n = n 

            //output matrices and their leading dimensions
            let mutable arg_lda = max 1 m
            let mutable arg_ldu = m
            let mutable arg_ldvt = n

            let arg_s   = NativeUtilities.pinA s
            let arg_a   = NativeUtilities.pinM a
            let arg_u   = NativeUtilities.pinM u
            let arg_vt  = NativeUtilities.pinM vt

            //workspaces
            let arg_work = NativeUtilities.pinA work
            let mutable arg_lwork = -1
            let arg_iwork = NativeUtilities.pinA iwork
            let mutable arg_info = 0

            try 
                LapackStubs.dgesdd_(&&arg_JOBZ,&&arg_m,&&arg_n,arg_a.Ptr,&&arg_lda,arg_s.Ptr,arg_u.Ptr,&&arg_ldu,arg_vt.Ptr,&&arg_ldvt,arg_work.Ptr,&&arg_lwork,arg_iwork.Ptr,&&arg_info )
            finally
                NativeUtilities.freeA arg_work

            //get workspace size
            if arg_info = 0   || arg_info=(-12) then
                arg_lwork <- int32 work.[0]
            else assert(false)

            //assign workspace size to the work array
            let arg_work = NativeUtilities.pinA (Array.zeroCreate arg_lwork : float[])
            //call dgesdd_
            try 
                LapackStubs.dgesdd_(&&arg_JOBZ,&&arg_m,&&arg_n,arg_a.Ptr,&&arg_lda,arg_s.Ptr,arg_u.Ptr,&&arg_ldu,arg_vt.Ptr,&&arg_ldvt,arg_work.Ptr,&&arg_lwork,arg_iwork.Ptr,&&arg_info )
            finally
                NativeUtilities.freeM arg_a
                NativeUtilities.freeA arg_s
                NativeUtilities.freeM arg_u
                NativeUtilities.freeM arg_vt
                NativeUtilities.freeA arg_work
                NativeUtilities.freeA arg_iwork
            //fail if info contains an error code
            match arg_info with
            | -1  -> failwith "dgesdd_: JOBZ (argument 1)"
            | -2  -> failwith "dgesdd_: m (argument 2)"
            | -3  -> failwith "dgesdd_: n (argument 3)"
            | -4  -> failwith "dgesdd_: a (argument 4)"
            | -5  -> failwith "dgesdd_: lda (argument 5)"
            | -6  -> failwith "dgesdd_: s (argument 6)"
            | -7  -> failwith "dgesdd_: u (argument 7)"
            | -8  -> failwith "dgesdd_: ldu (argument 8)"
            | -9  -> failwith "dgesdd_: vt (argument 9)"
            | -10 -> failwith "dgesdd_: ldvt (argument 10)"
            | -11 -> failwith "dgesdd_: work (argument 11)"
            | -12 -> failwith "dgesdd_: lwork (argument 12)"
            | -13 -> failwith "dgesdd_: iwork (argument 13)"
            | -14 -> failwith "dgesdd_: info (argument 14)"
            | 0   -> ()
            | n   -> failwith (sprintf "dgesdd_ : returned %d. The computation failed." n)
            
            // fixups
            let u = Matrix.transpose u
            let vt = Matrix.transpose vt
            // result tuple
            s,u,vt

        //Thin SVD
        member this.dgesdd_thin_(a:Matrix<float>) =
            // input copies
            let a = Matrix.copy a
            // dimensions
            let m = NativeUtilities.matrixDim1 a in
            let n = NativeUtilities.matrixDim2 a in

            let s = Array.zeroCreate  (min m n)
            let u = Matrix.zero (m) (n)
            let vt = Matrix.zero (n) (n)
            let work = Array.zeroCreate  (1)
            let iwork = Array.zeroCreate  (8*(min m n))

            //setup actuals ------------------------------------------------------------
            //computation type
            let mutable arg_JOBZ = 'S'

            //dimensions of input array a
            let mutable arg_m = m
            let mutable arg_n = n 

            //output matrices and their leading dimensions
            let mutable arg_ldu = m
            let mutable arg_ldvt = n
            let mutable arg_lda = max 1 m
            
            // transpose
            let a = Matrix.transpose a
            let u = Matrix.transpose u
            let vt = Matrix.transpose vt

            let arg_s   = NativeUtilities.pinA s
            let arg_a   = NativeUtilities.pinM a
            let arg_u   = NativeUtilities.pinM u
            let arg_vt  = NativeUtilities.pinM vt

            //workspaces
            let arg_work = NativeUtilities.pinA work
            let mutable arg_lwork = -1
            let arg_iwork = NativeUtilities.pinA iwork
            let mutable arg_info = 0

            try 
                LapackStubs.dgesdd_(&&arg_JOBZ,&&arg_m,&&arg_n,arg_a.Ptr,&&arg_lda,arg_s.Ptr,arg_u.Ptr,&&arg_ldu,arg_vt.Ptr,&&arg_ldvt,arg_work.Ptr,&&arg_lwork,arg_iwork.Ptr,&&arg_info )
            finally
                NativeUtilities.freeA arg_work

            //get workspace size
            if arg_info = 0   || arg_info=(-12) then
                arg_lwork <- int32 work.[0]
            else assert(false)

            //assign workspace size to the work array
            let arg_work = NativeUtilities.pinA (Array.zeroCreate arg_lwork : float[])
            //call dgesdd_
            try 
                LapackStubs.dgesdd_(&&arg_JOBZ,&&arg_m,&&arg_n,arg_a.Ptr,&&arg_lda,arg_s.Ptr,arg_u.Ptr,&&arg_ldu,arg_vt.Ptr,&&arg_ldvt,arg_work.Ptr,&&arg_lwork,arg_iwork.Ptr,&&arg_info )
            finally
                NativeUtilities.freeM arg_a
                NativeUtilities.freeA arg_s
                NativeUtilities.freeM arg_u
                NativeUtilities.freeM arg_vt
                NativeUtilities.freeA arg_work
                NativeUtilities.freeA arg_iwork
            //fail if info contains an error code
            match arg_info with
            | -1  -> failwith "dgesdd_: JOBZ (argument 1)"
            | -2  -> failwith "dgesdd_: m (argument 2)"
            | -3  -> failwith "dgesdd_: n (argument 3)"
            | -4  -> failwith "dgesdd_: a (argument 4)"
            | -5  -> failwith "dgesdd_: lda (argument 5)"
            | -6  -> failwith "dgesdd_: s (argument 6)"
            | -7  -> failwith "dgesdd_: u (argument 7)"
            | -8  -> failwith "dgesdd_: ldu (argument 8)"
            | -9  -> failwith "dgesdd_: vt (argument 9)"
            | -10 -> failwith "dgesdd_: ldvt (argument 10)"
            | -11 -> failwith "dgesdd_: work (argument 11)"
            | -12 -> failwith "dgesdd_: lwork (argument 12)"
            | -13 -> failwith "dgesdd_: iwork (argument 13)"
            | -14 -> failwith "dgesdd_: info (argument 14)"
            | 0   -> ()
            | n   -> failwith (sprintf "dgesdd_ : returned %d. The computation failed." n)
            
            // fixups
            let u = Matrix.transpose u
            let vt = Matrix.transpose vt
            // result tuple
            s,u,vt

        //Eigenvalue decomposition of a real non-symmetric square matrix
        //member this.dgeev_(a:Matrix<float>) =
        //    // input copies
        //    let a = Matrix.copy a
        //    // dimensions
        //    let n = NativeUtilities.matrixDim1 a in
        //    NativeUtilities.assertDimensions "dgeev_" ("n","Dim2(a)") (n,NativeUtilities.matrixDim2 a);
        //    // allocate results
        //    let wr = Array.zeroCreate  (n)
        //    let wi = Array.zeroCreate  (n)
        //    let vl = Matrix.zero (n) (n)
        //    let vr = Matrix.zero (n) (n)
        //    let work = Array.zeroCreate  (1)
        //    // transpose
        //    let a = Matrix.transpose a
        //    // setup actuals
        //    let mutable arg_jobvl = 'N'
        //    let mutable arg_jobvr = 'V'
        //    let mutable arg_n = n
        //    let arg_a = NativeUtilities.pinM a
        //    let mutable arg_lda = n
        //    let arg_wr = NativeUtilities.pinA wr
        //    let arg_wi = NativeUtilities.pinA wi
        //    let arg_vl = NativeUtilities.pinM vl
        //    let mutable arg_ldvl = n
        //    let arg_vr = NativeUtilities.pinM vr
        //    let mutable arg_ldvr = n
        //    let arg_work = NativeUtilities.pinA work
        //    let mutable arg_lwork = -1
        //    let mutable arg_info = 0
        //    //workspace query
        //    try 
        //        LapackStubs.dgeev_(&&arg_jobvl,&&arg_jobvr,&&arg_n,arg_a.Ptr,&&arg_lda,arg_wr.Ptr,arg_wi.Ptr,arg_vl.Ptr,&&arg_ldvl,arg_vr.Ptr,&&arg_ldvr,arg_work.Ptr,&&arg_lwork,&&arg_info)
        //    finally
        //        NativeUtilities.freeA arg_work
        //    //get workspace size
        //    if arg_info = 0   || arg_info=(-12) then
        //        arg_lwork <- int32 work.[0]
        //    else assert(false)
        //    //assign workspace size to the work array
        //    let arg_work = NativeUtilities.pinA (Array.zeroCreate arg_lwork : float[])
        //    //call function
        //    try
        //      LapackStubs.dgeev_(&&arg_jobvl,&&arg_jobvr,&&arg_n,arg_a.Ptr,&&arg_lda,arg_wr.Ptr,arg_wi.Ptr,arg_vl.Ptr,&&arg_ldvl,arg_vr.Ptr,&&arg_ldvr,arg_work.Ptr,&&arg_lwork,&&arg_info)
        //    finally
        //      NativeUtilities.freeM arg_a
        //      NativeUtilities.freeA arg_wr
        //      NativeUtilities.freeA arg_wi
        //      NativeUtilities.freeM arg_vl
        //      NativeUtilities.freeM arg_vr
        //      NativeUtilities.freeA arg_work
        //    // INFO
        //    match arg_info with
        //     | -1  -> failwith "dgeev_: jobvl (argument 1)"
        //     | -2  -> failwith "dgeev_: jobvr (argument 2)"
        //     | -3  -> failwith "dgeev_: n (argument 3)"
        //     | -4  -> failwith "dgeev_: a (argument 4)"
        //     | -5  -> failwith "dgeev_: lda (argument 5)"
        //     | -6  -> failwith "dgeev_: wr (argument 6)"
        //     | -7  -> failwith "dgeev_: wi (argument 7)"
        //     | -8  -> failwith "dgeev_: vl (argument 8)"
        //     | -9  -> failwith "dgeev_: ldvl (argument 9)"
        //     | -10 -> failwith "dgeev_: vr (argument 10)"
        //     | -11 -> failwith "dgeev_: ldvr (argument 11)"
        //     | -12 -> failwith "dgeev_: work (argument 12)"
        //     | -13 -> failwith "dgeev_: lwork (argument 13)"
        //     | -14 -> failwith "dgeev_: info (argument 14)"
        //     | 0   -> ()
        //     | n   -> failwith (sprintf "dgeev_ : returned %d. The computation failed." n)
        //    // fixups
        //    let vr = Matrix.transpose vr
        //    // result tuple
        //    wr,wi,vr

        ///Eigen Value Decomposition of Symmetric Matrix
        member this.dsyevd_(a:matrix) = 

            let jobz = 'V'
            let uplo = 'L'
            // input copies
            let a = Matrix.copy a
            // dimensions
            let n = NativeUtilities.matrixDim1 a in
            NativeUtilities.assertDimensions "dsyevd_" ("n","Dim2(a)") (n,NativeUtilities.matrixDim2 a);
            // allocate results
            let w = Array.zeroCreate  (n)
            let work = Array.zeroCreate  (1)
            let iwork = Array.zeroCreate  (1)
            // transpose
            let a = Matrix.transpose a
            // setup actuals
            let mutable arg_jobz = jobz
            let mutable arg_uplo = uplo
            let mutable arg_n = n
            let arg_a = NativeUtilities.pinM a
            let mutable arg_lda = max 1 n
            let arg_w = NativeUtilities.pinA w
            let arg_work = NativeUtilities.pinA work
            let mutable arg_lwork = -1
            let arg_iwork = NativeUtilities.pinA iwork
            let mutable arg_liwork = -1
            let mutable arg_info = 0
            // ask for work array size
            try
                LapackStubs.dsyevd_(&&arg_jobz,&&arg_uplo,&&arg_n,arg_a.Ptr,&&arg_lda,arg_w.Ptr,arg_work.Ptr,&&arg_lwork,arg_iwork.Ptr,&&arg_liwork,&&arg_info)
            finally
                NativeUtilities.freeA arg_work
                NativeUtilities.freeA arg_iwork
            if arg_info = 0   || arg_info=(-8)  || arg_info=(-10) then
                arg_lwork <- int32 work.[0]
                arg_liwork <-  iwork.[0]
            else assert(false)
            let arg_work = NativeUtilities.pinA (Array.zeroCreate arg_lwork : float[])
            let arg_iwork = NativeUtilities.pinA (Array.zeroCreate arg_liwork : int[])
            // call function
            try
                LapackStubs.dsyevd_(&&arg_jobz,&&arg_uplo,&&arg_n,arg_a.Ptr,&&arg_lda,arg_w.Ptr,arg_work.Ptr,&&arg_lwork,arg_iwork.Ptr,&&arg_liwork,&&arg_info)
            finally
                NativeUtilities.freeM arg_a
                NativeUtilities.freeA arg_w
                NativeUtilities.freeA arg_work
                NativeUtilities.freeA arg_iwork
            // INFO
            match arg_info with
             | -1  -> failwith "dsyevd_: jobz (argument 1)"
             | -2  -> failwith "dsyevd_: uplo (argument 2)"
             | -3  -> failwith "dsyevd_: n (argument 3)"
             | -4  -> failwith "dsyevd_: a (argument 4)"
             | -5  -> failwith "dsyevd_: lda (argument 5)"
             | -6  -> failwith "dsyevd_: w (argument 6)"
             | -7  -> failwith "dsyevd_: work (argument 7)"
             | -8  -> failwith "dsyevd_: lwork (argument 8)"
             | -9  -> failwith "dsyevd_: iwork (argument 9)"
             | -10 -> failwith "dsyevd_: liwork (argument 10)"
             | -11 -> failwith "dsyevd_: info (argument 11)"
             | 0   -> ()
             | n   -> failwith (sprintf "dsyevd_ : returned %d. The computation failed." n)
            // fixups
            let a = Matrix.transpose a
            // result tuple
            a,w



module ProviderService = 
    let MKLProvider = ServiceLocator.createProviderX64  "MKL" [|"mkl.dll"|] ServiceLocator.OS.Windows (fun () -> new LinearAlgebraMKL() :> ILinearAlgebra)
    let LAPACKProvider = ServiceLocator.createProviderX64  "LAPACK" [|"libblas.dll";"liblapack.dll";"libgcc_s_seh-1.dll";"libgfortran-4.dll";"libquadmath-0.dll";"libwinpthread-1.dll"|] ServiceLocator.OS.Windows (fun () -> new LinearAlgebraLAPACK() :> ILinearAlgebra)





