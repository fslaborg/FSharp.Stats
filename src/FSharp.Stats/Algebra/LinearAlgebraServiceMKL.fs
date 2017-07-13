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
  


module LinearAlgebraMKL = 

    open Microsoft.FSharp.NativeInterop
    open LapackMKLStubs
    
    ///Matrix-Matrix Multiplication
    let dgemm_ (a:matrix) (b:matrix) = 
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
    let dgesdd_ (a:matrix) = 
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







