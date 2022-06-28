namespace FSharp.Stats.SpecialFunctions

open System

/// Special mathematical functions
module Binomial =

    ///<summary>
    /// Returns the binomial coeffcient (n | k) via the factorial formula.
    ///
    /// for some combinations of n and k, this might result in overflows.
    ///
    /// The caller is responsible to handle edge cases such as nan, infinity, and -infinity in the input
    ///</summary>
    //<param name="n">input for n in the computation of (n | k)</param>
    //<param name="k">input for k in the computation of (n | k)</param>
    let _coeffcient (n:int) (k:int) = 
        if ( n < 0 || k < 0 || k > n) then invalidArg "Binomial.coeffcient" ""
        if (n < 171) then 
            //floor (0.5 + Factorial.factorial n / ((Factorial.factorial k) * (Factorial.factorial (n-k))))
            Factorial.factorial n / ((Factorial.factorial k) * (Factorial.factorial (n-k)))
        else
            floor (0.5 + exp ((Factorial._factorialLn n) - (Factorial._factorialLn k) - (Factorial._factorialLn (n-k))))

    ///<summary>
    /// Returns the binomial coeffcient (n | k) via the factorial formula.
    ///
    /// for some combinations of n and k, this might result in overflows.
    ///
    /// Edge cases in the input (nan, infinity, and -infinity) are catched and handled. 
    ///
    /// This might be slower than the unchecked version `_coefficient` but does not require input sanitation to get expected results for these cases.
    ///</summary>
    //<param name="n">input for n in the computation of (n | k)</param>
    //<param name="k">input for k in the computation of (n | k)</param>
    let coeffcient (n:int) (k:int) = 
        if ( n < 0 || k < 0 || k > n) then invalidArg "Binomial.coeffcient" ""
        if (n < 171) then 
            //floor (0.5 + Factorial.factorial n / ((Factorial.factorial k) * (Factorial.factorial (n-k))))
            Factorial.factorial n / ((Factorial.factorial k) * (Factorial.factorial (n-k)))
        else
            floor (0.5 + exp ((Factorial.factorialLn n) - (Factorial.factorialLn k) - (Factorial.factorialLn (n-k))))

    ///<summary>
    /// Returns the natural logarithm of the binomial coeffcient (n | k) via the factorial formula.
    ///
    /// for some combinations of n and k, this might result in overflows.
    ///
    /// The caller is responsible to handle edge cases such as nan, infinity, and -infinity in the input
    ///</summary>
    //<param name="n">input for n in the computation of ln(n | k)</param>
    //<param name="k">input for k in the computation of ln(n | k)</param>
    let _coeffcientLn (n:int) (k:int) = 
        if ( n < 0 || k < 0 || k > n) then invalidArg "Binomial.coeffcient" ""
        (Factorial._factorialLn n) - (Factorial._factorialLn k) - (Factorial._factorialLn (n-k))

    ///<summary>
    /// Returns the natural logarithm of the binomial coeffcient (n | k) via the factorial formula.
    ///
    /// for some combinations of n and k, this might result in overflows.
    ///
    /// Edge cases in the input (nan, infinity, and -infinity) are catched and handled. 
    ///
    /// This might be slower than the unchecked version `_coefficient` but does not require input sanitation to get expected results for these cases.
    ///</summary>
    //<param name="n">input for n in the computation of ln(n | k)</param>
    //<param name="k">input for k in the computation of ln(n | k)</param>
    let coeffcientLn (n:int) (k:int) = 
        if ( n < 0 || k < 0 || k > n) then invalidArg "Binomial.coeffcient" ""
        (Factorial.factorialLn n) - (Factorial.factorialLn k) - (Factorial.factorialLn (n-k))

