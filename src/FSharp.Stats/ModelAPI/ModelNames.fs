namespace FSharp.Stats

type StatisticalModelName =
    | UnivariableOrdinaryLeastSquares
    | MultivariableOrdinaryLeastSquares

    static member toString = function
        | UnivariableOrdinaryLeastSquares -> "Univariable Ordinary Least Squares"
        | MultivariableOrdinaryLeastSquares -> "Multivariable Ordinary Least Squares"

    override this.ToString() = StatisticalModelName.toString this