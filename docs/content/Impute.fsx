(*** hide ***)
#I "../../bin"

(** 

#Imputation

Short documentation how to impute values

*)
#r @"FSharp.Stats.dll"
open FSharp.Stats
open FSharp.Stats.ML


let a = [3.;2.;3.;4.;5.;]
let b = [1.;2.;3.;nan;5.;]
let c = [nan;2.;3.;4.;nan;]
let d = [5.;2.;6.;4.;5.;]
let e = [0.5;2.;3.;5.;5.;]

let data = [a;b;c;d;e]


(**
<a name="k-Nearest imputation"></a>

##k-Nearest imputation

Missing data imputation based on the k-nearest neighbour algorithm:

*)

// init kNearest MatrixBaseImpute
let kn : Impute.MatrixBaseImputation<float[],float> = Impute.kNearestImpute 2
Impute.imputeBy kn Ops.isNan data


(**

<a name="random imputation"></a>

##random imputation

...
*)

// init random VectorBaseImpute
let rnd = Impute.rnd (System.Random())

Impute.imputeRowWiseBy rnd Ops.isNan data
Impute.imputeColWiseBy rnd Ops.isNan data


(**

<a name="normal imputation"></a>

##normal imputation

...

*)

Impute.imputeRowWiseBy Impute.normal Ops.isNan data
Impute.imputeColWiseBy Impute.normal Ops.isNan data


