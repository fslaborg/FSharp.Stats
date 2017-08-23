(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
open FSharp.Plotly
(**
Basic stats
=========================

**)
#r "FSharp.Stats.dll"
open FSharp.Stats


// Range min to max
let rTv = 
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.rangeBy float


// Mean 28.6
(**
$ \sum_{i=1}^{10} t_i $
**)
let mTv = 
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.meanBy float


// Harmonic mean 10.01109
let hmTv = 
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.meanHarmonicBy float


// Geometric mean 18.92809
let gmTv = 
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.meanGeometricBy float 
 


// sample standard deviation n-1  23.70279
let stdevTv =
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.stDevBy float
    

// population standard deviation n  
let stdevPopTv =
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.stDevPopulationBy float


// Coefficient of Variation 0.0783
let cvTv =
    [5.; 5.5; 4.9; 4.85; 5.25; 5.05; 6.0;] 
    |> Seq.cv(*By float*)




type RunningStats<'T> = {
    N : int
    M1 : 'T
    M2 : 'T
    M3 : 'T
    M4 : 'T
}

let createRunningStats n m1 m2 m3 m4 =
    {N=n;M1=m1;M2=m2;M3=m3;M4=m4}


let inline ofSeq (items:seq<'T>) : RunningStats< 'U >  =
    use e = items.GetEnumerator()
    let zero  = LanguagePrimitives.GenericZero< 'U > 
    //let one   = LanguagePrimitives.GenericOne< 'U > 
                
    let rec loop n (m1:'U) (m2:'U) (m3:'U) (m4:'U) =
        match e.MoveNext() with
        | true  -> 

            let n' = n + 1
            let delta  = e.Current - m1       
            let delta_n  = LanguagePrimitives.DivideByInt< 'U > delta n'                       
            let m1'    = m1 + delta_n

            let delta2   = e.Current - m1'
            let term1    = delta * delta2            
            let delta_n2 = delta_n * delta_n

            let m2' = m2 + term1//delta * delta2

            let n'       = n + 1
            let delta    = e.Current - m1
            let delta_n  = LanguagePrimitives.DivideByInt< 'U > delta n'
            let delta_n2 = delta_n * delta_n
            

            let m1' = m1 + delta_n
            let m4' = m4 + (Ops.multByInt32 (term1 * delta_n2) (n'*n' - 3*n' + 3)) + (Ops.multByInt32 (delta_n2 * m2) 6) - (Ops.multByInt32 (delta_n * m3) 4)
            let m3' = m3 + (Ops.multByInt32 (term1 * m2) (n' - 2)) - (Ops.multByInt32 (delta_n * m2) 3 )
//                let m4' = m4 + (term1 * delta_n2 * (n'*n' - 3*n' + 3) + 6 * delta_n2 * m2 - 4 * delta_n * m3)
//                let m3' = m3 + (term1 * delta_n * (n' - 2) - 3 * delta_n * m2)
            

            printfn "m2: %f" m2'

            loop (n + 1) m1' m2' m3' m4' 
        | false -> 
            if (n > 1) then 
                createRunningStats n m1 m2 m3 m4 
            else
                let nanU = zero / zero
                createRunningStats n nanU nanU nanU nanU
    loop 0 zero zero zero zero

let inline var (rStats:RunningStats<'T>) = 
    LanguagePrimitives.DivideByInt rStats.M2 (rStats.N-1)

let stdevTv' =
    [10; 2; 19; 24; 6; 23; 47; 24; 54; 77;]
    |> Seq.map float
    |> ofSeq
    |> var
    




























//System.Math.Pow

let nv = Vector.init 10000 (fun _ -> Distributions.Continuous.Normal.Sample 0. 4.0)

nv |> Seq.stDevPopulation


Distributions.Bandwidth.nrd0 nv.Values



Quantile.interQuantileRange Quantile.nist nv.Values


let x = [0.1 .. 0.01 .. 1.0] 
let d = [|0.05;0.5;0.9|]

let y = 
    x 
    |> List.map (fun q -> Quantile.OfSorted.mode q  d )
    

let inline divByInt a b =
    LanguagePrimitives.DivideByInt a b


//divByInt 6 5



let firstNumber=5000
let secondeNumber=37

let inline decimalResult (a:'t) = 
    let ops = GlobalAssociations.TryGetNumericAssociation<'t>()
    ops.Value.Add(a,secondeNumber)
    



//type System.Int32 with
//    member this.DivideByInt a b = LanguagePrimitives.DivideByInt (float a) / b 

//System.Int32()





let q = 0.5
let h = (float 3) * q + 0.5

Array.quickSelectInPlace (int (ceil (h-0.5))) d


x |> List.map (fun q -> (float 3 * q + 1.) |> int)

Array.quickSelect 1 d

Quantile.mode 0.8  d

Chart.Point( y, x)
|> Chart.Show


let v = 
    vector [|2.0; 20.0; 1.|]


Vector.interval nv


Vector.median nv

Vector.stats nv

Array.median [|2.0; 20.0; 1.|]



let inline stDevPopulationOfMean mean (items:seq<'T>) : 'U  =
    use e = items.GetEnumerator()
    let rec loop n (acc) =
        match e.MoveNext() with
        | true  -> loop (n + 1) (acc + ((e.Current - mean) * (e.Current - mean)))
        | false -> if (n > 1) then sqrt(LanguagePrimitives.DivideByInt< 'U > acc (n )) else Unchecked.defaultof< 'U >            
    loop 0 LanguagePrimitives.GenericZero< 'U > 

let m1 = Seq.mean [1.;2.;3.;4.;]
let stDevPop = stDevPopulationOfMean m1 [1.;2.;3.;4.;]

let stDevPop' = Seq.stDevPopulation [1.;2.;3.;4.;]

(GlobalAssociations.ht.[typeof<float>])


let inline stDevPopulation (items:seq<'T>) : 'U  =
    use e = items.GetEnumerator()
    let zero = LanguagePrimitives.GenericZero< 'U > 
    let one = LanguagePrimitives.GenericOne< 'U >
    let rec loop n m1 m2 =
        match e.MoveNext() with
        | true  ->                         
            let delta  = e.Current - m1                                   
            let m1'    = m1 + (delta / n)
            let delta2   = e.Current - m1'
            let m2' = m2 + delta * delta2
            loop (n + one) m1' m2'
        | false -> if (LanguagePrimitives.GenericGreaterThan n one) then sqrt(m2 / (n-one)) else (zero / zero)
    loop one zero zero
    


stDevPopulation [1.;2.;3.;4.;]

let zero = LanguagePrimitives.GenericZero< float > 
let one = LanguagePrimitives.GenericOne< float >

(zero / zero)

LanguagePrimitives.GenericGreaterThan 2. 1.

