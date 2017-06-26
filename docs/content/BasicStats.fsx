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



let v = 
    vector [|2.0; 20.0; 1.|]


Vector.interval v




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

