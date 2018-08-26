(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
//#I "../../bin"
#r "netstandard"
#r "../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/Fsharp.Plotly.dll"
#I "../../bin/FSharp.Stats.MSF/net47"
#r "FSharp.Stats.dll"
#r "Microsoft.Solver.Foundation.dll"
#r "FSharp.Stats.MSF.dll"
open FSharp.Plotly
open FSharp.Stats
(**

#Optimization

<a name="GradientDescent"></a>

##GradientDescent

<a name="LP"></a>

##LP

<a name="QP"></a>

##QP

<a name="QuasiNewton"></a>

##QuasiNewton


*)

//#r "FSharp.Stats.dll"
//open FSharp
open Microsoft.FSharp.Math


open FSharp.Stats.Optimization


// http://fsharpnews.blogspot.de/2011/01/gradient-descent.html

let rosenbrock (xs: vector) =
    let x, y = xs.[0], xs.[1]
    pown (1.0 - x) 2 + 100.0 * pown (y - pown x 2) 2


// The minimum at (1, 1) may be found quickly and easily using the functions defined above as follows:

//let xs =
//    vector[0.0; 0.0]
//    |> GradientDescent.minimize rosenbrock (GradientDescent.grad rosenbrock)



//Optimization.NelderMead.minimizeWith rosenbrock [|0.;0.|] [|0.;0.|] [|1.5;1.5|]


let d x = x**4. - x

let s = Optimization.NelderMead.minimizeSingleWith d 0. -1. 0.5


let x = vector [|-2. ..0.1.. 2.|]
let y = vector [|-2. ..0.1.. 2.|]

let rosen (x,y) =
    pown (1.0 - x) 2 + 100.0 * pown (y - pown x 2) 2


let z = 
    Array.init y.Length (fun i -> 
        Array.init x.Length (fun j -> rosen (x.[j], y.[i]) )
                    )


  
(*** define:rosenContour ***)
Chart.Surface(z,x,y)
|> Chart.withSize(600.,600.)
(*** include:rosenContour ***)




// we define a small number that we be used to calculate numerical approximations to derivatives:
let eps = System.Double.Epsilon ** (1.0 / 3.0)

// The following function repeatedly applies the given function to the given initial value until the result stops changing:
let rec fixedPoint f x =
    let f_x = f x
    if f_x = x then x else fixedPoint f f_x


// The numerical approximation to the grad of a scalar field is built up from partial derivatives in each direction:
let partialD f_xs f (xs : vector) i xi =
    xs.[i] <- xi + eps
    try (f xs - f_xs) / eps finally
    xs.[i] <- xi


// The following function performs a single iteration of gradient descent by scaling the step size lambda by either 'a' or 'b' if the result increases or decreases the function being minimized, respectively:
let descend a b f (f': _ -> vector) (lambda, xs, f_xs) =
    let xs_2 = xs - lambda * f' xs
    let f_xs_2 = f xs_2
    if f_xs_2 >= f_xs then
        a * lambda, xs, f_xs
    else
        b * lambda, xs_2, f_xs_2


/// gradient descent algorithm to minimize a given function and derivative
let minimize f f' xs =
    //let _, xs, _ = fixedPoint (descend 0.5 1.1 f f') (eps, xs, f xs)
    //xs
    fixedPoint (descend 0.5 1.1 f f') (eps, xs, f xs)

/// Computes a numerical approximation to the derivative of a function
let grad f xs =
    Vector.mapi (partialD (f xs) f xs) xs
   

let xs' =
    vector[0.0; 0.0]
    |> minimize rosenbrock (grad rosenbrock)

