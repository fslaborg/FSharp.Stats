namespace FSharp.Stats.Integration

module DefiniteIntegral =

    type IntegrationMethod = (float->float) -> float -> float -> float

    let midRect f (x:float) (h:float) = f (x + h/2.)

    let trapezium f (x:float) (h:float) = ( (f x) + f (x+h)) / 2.0

    let simpson f (x:float) (h:float) = (f x + 4. * f (x + h/2.) + f(x+h))/6.0

    let integrate (methode:IntegrationMethod) f a b steps =
        let h = (b - a ) / steps
        let rec loop acc i =
            if i >= steps then
                acc
            else
                let sum = methode f (a+i*h) h 
                loop (acc+sum) (i+1.)
        
        h * loop 0. 0.


// let f' (x:float) = x * x * x 
// integrate simpson f' 0. 1. 100.