namespace FSharp.Stats.Optimization

module Bisection = 
    
    ///Finds the value in an interval for which a given function returns a value close to 0 
    let tryFindRoot func accuracy lowerBound upperBound maxIter = 

        let rec loop a b i = 
            let c = (a + b)/2.
            let fc = func c
            if fc = 0. then 
                Some c
            elif (b - a)/2. < accuracy then 
                Some c
            else
                if i = maxIter then None
                else 
                    if sign fc = sign (func a) then loop c b (i+1)
                    else loop a c (i+1)

        let checkConditions a b = 
            let fa = func a
            let fb = func b
            if fa = 0. then Some a
            elif fb = 0. then Some b
            else 
                if fa < 0. then 
                    if fb > 0. then
                        loop a b 0
                    else None
                else 
                    if fb < 0. then 
                        loop a b 0
                    else None
        if lowerBound < upperBound then checkConditions lowerBound upperBound
        else checkConditions upperBound lowerBound
            

