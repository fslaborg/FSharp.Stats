namespace FSharp.Stats.Optimization

module Brent = 
    open System

    /// (3.9 - sqrt(5)) / 2
    let private goldSectionRatio = 0.831966011250105
    
    /// TO-DO: Refactor to global (ops)
    let private doubleEpsilon = 1.11022302462516E-16

    /// Compute stepsize
    let private computeNewStep x v w fx fv fw tolAct lowerBound upperBound middleRange = 

        let newStep = 
            goldSectionRatio * (if x < middleRange then upperBound - x else lowerBound - x)
        let newStep' =

            if (abs (x - w) >= tolAct) then

                let t = (x - w) * (fx - fv)

                let (q,p) = 
                    let q' = (x - v) * (fx - fw)
                    let p' = (x - v) * q' - (x - w) * t
                    let q'' = 2. * (q' - t)
                    if (q'' > 0.) then 
                        q'',-p'
                    else 
                        -q'',p';

                if (abs (p) < abs (newStep * q) && 
                    p > q * (lowerBound - x + 2. * tolAct) && 
                    p < q * (upperBound - x - 2. * tolAct))          
                    then

                    p / q;                
                else 
                    newStep
            else 
                newStep                
        if abs newStep' < tolAct then 
            if newStep' > 0. then tolAct else - tolAct
        else 
            newStep'

    /// Finds the minimum in the given function between the lower and upper boundary with given tolerance via brent search. Returns None if maxiterations are reached.
    let minimizeWith f lowerBound upperBound tolerance maxIterations = 
        
        if Double.IsNaN lowerBound || Double.IsInfinity lowerBound then
            failwith "Brent-Method Error: Value lowerBound is either nan or infinite"

        if Double.IsNaN upperBound || Double.IsInfinity upperBound then
            failwith "Brent-Method Error: Value upperBound is either nan or infinite"

        if Double.IsNaN tolerance || Double.IsInfinity tolerance then
            failwith "Brent-Method Error: Value tolerance is either nan or infinite"

        if tolerance <= 0. then
            failwith "Brent-Method Error: Tolerance can't be smaller than or equal to zero"

        let (lowerBound,upperBound) = 
            if lowerBound > upperBound then upperBound,lowerBound else lowerBound,upperBound
        
        let v = lowerBound + goldSectionRatio * (upperBound - lowerBound)
        let fv = f v

        let rec mainLoop i x v w fx fv fw lowerBound upperBound =
            if i = maxIterations then 
                None
            else 
                let range = upperBound - lowerBound
                let middleRange = lowerBound / 2.0 + upperBound / 2.0;
                let tolAct = sqrt(doubleEpsilon) * abs(x) + tolerance / 3.;
                if (abs (x - middleRange) + range / 2. <= 2. * tolAct) then
                    Some x  
                else

                    let newStep =  computeNewStep x v w fx fv fw tolAct lowerBound upperBound middleRange           

                    let t = x + newStep
                    let ft = f t

                    if (Double.IsNaN ft || Double.IsInfinity ft) then
                        failwith "Brent-Method Error: function is not finite"

                    if ft <= fx then                
                        let (lowerBound',upperBound') =
                            if t < x then                         
                                lowerBound,x
                            else                         
                                x, upperBound  
                        mainLoop (i+1) t w x ft fw fx lowerBound' upperBound'

                    else                
                        let (lowerBound',upperBound') =
                            if t < x then 
                                t,upperBound
                            else 
                                lowerBound,t
                        if (ft <= fw || w = x) then
                            mainLoop (i+1) x w t fx fw ft lowerBound' upperBound'
                        elif (ft <= fv || v = x || v = w) then 
                            mainLoop (i+1) x t w fx ft fw lowerBound' upperBound'
                        else
                            mainLoop (i+1) x v w fx fv fw lowerBound' upperBound'
        //Initialize loop
        mainLoop 0 v v v fv fv fv lowerBound upperBound

    /// Finds the minimum in the given function between the lower and upper boundary with tolerance 10^-7 via brent search. Returns None if 100 iterations are reached.   
    let minimize f lowerBound upperBound = 
        minimizeWith f lowerBound upperBound 1.0E-7 100

    /// Finds the maximum in the given function between the lower and upper boundary with given tolerance via brent search. Returns None if maxiterations are reached.
    let maximizeWith f lowerBound upperBound tolerance maxIterations =
        let f' = fun x -> - f x
        minimizeWith f' lowerBound upperBound tolerance maxIterations

    /// Finds the maximum in the given function between the lower and upper boundary with tolerance 10^-7 via brent search. Returns None if 100 iterations are reached.   
    let maximize f lowerBound upperBound =
        let f' = fun x -> - f x
        minimizeWith f' lowerBound upperBound 1.0E-7 100