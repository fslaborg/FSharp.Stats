namespace FSharp.Stats


/// Module to 
module Geometry =

    let hypot (a:float) (b:float) =
        let at = abs a
        let bt = abs b
        if at > bt then
            let ct = bt / at
            at * sqrt (1. + ct * ct) 
        elif (bt > 0.0) then
            let ct = at / bt
            bt * sqrt (1. + ct * ct)
        else
            0.0    