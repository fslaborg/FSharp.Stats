namespace FSharp.Stats.Testing

module PostHoc =    

    open TestStatistics
        
    type Contrast = { Index            : int;                      
                      L                : float;
                      DegreesOfFreedom : float;
                      MeanSquares      : float;
                      Significance     : float;                      
                      Statistic        : float;
                      SumOfSquares     : float;
                      }
    
    let createContrast index l degreesOfFreedom meanSquares significance statistic sumOfSquares =
        {Index = index; L = l; DegreesOfFreedom = degreesOfFreedom; MeanSquares = meanSquares; Significance = significance; Statistic = statistic; SumOfSquares = sumOfSquares;}

