namespace FSharp.Stats.Interactive

module InteractiveConfig =

    module Matrix =

        let mutable MaxRows = 10
        let mutable MaxCols = 20
        let mutable ShowInfo = true
        let mutable ShowDimensions = true

    let Reset() =

        Matrix.MaxRows <- 10
        Matrix.MaxCols <- 20
        Matrix.ShowInfo <- true
        Matrix.ShowDimensions <- true
