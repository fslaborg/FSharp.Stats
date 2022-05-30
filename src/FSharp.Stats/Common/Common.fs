namespace FSharp.Stats
open System
open System.Text
open System.IO

type IFsiFormattable =
    abstract Format : unit -> string
    abstract FormatWithInfo : unit -> string

type IMatrixFormattable =
    abstract InteractiveFormat : nRows:int * nCols:int -> string [] []
    abstract GetNumRows: unit -> int
    abstract GetNumCols: unit -> int

type IVectorFormattable =
    abstract InteractiveFormat : nVals:int -> string []
    abstract GetValueCount : unit -> int

type IRowVectorFormattable =
    abstract InteractiveFormat : nVals:int -> string []
    abstract GetValueCount : unit -> int

module Formatting = 
    
    module Matrix = 
        let mutable RowStartItemCount = 15
        let mutable RowEndItemCount = 15
        let mutable ColumnStartItemCount = 15
        let mutable ColumnEndItemCount = 15

    module Vector = ()
    module RowVector = ()

    let formatValue (value:'T) =
        match (box value) with
        | :? System.Double as f -> 
            match f with
            | f when f <= -10000. -> sprintf "%.2g" f
            | f when f < 0.     -> sprintf "%.2f" f
            | f when f >= 10000.  -> sprintf "%.2g" f
            | _    -> sprintf "%.3f" f
        | _ -> value.ToString()
          
    // Simple functions that pretty-print series and frames
    // (to be integrated as ToString and with F# Interactive)
    let formatTable (data:string[,]) =
        let sb = StringBuilder()
        use wr = new StringWriter(sb)

        let rows = data.GetLength(0)
        let columns = data.GetLength(1)
        let widths = Array.zeroCreate columns
        data |> Array2D.iteri (fun r c str ->
            widths.[c] <- max (widths.[c]) (str.Length))
        for r in 0 .. rows - 1 do
            for c in 0 .. columns - 1 do
                wr.Write(data.[r, c].PadLeft(widths.[c] + 1))
            wr.WriteLine()

        sb.ToString()