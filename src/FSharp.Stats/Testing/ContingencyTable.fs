namespace FSharp.Stats.Testing

open System
open System.Collections.Generic
open FSharp.Stats
open FsMath

/// An estimate with its standard‐error uncertainty.
type UncertainValue = {
    Estimate    : float
    Uncertainty : float
}


///// A contingency-specific interface on top of ITable<_,_,int>
//type IContingencyTable<'R,'C> =
//  inherit ITable<'R,'C,int>
//  /// Sum of counts in one row
//  abstract RowTotal       : 'R -> int
//  /// Sum of counts in one column
//  abstract ColumnTotal    : 'C -> int
//  /// Grand total
//  abstract Total          : unit -> int
//  ///// Probability of a single cell (with uncertainty)
//  //abstract ProbabilityOf  : 'R * 'C -> UncertainValue


/// Alias: a contingency table is just a Table<'R,'C,int>
type ContingencyTable<'R,'C when 'R : comparison and 'C : comparison> = Table<'R,'C,int>


/// A strictly 2×2 contingency table with generic row and column labels.
type Contingency2x2<'R,'C when 'R : comparison and 'C : comparison>
  ( rowKeys : 'R * 'R,
    colKeys : 'C * 'C,
    counts  : int * int * int * int ) =

  let (r1, r2) = rowKeys
  let (c1, c2) = colKeys
  let (a,  b,  c,  d)  = counts

  do  if a<0 || b<0 || c<0 || d<0 then
        invalidArg "counts" "All cell counts must be non‐negative"

  /// First row label
  member _.Row1 = r1
  /// Second row label
  member _.Row2 = r2
  /// First column label
  member _.Col1 = c1
  /// Second column label
  member _.Col2 = c2

  /// Cell a = (r1,c1)
  member _.A = a
  /// Cell b = (r1,c2)
  member _.B = b
  /// Cell c = (r2,c1)
  member _.C = c
  /// Cell d = (r2,c2)
  member _.D = d

  /// Index by zero‐based coordinates  0..1 × 0..1
  member _.Item
    with get(i,j) =
      match i,j with
      | 0,0 -> a | 0,1 -> b
      | 1,0 -> c | 1,1 -> d
      | _   -> invalidArg "i,j" "Indices must be 0 or 1"

  /// Index by labels
  member this.Item
    with get(row: 'R, col: 'C) =
      match (row,col) with
      | (x,y) when x = r1 && y = c1 -> a
      | (x,y) when x = r1 && y = c2 -> b
      | (x,y) when x = r2 && y = c1 -> c
      | (x,y) when x = r2 && y = c2 -> d
      | _ -> invalidArg "r,c" "Invalid row or column label"

  //interface IContingencyTable<'R,'C> with
  //  member _.RowKeys       = [| r1; r2 |]
  //  member _.ColKeys       = [| c1; c2 |]
  //  member _.NumRows       = 2
  //  member _.NumCols       = 2
  //  member this.Get(row,col)   = this.[row,col]
  //  member this.GetRow(row)  = [| c1, this.[row,c1]; c2, this.[row,c2] |]
  //  member this.GetColumn(col)= [| r1, this.[r1,col]; r2, this.[r2,col] |]
  //  member _.RowTotal(row)   = if row = r1 then a+b else c+d
  //  member _.ColumnTotal(col)= if col = c1 then a+c else b+d
  //  member _.Total()       = a + b + c + d

  /// Odds ratio = (a·d)/(b·c)
  member _.OddsRatio() =
    if b = 0 || c = 0 then
      invalidOp "Cannot compute odds‐ratio with a zero cell in denominator."
    float a * float d / (float b * float c)



module Contingency =

  let create (rows: 'R []) (cols: 'C []) (data: int[]) : ContingencyTable<'R,'C> =
    // validate shapes
    if rows.Length * cols.Length <> data.Length then
      invalidArg "data" $"Length of data ({data.Length}) does not match rows × cols ({rows.Length}×{cols.Length})"
    // build the table
    Table( rows, cols, Matrix(rows.Length, cols.Length, data) )

  /// Create a zero‐filled table for the given row & column labels.
  let zeroCreate (rows: 'R []) (cols: 'C []) : ContingencyTable<'R,'C> =
    // zero‐filled flat data
    let data = Matrix.zeroCreate<int> rows.Length cols.Length
    Table( rows, cols, data )


  /// Build a ContingencyTable<'R,'C> of counts from a sequence of category‐pairs.
  let ofDataSeq
      (rows: seq<'R>)
      (cols: seq<'C>)
      (data: seq<'R * 'C>)
      : ContingencyTable<'R,'C> =

    // 1) Materialize the label sets
    let rowKeys = rows |> Seq.distinct |> Seq.toArray
    let colKeys = cols |> Seq.distinct |> Seq.toArray

    // 2) Start with all zeros
    let table = zeroCreate rowKeys colKeys

    // 3) Tally each occurrence
    for (r,c) in data do
      let curr = table.[r,c]
      table.[r, c] <- curr + 1

    table

  /// Build from a 2D integer array (rows × columns), zero‐based.
  /// Throws if any entry is negative.
  let ofArray (data2d: int[,]) : ContingencyTable<int,int> =
    let numRows = data2d.GetLength 0
    let numCols = data2d.GetLength 1
    // flatten row-major
    let flat = Array.zeroCreate<int> (numRows * numCols)
    for i in 0..numRows-1 do
      for j in 0..numCols-1 do
        let v = data2d.[i,j]
        if v < 0 then
          invalidArg "data2d" $"Negative entry at (%d{i},%d{j})"
        flat.[i * numCols + j] <- v
    let matrix = Matrix(numRows,numCols,flat)
    Table( Array.init numRows id, Array.init numCols id, matrix )

  /// Get the count in cell (r,c).  Throws if (r,c) not a valid key.
  let getCount (r:'R) (c:'C) (t:ContingencyTable<'R,'C>) =
    t.[r, c]

  /// Set the count in (r,c) to a non‐negative value, returning a new table.
  let setCount (r:'R) (c:'C) (value:int) (t:ContingencyTable<'R,'C>) =
    if value < 0 then invalidArg "value" "Count must be ≥0"
    // clone the flat array, update the one entry
    let data' = Array.copy t.Data
    // find the zero-based indices
    let i = Array.findIndex ((=) r) t.RowKeys
    let j = Array.findIndex ((=) c) t.ColKeys
    data'.[i * t.NumCols + j] <- value
    Table( t.RowKeys, t.ColKeys, Matrix(t.RowKeys.Length,t.ColKeys.Length, data') )

  /// Increment cell (r,c) by 1.
  let increment (r:'R) (c:'C) (t:ContingencyTable<'R,'C>) =
    let old = getCount r c t
    setCount r c (old + 1) t

  /// Decrement cell (r,c) by 1 (throws if it would go below zero).
  let decrement (r:'R) (c:'C) (t:ContingencyTable<'R,'C>) =
    let old = getCount r c t
    if old < 1 then invalidOp "Cell count is already zero."
    setCount r c (old - 1) t

  /// Sum of counts in a given row.
  let rowTotal (r:'R) (t:ContingencyTable<'R,'C>) =
    t.ColKeys |> Array.sumBy (fun c -> getCount r c t)

  /// Sum of counts in a given column.
  let columnTotal (c:'C) (t:ContingencyTable<'R,'C>) =
    t.RowKeys |> Array.sumBy (fun r -> getCount r c t)

  /// Total count in the whole table.
  let total (t:ContingencyTable<'R,'C>) =
    t.RowKeys |> Array.sumBy (fun r -> rowTotal r t)


  /// Uncertainty = sqrt(p*(1-p)/N), binomial approximation.
  let private uncertainty (p:float) (N:float) =
    sqrt (p * (1.0 - p) / N)

  /// Probability of a single cell.
  let probabilityOf (r:'R) (c:'C) (t:ContingencyTable<'R,'C>) : UncertainValue =
    let n = float (getCount r c t)
    let N = float (total t)
    let p = n / N
    { Estimate    = p; Uncertainty = uncertainty p N }

  /// Probability of a row.
  let probabilityOfRow (r:'R) (t:ContingencyTable<'R,'C>) : UncertainValue =
    let n = float (rowTotal r t)
    let N = float (total t)
    let p = n / N
    { Estimate    = p; Uncertainty = uncertainty p N }

  /// Probability of a column.
  let probabilityOfColumn (c:'C) (t:ContingencyTable<'R,'C>) : UncertainValue =
    let n = float (columnTotal c t)
    let N = float (total t)
    let p = n / N
    { Estimate    = p; Uncertainty = uncertainty p N }

  /// Probability of row given column.
  let probabilityRowGivenColumn (r:'R) (c:'C) (t:ContingencyTable<'R,'C>) : UncertainValue =
    let nrc = float (getCount r c t)
    let Nc  = float (columnTotal c t)
    let p = nrc / Nc
    { Estimate    = p; Uncertainty = uncertainty p Nc }

  /// Probability of column given row.
  let probabilityColumnGivenRow (c:'C) (r:'R) (t:ContingencyTable<'R,'C>) : UncertainValue =
    let nrc = float (getCount r c t)
    let Nr  = float (rowTotal r t)
    let p = nrc / Nr
    { Estimate    = p; Uncertainty = uncertainty p Nr }

  /// Ensure it's 2×2; throws otherwise.
  let ensure2x2 (t:ContingencyTable<'R,'C>) =
    if t.NumRows <> 2 || t.NumCols <> 2 then
      invalidArg "t" "ContingencyTable must be 2×2."
    t

  let as2x2 (t: Table<'R,'C,int>) : Contingency2x2<'R,'C> =
    if t.NumRows <> 2 || t.NumCols <> 2 then
        invalidArg "t" "Must be 2×2"
    let rkeys = t.RowKeys.[0], t.RowKeys.[1]
    let ckeys = t.ColKeys.[0], t.ColKeys.[1]
    let a = t.DataMatrix.[0,0]
    let b = t.DataMatrix.[0,1]
    let c = t.DataMatrix.[1,0]
    let d = t.DataMatrix.[1,1]
    Contingency2x2( rowKeys = rkeys, colKeys = ckeys, counts = (a,b,c,d))