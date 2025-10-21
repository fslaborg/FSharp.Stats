namespace FSharp.Stats


open System
open FSharp.Stats
open FsMath
open FsMath.Algebra

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Numerics
open System.Text


/// A general read-only 2D table abstraction.
type ITable<'R,'C,'T> =
  /// All row labels (in order).
  abstract RowKeys   : 'R[]
  /// All column labels (in order).
  abstract ColKeys   : 'C[]
  /// Number of rows
  abstract NumRows   : int
  /// Number of columns
  abstract NumCols   : int
  /// Get the value at (rowLabel, colLabel)
  abstract Get       : rowLabel:'R * colLabel:'C -> 'T
  /// Enumerate a single row as (colLabel * value) seq
  abstract GetRow    : rowLabel:'R -> seq<'C * 'T>
  /// Enumerate a single column as (rowLabel * value) seq
  abstract GetColumn : colLabel:'C -> seq<'R * 'T>

/// A read/write extension (if you need mutability)
type ITableMutable<'R,'C,'T> =
  inherit ITable<'R,'C,'T>
  /// Set the value at (rowLabel, colLabel)
  abstract Set       : rowLabel:'R * colLabel:'C * value:'T -> unit








/// A 2D table with generic row‐labels and column‐labels,
/// backed by a flattened Matrix<'T>
type Table<'R,'C,'T
    when 'R : comparison
     and 'C : comparison
     and 'T :> Numerics.INumber<'T>
     and 'T : (new: unit -> 'T)
     and 'T : struct
     and 'T : comparison
     and 'T :> ValueType>
  ( rowKeys : 'R[],
    colKeys : 'C[],
    dataMatrix   : Matrix<'T> ) =

  // validate shapes
  do
    if rowKeys.Length <> dataMatrix.NumRows then
      invalidArg "rowKeys" $"Length of rowKeys ({rowKeys.Length}) <> data.NumRows ({dataMatrix.NumRows})"
    if colKeys.Length <> dataMatrix.NumCols then
      invalidArg "colKeys" $"Length of colKeys ({colKeys.Length}) <> data.NumCols ({dataMatrix.NumCols})"

  // build fast lookup maps
  let rowIndexMap =
    rowKeys |> Array.mapi (fun i k -> k,i) |> dict<'R,int>
  let colIndexMap =
    colKeys |> Array.mapi (fun j k -> k,j) |> dict<'C,int>

  /// All row labels in order.
  member _.RowKeys = rowKeys

  /// All column labels in order.
  member _.ColKeys = colKeys

  /// Underlying matrix storage.
  member _.DataMatrix = dataMatrix

  /// Underlying data storage.
  member _.Data = dataMatrix.Data

  /// Number of rows.
  member _.NumRows = rowKeys.Length

  /// Number of columns.
  member _.NumCols = colKeys.Length

  /// Indexer by integer coordinates (0-based).
  member this.Item
    with get (i:int, j:int) =
      dataMatrix.[i, j]
    and set (i:int, j:int) (v:'T) =
      dataMatrix.[i, j] <- v

  /// Indexer by label keys.
  member this.Item
    with get (r:'R, c:'C) =
      let i = 
        match rowIndexMap.TryGetValue r with
        | true, idx -> idx
        | _         -> invalidArg "r" $"Invalid row label: {r}"
      let j =
        match colIndexMap.TryGetValue c with
        | true, idx -> idx
        | _         -> invalidArg "c" $"Invalid column label: {c}"
      dataMatrix.[i, j]
    and set (r:'R, c:'C) (v:'T) =
      let i = 
        match rowIndexMap.TryGetValue r with
        | true, idx -> idx
        | _         -> invalidArg "r" $"Invalid row label: {r}"
      let j =
        match colIndexMap.TryGetValue c with
        | true, idx -> idx
        | _         -> invalidArg "c" $"Invalid column label: {c}"
      dataMatrix.[i, j] <- v

  /// Slice out a row (as array of label * value).
  member this.GetRow (r:'R) : ('C * 'T)[] =
    // ToDo: leaverage the Matrix based Matrix.getRows for accelaration
    let i =
      match rowIndexMap.TryGetValue r with
      | true, idx -> idx
      | _         -> invalidArg "r" $"Invalid row label: {r}"
    colKeys
    |> Array.mapi (fun j c -> c, dataMatrix.[i,j])

  /// Slice out a column (as array of label * value).
  member this.GetColumn (c:'C) : ('R * 'T)[] =
    let j =
      match colIndexMap.TryGetValue c with
      | true, idx -> idx
      | _         -> invalidArg "c" $"Invalid column label: {c}"
    rowKeys
    |> Array.mapi (fun i r -> r, dataMatrix.[i,j])

  /// Pretty‐print with headers and labels.
  override this.ToString() =
    let sb = StringBuilder()
    
    // Header row: blank corner + column keys
    sb.Append("\t") |> ignore
    for c in colKeys do
      sb.Append(c.ToString()).Append("\t") |> ignore
    sb.AppendLine() |> ignore

    // Each data row: row key + its values
    for i in 0 .. this.NumRows - 1 do
      let r = rowKeys.[i]
      sb.Append(r.ToString()).Append("\t") |> ignore
      for j in 0 .. this.NumCols - 1 do
        sb.Append(dataMatrix.[i, j].ToString()).Append("\t") |> ignore
      sb.AppendLine() |> ignore

    sb.ToString()

