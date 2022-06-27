namespace FSharp.Stats.Interactive
open FSharp.Stats

module Formatters =
    
    let matrixToHtmlTable (m: IMatrixFormattable) =

        let nRows = m.GetNumRows()
        let nCols = m.GetNumCols()

        let formattedStrings = 
            m.InteractiveFormat(
                InteractiveConfig.Matrix.MaxRows,
                InteractiveConfig.Matrix.MaxCols
            )
        let colIndices =
            formattedStrings 
            |> Array.take 1
            |> Array.map (fun row ->
                row
                |> Seq.map (fun v -> sprintf"<th>%s</th>" v)
                |> String.concat ""
            )
            |> Array.map (fun row -> sprintf "<thead>%s</thead>" row)
            |> String.concat ""

        let values = 
            formattedStrings 
            |> Array.skip 2
            |> Array.map (fun row ->
                row
                |> Seq.mapi (fun i v ->
                    match i with 
                    | 0 -> sprintf "<td><b>%s</b></td>" v
                    | 1 -> sprintf """<td class="no-wrap">%s</td>""" v
                    | _ -> sprintf "<td>%s</td>" v
                )
                |> String.concat ""
              )
              |> Seq.map (fun row -> sprintf "<tr>%s</tr>" row)
              |> String.concat ""

        let matrixInfo = $"Matrix of {nRows} rows x {nCols} columns"

        sprintf "<div>
<style scoped>,
  .dataframe tbody tr th:only-of-type {
    vertical-align: middle;
  }
  .dataframe tbody tr th {,
    vertical-align: top
  }
  .dataframe thead th {
    text-align: right;
  }
  .no-wrap {
    white-space: nowrap;
  }
</style>
<table border='1' class='dataframe'>
<thead>%s</thead>
<tbody>%s</tbody>
</table>
<br>
%s
</div>
"
            colIndices values matrixInfo