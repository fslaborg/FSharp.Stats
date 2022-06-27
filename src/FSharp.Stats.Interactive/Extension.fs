namespace FSharp.Stats.Interactive

open System
open System.Threading.Tasks
open Microsoft.DotNet.Interactive
open Microsoft.DotNet.Interactive.Formatting
open FSharp.Stats


type FormatterKernelExtension() =

  interface IKernelExtension with
    member _.OnLoadAsync _ =
      Formatter.Register<IMatrixFormattable>(
        Action<_, _>
          (fun item (writer: IO.TextWriter) ->
            writer.Write(item |> FSharp.Stats.Interactive.Formatters.matrixToHtmlTable)),
        "text/html"
      )
      Task.CompletedTask