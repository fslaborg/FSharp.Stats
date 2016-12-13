// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "System.Numerics"
//#r @"C:\Users\muehl\Source\FSharp.Stats\packages\System.Numerics.Vectors\lib\net46\System.Numerics.Vectors.dll"
#r @"C:\Users\muehl\Source\FSharp.Stats\bin\FSharp.Stats\FSharp.Stats.dll"
open FSharp.Stats

let num = Library.hello 42
printfn "%i" num

Library.isAcc
