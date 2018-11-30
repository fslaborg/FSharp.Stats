module DotNetExtensions

open System.Collections.Generic

[<Extension>]
type List<'T> with
    member m.Front
        with get() = m.[0]
        and set(value : 'T) = m.[0] <- value
    member m.PopFront =
        let value = m.[0]
        m.RemoveAt(0)
        value
    member m.IsEmpty =
        m.Count <= 0