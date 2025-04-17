namespace FSharp.Stats

open System

// Fallback implementation container (Type class)
type VectorOps =


    static member inline Plus (a: Vector<'T>, b: Vector<'T>) = Vector.add a b
    static member inline Plus (a: Vector<'T>, b: 'T) = Vector.addScalar b a 
    static member inline Plus (a: 'T, b: Vector<'T>) = Vector.addScalar a b 

    static member inline Minus (a: Vector<'T>, b: Vector<'T>) = Vector.subtract a b
    static member inline Minus (a: Vector<'T>, b: 'T) = Vector.subtractScalar b a
 

    static member inline Multiply (a: Vector<'T>, b: Vector<'T>) = Vector.multiply a b
    static member inline Multiply (a: Vector<'T>, b: 'T) = Vector.multiplyScalar b a
    static member inline Multiply (a: 'T, b: Vector<'T>) = Vector.multiplyScalar a b

    static member inline Divide (a: Vector<'T>, b: Vector<'T>) = Vector.divide a b
    static member inline Divide (a: Vector<'T>, b: 'T) = Vector.divideScalar b a 

    static member inline Power (a: Vector<'T>, power: 'T) = Vector.pow power a


// Dispatcher types (Instance resolution)
type Plus = static member inline Invoke (a: ^A, b: ^B) = ((^A or ^B or VectorOps) : (static member Plus : ^A * ^B -> _)(a, b))
type Minus = static member inline Invoke (a: ^A, b: ^B) = ((^A or ^B or VectorOps) : (static member Minus : ^A * ^B -> _)(a, b))
type Multiply = static member inline Invoke (a: ^A, b: ^B) = ((^A or ^B or VectorOps) : (static member Multiply : ^A * ^B -> _)(a, b))
type Divide = static member inline Invoke (a: ^A, b: ^B) = ((^A or ^B or VectorOps) : (static member Divide : ^A * ^B -> _)(a, b))
type Power = static member inline Invoke (a: ^A, b: ^B) = ((^A or ^B or VectorOps) : (static member Power : ^A * ^B -> _)(a, b))

// Operators
[<AutoOpen>]
module VectorOpsSymbols =
    let inline (.+) a b = Plus.Invoke(a, b)
    let inline (.-) a b = Minus.Invoke(a, b)
    let inline (.*) a b = Multiply.Invoke(a, b)
    let inline (./) a b = Divide.Invoke(a, b)
    let inline (.^) a b = Power.Invoke(a, b)
    // Dot product ( @ )


//// A type extension on `'T[]` so you can do `myArray.Transposed`.
//module ArrayExtensions =

//    type Array with
        
//        member this.Transpose
            
//            with get () =
//                RowVector<'T>(this)

