module AccelerationTests

open Expecto
open System
open FSharp.Stats
open TestExtensions

open Expecto

[<Tests>]
let dotRangeTests =
  testList "dotRange tests" [

    test "Basic dot product with zero offset" {
      let a = [| 1.0; 2.0; 3.0 |]
      let b = [| 4.0; 5.0; 6.0 |]
      let result = Acceleration.SIMDRangeUtils.dotRange a 0 b 0 3
      Expect.floatClose Accuracy.high result 32.0 "Dot product mismatch"
    }

    test "Dot product with offset in first array" {
      let a = [| 0.0; 1.0; 2.0; 3.0 |]  // use 1.0, 2.0, 3.0
      let b = [| 4.0; 5.0; 6.0 |]
      let result = Acceleration.SIMDRangeUtils.dotRange a 1 b 0 3
      Expect.floatClose Accuracy.high result 32.0 "Offset dot mismatch"
    }

    test "Dot product with both arrays offset" {
      let a = [| 0.0; 1.0; 2.0; 3.0 |]
      let b = [| 0.0; 4.0; 5.0; 6.0 |]
      let result = Acceleration.SIMDRangeUtils.dotRange a 1 b 1 3
      Expect.floatClose Accuracy.high result 32.0 "Offset-dot mismatch"
    }

    test "Dot with zero-length" {
      let a = [| 1.0; 2.0; 3.0 |]
      let b = [| 4.0; 5.0; 6.0 |]
      let result = Acceleration.SIMDRangeUtils.dotRange a 0 b 0 0
      Expect.equal result 0.0 "Zero-length dot should be 0"
    }

    test "Out of bounds throws" {
      let a = [| 1.0; 2.0 |]
      let b = [| 1.0; 2.0 |]
      Expect.throws (fun () ->
        Acceleration.SIMDRangeUtils.dotRange a 1 b 1 2 |> ignore)
        "Should throw on out-of-bounds"
    }
  ]
