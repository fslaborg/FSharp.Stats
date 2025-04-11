module VectorTests 

open System
open Expecto
open FSharp.Stats

[<Tests>]
let vectorTests =
    testList "Vector Tests" [
        
        // ======================================================================
        // INT TESTS
        // ======================================================================
        testList "Integer Vector Tests" [

            // ------------------------------------------------------------------
            // Add
            // ------------------------------------------------------------------
            testCase "add: integer vectors of same length" <| fun _ ->
                let v1 = [| 1; 2; 3 |]
                let v2 = [| 4; 5; 6 |]
                let result = Vector.add v1 v2
                let expected = [| 5; 7; 9 |]
                Expect.equal result expected "Should add each element pairwise"

            testCase "add: throws on dimension mismatch" <| fun _ ->
                let v1 = [| 1; 2 |]
                let v2 = [| 1; 2; 3 |]
                Expect.throwsT<ArgumentException> (fun () -> Vector.add v1 v2 |> ignore)
                    "Should throw ArgumentException when vectors differ in length"

            // ------------------------------------------------------------------
            // Subtract
            // ------------------------------------------------------------------
            testCase "subtract: integer vectors of same length" <| fun _ ->
                let v1 = [| 5; 5; 5 |]
                let v2 = [| 1; 2; 3 |]
                let result = Vector.subtract v1 v2
                let expected = [| 4; 3; 2 |]
                Expect.equal result expected "Should subtract each element pairwise"

            testCase "subtract: throws on dimension mismatch" <| fun _ ->
                let v1 = [| 1; 2; 3 |]
                let v2 = [| 1; 2 |]
                Expect.throwsT<ArgumentException> (fun () -> Vector.subtract v1 v2 |> ignore)
                    "Should throw ArgumentException when vectors differ in length"

            // ------------------------------------------------------------------
            // Multiply
            // ------------------------------------------------------------------
            testCase "multiply: integer vectors of same length" <| fun _ ->
                let v1 = [| 2; 2; 2 |]
                let v2 = [| 3; 4; 5 |]
                let result = Vector.multiply v1 v2
                let expected = [| 6; 8; 10 |]
                Expect.equal result expected "Should multiply each element pairwise"

            testCase "multiply: throws on dimension mismatch" <| fun _ ->
                let v1 = [| 2; 2 |]
                let v2 = [| 3; 4; 5 |]
                Expect.throwsT<ArgumentException> (fun () -> Vector.multiply v1 v2 |> ignore)
                    "Should throw ArgumentException when vectors differ in length"

            // ------------------------------------------------------------------
            // Divide
            // ------------------------------------------------------------------
            testCase "divide: integer vectors of same length" <| fun _ ->
                let v1 = [| 10; 20; 30 |]
                let v2 = [| 2; 5; 5 |]
                let result = Vector.divide v1 v2
                let expected = [| 5; 4; 6 |]
                Expect.equal result expected "Should divide each element pairwise"

            testCase "divide: throws on dimension mismatch" <| fun _ ->
                let v1 = [| 10; 20 |]
                let v2 = [| 2; 5; 5 |]
                Expect.throwsT<ArgumentException> (fun () -> Vector.divide v1 v2 |> ignore)
                    "Should throw ArgumentException when vectors differ in length"

            // ------------------------------------------------------------------
            // Scalar Operations
            // ------------------------------------------------------------------
            testCase "addScalar: adds scalar to every element" <| fun _ ->
                let v = [| 1; 2; 3 |]
                let scalar = 5
                let result = Vector.addScalar v scalar
                let expected = [| 6; 7; 8 |]
                Expect.equal result expected "Should add the scalar to each element"

            testCase "subtractScalar: subtracts scalar from every element" <| fun _ ->
                let v = [| 1; 2; 3 |]
                let scalar = 1
                let result = Vector.subtractScalar v scalar
                let expected = [| 0; 1; 2 |]
                Expect.equal result expected "Should subtract the scalar from each element"

            testCase "multiplyScalar: multiplies scalar to every element" <| fun _ ->
                let v = [| 2; 4; 6 |]
                let scalar = 3
                let result = Vector.multiplyScalar v scalar
                let expected = [| 6; 12; 18 |]
                Expect.equal result expected "Should multiply the scalar with each element"

            testCase "divideScalar: divides every element by scalar" <| fun _ ->
                let v = [| 10; 20; 30 |]
                let scalar = 10
                let result = Vector.divideScalar v scalar
                let expected = [| 1; 2; 3 |]
                Expect.equal result expected "Should divide each element by the scalar"

            // ------------------------------------------------------------------
            // sum
            // ------------------------------------------------------------------
            testCase "sum: sums all elements of an integer vector" <| fun _ ->
                let v = [| 1; 2; 3; 4 |]
                let result = Vector.sum v
                Expect.equal result 10 "Should be the sum of all elements"

            testCase "sum: sums an empty integer vector (should be 0)" <| fun _ ->
                let v : int[] = [||]
                let result = Vector.sum v
                Expect.equal result 0 "Sum of empty array should be 0"

            // ------------------------------------------------------------------
            // product
            // ------------------------------------------------------------------
            testCase "product: multiplies all elements of an integer vector" <| fun _ ->
                let v = [| 1; 2; 3; 4 |]
                let result = Vector.product v
                Expect.equal result 24 "Should be the product of all elements"

            testCase "product: empty integer vector (should be 1 for identity)" <| fun _ ->
                let v : int[] = [||]
                let result = Vector.product v
                Expect.equal result 1 "Product of empty array should be 1"


            // ------------------------------------------------------------------
            // dot
            // ------------------------------------------------------------------
            testCase "dot: computes dot product of two integer vectors" <| fun _ ->
                let v1 = [| 1; 2; 3 |]
                let v2 = [| 4; 5; 6 |]
                let result = Vector.dot v1 v2
                // Dot = 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
                Expect.equal result 32 "Dot product should match expected"

            testCase "dot: throws on dimension mismatch" <| fun _ ->
                let v1 = [| 1; 2 |]
                let v2 = [| 3; 4; 5 |]
                Expect.throwsT<ArgumentException> (fun () -> Vector.dot v1 v2 |> ignore)
                    "Should throw ArgumentException when vectors differ in length"


            // ------------------------------------------------------------------
            // min
            // ------------------------------------------------------------------
            testCase "min: finds minimum in a non-empty integer vector" <| fun _ ->
                let v = [| 10; 2; 30; 4 |]
                let result = Vector.min v
                Expect.equal result 2 "Should find the smallest integer in the array"

            testCase "min: throws on empty integer vector" <| fun _ ->
                let v : int[] = [||]
                Expect.throwsT<ArgumentException> (fun () -> Vector.min v |> ignore)
                    "Should throw ArgumentException for empty vector"

            // ------------------------------------------------------------------
            // max
            // ------------------------------------------------------------------
            testCase "max: finds maximum in a non-empty integer vector" <| fun _ ->
                let v = [| 10; 2; 30; 4 |]
                let result = Vector.max v
                Expect.equal result 30 "Should find the largest integer in the array"

            testCase "max: throws on empty integer vector" <| fun _ ->
                let v : int[] = [||]
                Expect.throwsT<ArgumentException> (fun () -> Vector.max v |> ignore)
                    "Should throw ArgumentException for empty vector"

            // ------------------------------------------------------------------
            // OPERATOR TESTS (Int)
            // ------------------------------------------------------------------
            testCase "vector-vector operators (int): .+ .- .* ./" <| fun _ ->
                let v1 = [| 1; 2; 3 |]
                let v2 = [| 4; 5; 6 |]

                // .+
                let addResult = v1 .+ v2
                Expect.equal addResult [|5; 7; 9|] "v1 .+ v2 should match element-wise add"

                // .-
                let subResult = v1 .- v2
                Expect.equal subResult [|-3; -3; -3|] "v1 .- v2 should match element-wise subtract"

                // .*
                let mulResult = v1 .* v2
                Expect.equal mulResult [|4; 10; 18|] "v1 .* v2 should match element-wise multiply"

                // ./ 
                let divResult = [| 10; 20; 30 |] ./ [| 2; 5; 5 |]
                Expect.equal divResult [|5; 4; 6|] "Should match element-wise division"

            testCase "vector-scalar operators (int): .+ .- .* ./" <| fun _ ->
                let v = [| 2; 4; 6 |]

                // v .+ scalar
                let addScalarResult = v .+ 2
                Expect.equal addScalarResult [|4; 6; 8|] "v .+ 2 should add 2 to all elements"

                // v .- scalar
                let subScalarResult = v .- 2
                Expect.equal subScalarResult [|0; 2; 4|] "v .- 2 should subtract 2 from all elements"

                // v .* scalar
                let mulScalarResult = v .* 3
                Expect.equal mulScalarResult [|6; 12; 18|] "v .* 3 should multiply all elements by 3"

                // v ./ scalar
                let divScalarResult = [| 10; 20; 30 |] ./ 10
                Expect.equal divScalarResult [|1; 2; 3|] "Should divide all elements by 10"
        ]


        // ======================================================================
        // FLOAT TESTS
        // ======================================================================
        testList "Float Vector Tests" [

            // ------------------------------------------------------------------
            // Add
            // ------------------------------------------------------------------
            testCase "add: float vectors of same length" <| fun _ ->
                let v1 = [| 1.0; 2.0; 3.0 |]
                let v2 = [| 4.0; 5.0; 6.0 |]
                let result = Vector.add v1 v2
                let expected = [| 5.0; 7.0; 9.0 |]
                Expect.equal result expected "Should add each element pairwise"

            testCase "add: throws on dimension mismatch" <| fun _ ->
                let v1 = [| 1.0; 2.0 |]
                let v2 = [| 1.0; 2.0; 3.0 |]
                Expect.throwsT<ArgumentException> (fun () -> Vector.add v1 v2 |> ignore)
                    "Should throw ArgumentException when vectors differ in length"

            // ------------------------------------------------------------------
            // Subtract
            // ------------------------------------------------------------------
            testCase "subtract: float vectors of same length" <| fun _ ->
                let v1 = [| 5.5; 5.0; 5.75 |]
                let v2 = [| 1.5; 2.0; 3.25 |]
                let result = Vector.subtract v1 v2
                let expected = [| 4.0; 3.0; 2.5 |]
                Expect.equal result expected "Should subtract each element pairwise"

            // ------------------------------------------------------------------
            // Multiply
            // ------------------------------------------------------------------
            testCase "multiply: float vectors of same length" <| fun _ ->
                let v1 = [| 2.0; 2.5; 3.0 |]
                let v2 = [| 3.0; 4.0; 5.0 |]
                let result = Vector.multiply v1 v2
                let expected = [| 6.0; 10.0; 15.0 |]
                Expect.equal result expected "Should multiply each element pairwise"

            // ------------------------------------------------------------------
            // Divide
            // ------------------------------------------------------------------
            testCase "divide: float vectors of same length" <| fun _ ->
                let v1 = [| 10.0; 20.0; 30.0 |]
                let v2 = [| 2.0; 5.0; 5.0 |]
                let result = Vector.divide v1 v2
                let expected = [| 5.0; 4.0; 6.0 |]
                Expect.equal result expected "Should divide each element pairwise"

            // ------------------------------------------------------------------
            // Scalar Operations
            // ------------------------------------------------------------------
            testCase "addScalar: adds scalar to every float element" <| fun _ ->
                let v = [| 1.0; 2.0; 3.0 |]
                let scalar = 5.5
                let result = Vector.addScalar v scalar
                let expected = [| 6.5; 7.5; 8.5 |]
                Expect.equal result expected "Should add 5.5 to each element"

            testCase "subtractScalar: subtract scalar from every float element" <| fun _ ->
                let v = [| 1.0; 2.5; 3.75 |]
                let scalar = 1.25
                let result = Vector.subtractScalar v scalar
                let expected = [| -0.25; 1.25; 2.5 |]
                Expect.equal result expected "Should subtract 1.25 from each element"

            testCase "multiplyScalar: multiplies scalar with every float element" <| fun _ ->
                let v = [| 2.5; 4.0; 6.0 |]
                let scalar = 2.0
                let result = Vector.multiplyScalar v scalar
                let expected = [| 5.0; 8.0; 12.0 |]
                Expect.equal result expected "Should multiply each element by 2.0"

            testCase "divideScalar: divides every float element by scalar" <| fun _ ->
                let v = [| 10.0; 20.0; 30.0 |]
                let scalar = 2.0
                let result = Vector.divideScalar v scalar
                let expected = [| 5.0; 10.0; 15.0 |]
                Expect.equal result expected "Should divide each element by 2.0"

            // ------------------------------------------------------------------
            // sum
            // ------------------------------------------------------------------
            testCase "sum: sums all elements of a float vector" <| fun _ ->
                let v = [| 1.1; 2.2; 3.3; 4.4 |]
                let result = Vector.sum v
                // Expect exact or near: 1.1 + 2.2 + 3.3 + 4.4 = 11.0
                Expect.floatClose Accuracy.veryHigh 11.0 result "Sum of [1.1;2.2;3.3;4.4] = 11.0"

            testCase "sum: sums empty float vector (should be 0.0)" <| fun _ ->
                let v : float[] = [||]
                let result = Vector.sum v
                Expect.equal result 0.0 "Sum of empty array should be 0.0"

            // ------------------------------------------------------------------
            // product
            // ------------------------------------------------------------------
            testCase "product: multiplies all elements of a float vector" <| fun _ ->
                let v = [| 1.0; 2.0; 3.0 |]
                let result = Vector.product v
                Expect.equal result 6.0 "1.0 * 2.0 * 3.0 = 6.0"

            testCase "product: empty float vector" <| fun _ ->
                // By definition in your code, fold starts with 1. 
                let v : float[] = [||]
                let result = Vector.product v
                Expect.equal result 1.0 "Product of empty array should be 1.0"

            // ------------------------------------------------------------------
            // mean
            // ------------------------------------------------------------------
            testCase "mean: computes mean of a float vector" <| fun _ ->
                let v = [| 2.0; 4.0; 6.0; 8.0 |]
                let result = Vector.mean v
                // (2.0 + 4.0 + 6.0 + 8.0) / 4 = 5.0
                Expect.floatClose Accuracy.veryHigh 5.0 result "Mean should be 5.0"

            testCase "mean: throws on empty float vector" <| fun _ ->
                let v : float[] = [||]
                Expect.throwsT<ArgumentException> (fun () -> Vector.mean v |> ignore)
                    "Should throw ArgumentException when float vector is empty"

            // ------------------------------------------------------------------
            // dot
            // ------------------------------------------------------------------
            testCase "dot: computes dot product of two float vectors" <| fun _ ->
                let v1 = [| 1.0; 2.0; 3.0 |]
                let v2 = [| 4.0; 5.0; 6.0 |]
                let result = Vector.dot v1 v2
                // Dot = 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
                Expect.floatClose Accuracy.veryHigh 32.0 result "Dot product should be 32.0"

            // ------------------------------------------------------------------
            // norm
            // ------------------------------------------------------------------
            testCase "norm: computes Euclidean norm of a 3-4-5 triangle (float)" <| fun _ ->
                let v = [| 3.0; 4.0 |]
                let result = Vector.norm v
                Expect.floatClose Accuracy.veryHigh 5.0 result "Norm of [3.0;4.0] should be 5.0"

            // ------------------------------------------------------------------
            // min
            // ------------------------------------------------------------------
            testCase "min: finds minimum in a non-empty float vector" <| fun _ ->
                let v = [| 10.0; 2.0; 30.5; 4.1 |]
                let result = Vector.min v
                Expect.equal result 2.0 "Should find the smallest float in the array"

            testCase "min: throws on empty float vector" <| fun _ ->
                let v : float[] = [||]
                Expect.throwsT<ArgumentException> (fun () -> Vector.min v |> ignore)
                    "Should throw ArgumentException for empty vector"

            // ------------------------------------------------------------------
            // max
            // ------------------------------------------------------------------
            testCase "max: finds maximum in a non-empty float vector" <| fun _ ->
                let v = [| 10.0; 2.0; 30.5; 4.1 |]
                let result = Vector.max v
                Expect.equal result 30.5 "Should find the largest float in the array"

            testCase "max: throws on empty float vector" <| fun _ ->
                let v : float[] = [||]
                Expect.throwsT<ArgumentException> (fun () -> Vector.max v |> ignore)
                    "Should throw ArgumentException for empty vector"

            // ------------------------------------------------------------------
            // OPERATOR TESTS (Float)
            // ------------------------------------------------------------------
            testCase "vector-vector operators (float): .+ .- .* ./" <| fun _ ->
                let v1 = [| 1.0; 2.0; 3.0 |]
                let v2 = [| 4.0; 5.0; 6.0 |]

                // .+
                let addResult = v1 .+ v2
                Expect.equal addResult [|5.0; 7.0; 9.0|] "v1 .+ v2 should match element-wise add"

                // .-
                let subResult = v1 .- v2
                Expect.equal subResult [|-3.0; -3.0; -3.0|] "v1 .- v2 should match element-wise subtract"

                // .*
                let mulResult = v1 .* v2
                Expect.equal mulResult [|4.0; 10.0; 18.0|] "v1 .* v2 should match element-wise multiply"

                // ./ 
                let divResult = [| 10.0; 20.0; 30.0 |] ./ [| 2.0; 5.0; 5.0 |]
                Expect.equal divResult [|5.0; 4.0; 6.0|] "Should match element-wise division"

            testCase "vector-scalar operators (float): .+ .- .* ./" <| fun _ ->
                let v = [| 2.5; 4.0; 6.0 |]

                // v .+ scalar
                let addScalarResult = v .+ 2.0
                Expect.equal addScalarResult [|4.5; 6.0; 8.0|] "v .+ 2.0 should add 2.0 to all elements"

                // v .- scalar
                let subScalarResult = v .- 0.5
                Expect.equal subScalarResult [|2.0; 3.5; 5.5|] "v .- 0.5 should subtract 0.5 from all elements"

                // v .* scalar
                let mulScalarResult = v .* 1.5
                Expect.equal mulScalarResult [|3.75; 6.0; 9.0|] "v .* 1.5 should multiply all elements by 1.5"

                // v ./ scalar
                let divScalarResult = [| 10.0; 20.0; 30.0 |] ./ 10.0
                Expect.equal divScalarResult [|1.0; 2.0; 3.0|] "Should divide all elements by 10.0"

            // ------------------------------------------------------------------
            // cross (outer) product
            // ------------------------------------------------------------------
            testCase "cross: computes the outer product of two float vectors" <| fun _ ->
                // 'Vector.cross' is assumed to produce a 2D array (matrix)
                let colvec = [| 1.0; 2.0; 3.0 |]
                let rowvec = [| 4.0; 5.0; 6.0 |]
                
                // The expected 3x3 matrix is computed by: 
                // [ [1.0*4.0, 1.0*5.0, 1.0*6.0]
                //   [2.0*4.0, 2.0*5.0, 2.0*6.0]
                //   [3.0*4.0, 3.0*5.0, 3.0*6.0] ]
                let result   = Vector.cross colvec rowvec
                let expected = 
                    [| [|4.0;  5.0;  6.0|]
                       [|8.0; 10.0; 12.0|]
                       [|12.0;15.0; 18.0|] |] |> matrix

                Expect.equal result expected "Expected outer product of colvec and rowvec"
        ]
        // ======================================================================
        // Vector manipulation tests
        // ======================================================================
        testList "Vector manipulation Tests" [

            test "foldi applies a function with index to a vector" {
                let v = [|1; 2; 3|]
                let result = Vector.foldi (fun i acc x -> acc + i * x) 0 v
                Expect.equal result 8 "Expected indexed fold result"
            }

            test "mapi applies a function with index to each element" {
                let v = [|1; 2; 3|]
                let result = Vector.mapi (fun i x -> x + i) v
                Expect.equal result [|1; 3; 5|] "Expected indexed map result"
            }

            test "filter filters elements by predicate" {
                let v = [|1; 2; 3; 4|]
                let result = Vector.filter (fun x -> x % 2 = 0) v
                Expect.equal result [|2; 4|] "Expected filtered vector"
            }

            test "init creates a vector using a generator function" {
                let result = Vector.init 5 (fun i -> i * 2)
                Expect.equal result [|0; 2; 4; 6; 8|] "Expected initialized vector"
            }

            test "slice extracts a slice from a vector" {
                let v = [|1; 2; 3; 4; 5|]
                let result = Vector.slice 1 3 v
                Expect.equal result [|2; 3; 4|] "Expected sliced vector"
            }

            test "argmax finds the index of the maximum value" {
                let v = [|1; 3; 2|]
                let result = Vector.argmax v
                Expect.equal result 1 "Expected index of maximum value"
            }

            test "argmin finds the index of the minimum value" {
                let v = [|3; 1; 2|]
                let result = Vector.argmin v
                Expect.equal result 1 "Expected index of minimum value"
            }

            test "padRight pads a vector to a given length" {
                let v = [|1; 2|]
                let result = Vector.padRight 5 0 v
                Expect.equal result [|1; 2; 0; 0; 0|] "Expected padded vector"
            }

            test "zip combines two vectors with indices" {
                let v1 = [|1; 2; 3|]
                let v2 = [|4; 5; 6|]
                let result = Vector.zip v1 v2
                Expect.equal result [|(0, 1, 4); (1, 2, 5); (2, 3, 6)|] "Expected zipped vector"
            }

            test "argminBy finds the index of the minimum value using a projection" {
                let v = [|1; 2; 3|]
                let result = Vector.argminBy (fun x -> -x) v
                Expect.equal result 2 "Expected index of minimum value by projection"
            }

            test "argmaxBy finds the index of the maximum value using a projection" {
                let v = [|1; 2; 3|]
                let result = Vector.argmaxBy (fun x -> -x) v
                Expect.equal result 0 "Expected index of maximum value by projection"
            }

            test "tryFindIndex finds the index of the first matching element" {
                let v = [|1; 2; 3|]
                let result = Vector.tryFindIndex (fun x -> x = 2) v
                Expect.equal result (Some 1) "Expected index of first matching element"
            }

            test "findIndex finds the index of the first matching element or throws" {
                let v = [|1; 2; 3|]
                let result = Vector.findIndex (fun x -> x = 2) v
                Expect.equal result 1 "Expected index of first matching element"
            }

            test "enumerateNonZero enumerates non-zero elements with indices" {
                let v = [|0; 1; 0; 2|]
                let result = Vector.enumerateNonZero v
                Expect.equal result [|(1, 1); (3, 2)|] "Expected non-zero elements with indices"
            }

            test "split splits a vector into prefix and suffix" {
                let v = [|1; 2; 3; 4|]
                let prefix, suffix = Vector.split 2 v
                Expect.equal prefix [|1; 2|] "Expected prefix"
                Expect.equal suffix [|3; 4|] "Expected suffix"
            }

            test "chunk chunks a vector into equally sized pieces" {
                let v = [|1; 2; 3; 4; 5|]
                let result = Vector.chunk 2 v
                Expect.equal result [|[|1; 2|]; [|3; 4|]; [|5|]|] "Expected chunked vector"
            }

            test "windowed creates a sliding window over the vector" {
                let v = [|1; 2; 3; 4|]
                let result = Vector.windowed 2 v
                Expect.equal result [|[|1; 2|]; [|2; 3|]; [|3; 4|]|] "Expected sliding windows"
            }

            test "splitVector splits a vector based on indices" {
                let v = [|1; 2; 3; 4|]
                let nvi, nv = Vector.splitVector [|1; 3|] v
                Expect.equal nvi [|2; 4|] "Expected extracted elements"
                Expect.equal nv [|1; 3|] "Expected remaining elements"
            }

            test "permuteBy permutes a vector using a permutation function" {
                let v = [|1; 2; 3|]
                let result = Vector.permuteBy (fun i -> (i + 1) % 3) v // 1 2 0
                Expect.equal result [|2; 3; 1|] "Expected permuted vector"
            }

            test "ofSeq converts a sequence to a vector" {
                let seq = seq { 1; 2; 3 }
                let result = Vector.ofSeq seq
                Expect.equal result [|1; 2; 3|] "Expected vector from sequence"
            }
        ]

    ]





//let private testVectorA =
//    let values = [|0.;3.;6.|]
//    Vector<float>(Some (Instances.FloatNumerics :> INumeric<float>),values)

//[<Tests>]
//let covarianceTests =
//    let x = vector [5.;12.;18.;-23.;45.]
//    let y = vector [2.;8.;18.;-20.;28.]

//    testList "Vector" [
//        testCase "cov" <| fun () ->
//            let cov = Vector.cov x y
//            Expect.floatClose Accuracy.high cov 434.90 "Should be equal (double precision)"
//        testCase "covPopulation" <| fun () ->
//            let covPop = Vector.covPopulation x y
//            Expect.floatClose Accuracy.high covPop 347.92 "Should be equal (double precision)"
//    ]
