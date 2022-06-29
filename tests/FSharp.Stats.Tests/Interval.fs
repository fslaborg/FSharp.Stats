module IntervalTests

open Expecto

open FSharp.Stats
open FSharp.Stats.Intervals
open TestExtensions

[<Tests>]
let intervalTests =
    //apply tests also to Seq.range

    testList "Intervals" [
        
        testCase "create" (fun _ -> 
            Expect.equal 1 1 "Instantiation of Intervals.Interval is incorrect"

            let expected = Interval.ClosedInterval (-5.,5.)
            let actual = Intervals.create -5. 5.
            Expect.equal expected actual "Instantiation of Intervals.Interval is incorrect"
        
            //let expectedError() = Intervals.create 5. -5. |> ignore
            //Expect.throws expectedError "Interval maximum must be greater than minimum"
        )
        
        testCase "ofSeq" (fun _ -> 
            let expected = Intervals.create -5. 5.
            let actual = Intervals.ofSeq [3.;-0.;-5.;0.;0.;5.]
            Expect.equal actual expected "Wrong interval was extracted from sequence"

            let expectedInt = Intervals.create 5 5
            let actualInt = Intervals.ofSeq [5;5;5;5;5;]
            Expect.equal actualInt expectedInt "Wrong interval was extracted from sequence"

            let nanCase() = Intervals.ofSeq [3.;nan;-0.;-5.;0.;0.;5.] |> ignore
            Expect.throws nanCase "collections containing nan should fail to return a valid interval"
        
            let expectedInf = Intervals.create -5. infinity
            let actualInf = Intervals.ofSeq [3.;infinity;-0.;-5.;infinity;0.;0.;5.]
            Expect.equal actualInf expectedInf "infinity should be upper margin of interval"
        
            let expectedInfNeg = Intervals.create -infinity 5.
            let actualInfNeg = Intervals.ofSeq [3.;-infinity;-0.;-5.;0.;0.;-infinity;5.]
            Expect.equal actualInfNeg expectedInfNeg "-infinity should be lower margin of interval"
        
            let expectedInfs = Intervals.create -infinity infinity
            let actualInfs = Intervals.ofSeq [3.;infinity;-0.;-5.;0.;0.;-infinity;5.]
            Expect.equal actualInfs expectedInfs "-infinity and infinity should be interval margins"
        
            let expectedEmpty = Intervals.Interval.Empty
            let actualEmpty = Intervals.ofSeq []
            Expect.equal actualEmpty expectedEmpty "Interval should be empty"
            
            let expectedStr = Intervals.create "aavbsd" "z"
            let actualStr = Intervals.ofSeq ["asd";"bcd";"aavbsd";"z"]
            Expect.equal actualStr expectedStr "Interval of strings is incorrect"
            
            let expectedChar = Intervals.create 'f' 'r'
            let actualChar = Intervals.ofSeq ['g';'f';'q';'q';'r';]
            Expect.equal actualChar expectedChar "Interval of chars is incorrect"
        )
        
        testCase "ofSeqBy" (fun _ -> 
            let expected = Intervals.create (3,-5.) (6,5.)
            let actual = Intervals.ofSeqBy snd [0,3.;1,5;2,-0.;3,-5.;4,0.;5,0.;6,5.]
            Expect.equal actual expected "Wrong interval was extracted from indexed sequence"
            
            let expectedInt = Intervals.create (0,5) (4,5)
            let actualInt = Intervals.ofSeqBy snd (List.indexed [5;5;5;5;5;])
            Expect.equal actualInt expectedInt "Wrong interval was extracted from sequence"

            let nanCase() = Intervals.ofSeqBy snd (List.indexed [3.;nan;-0.;-5.;0.;0.;5.]) |> ignore
            Expect.throws nanCase "collections containing nan should fail to return a valid interval"
        
            let expectedInf = Intervals.create (3,-5.) (4,infinity)
            let actualInf = Intervals.ofSeqBy snd (List.indexed [3.;infinity;-0.;-5.;infinity;0.;0.;5.])
            Expect.equal actualInf expectedInf "infinity should be upper margin of interval"
        
            let expectedInfNeg = Intervals.create (1,-infinity) (7,5.)
            let actualInfNeg = Intervals.ofSeqBy snd (List.indexed [3.;-infinity;-0.;-5.;0.;0.;-infinity;5.])
            Expect.equal actualInfNeg expectedInfNeg "-infinity should be lower margin of interval"
        
            let expectedInfs = Intervals.create (6,-infinity) (1,infinity)
            let actualInfs = Intervals.ofSeqBy snd (List.indexed [3.;infinity;-0.;-5.;0.;0.;-infinity;5.])
            Expect.equal actualInfs expectedInfs "-infinity and infinity should be interval margins"
            
            let expectedEmpty = Intervals.Interval.Empty
            let actualEmpty = Intervals.ofSeqBy snd []
            Expect.equal actualEmpty expectedEmpty "Interval should be empty"
            
            let expectedStr = Intervals.create (2,"a") (4,"zz")
            let actualStr = Intervals.ofSeqBy snd (List.indexed ["asd";"bcd";"a";"z";"zz";"aavbsd"])
            Expect.equal actualStr expectedStr "Interval of strings is incorrect"
            
            let expectedChar = Intervals.create (1,'f') (5,'r')
            let actualChar = Intervals.ofSeqBy snd (List.indexed  ['g';'f';'q';'q';'r';'r'])
            Expect.equal actualChar expectedChar "Interval of chars is incorrect"
        )

        testCase "values" (fun _ -> 
            let expected = 
                Intervals.create (-5.) (5.)
                |> Intervals.values
            let actual = (-5.,5.)
            Expect.equal actual expected "Interval value assessment is incorrect"
            
            let expectedEmpty() = 
                Intervals.Interval.Empty
                |> Intervals.values
                |> ignore
            Expect.throws expectedEmpty "Empty intervals cannot have values"
            )

        testCase "getStart" (fun _ -> 
            let expected = 
                Intervals.create (-5.) (5.)
                |> Intervals.getStart
            let actual = -5.
            Expect.equal actual expected "Interval minimum assessment is incorrect"
            
            let expectedEmpty() = 
                Intervals.Interval.Empty
                |> Intervals.getStart
                |> ignore
            Expect.throws expectedEmpty "Empty intervals cannot have starts"
            )

        testCase "getEnd" (fun _ -> 
            let actual = 
                Intervals.create (-5.) (5.)
                |> Intervals.getEnd
            let expected = 5.
            Expect.equal actual expected "Interval maximum assessment is incorrect"
            
            let expectedEmpty() = 
                Intervals.Interval.Empty
                |> Intervals.getEnd
                |> ignore
            Expect.throws expectedEmpty "Empty intervals cannot have ends"
            )

        testCase "getSize" (fun _ -> 
            let actual = 
                Intervals.create (-5.) (5.5)
                |> Intervals.getSize
            let expected = 10.5
            Expect.equal actual expected "Interval size calculation is incorrect"
            
            let expectedEmpty() = 
                Intervals.Interval.Empty
                |> Intervals.getSize
                |> ignore
            Expect.throws expectedEmpty "Empty intervals cannot have have a size"
            )

        testCase "getSizeBy" (fun _ -> 
            let actual = 
                Intervals.create ("a",-5.) ("b",5.5)
                |> Intervals.getSizeBy snd
            let expected = 10.5
            Expect.equal actual expected "Interval size calculation is incorrect"
            
            let expectedEmpty() = 
                Intervals.Interval.Empty
                |> Intervals.getSizeBy id
                |> ignore
            Expect.throws expectedEmpty "Empty intervals cannot have a size"
            )

        testCase "trySize" (fun _ -> 
            let actual = 
                Intervals.create (-5.) (5.5)
                |> Intervals.trySize
            let expected = Some 10.5
            Expect.equal actual expected "Size of interval is incorrect"
            
            let expectedEmpty = 
                Intervals.Interval.Empty
                |> Intervals.trySize
            Expect.equal expectedEmpty None "Empty intervals have no size"
            )

        testCase "add" (fun _ -> 
            let actual = 
                let i1 = Intervals.create (-5.) (5.5)
                let i2 = Intervals.create (0.) (-3.)
                Intervals.add i1 i2
            let expected = Intervals.create (-5.) (2.5)
            Expect.equal actual expected "Interval addition is incorrect"
            
            let actualCE = 
                let i1 = Intervals.create (-5.) (5.5)
                let i2 = Intervals.Interval.Empty
                Intervals.add i1 i2
            let expectedCE = Intervals.create (-5.) (5.5)
            Expect.equal actualCE expectedCE "Interval addition of Empty intervals is incorrect"
            
            let actualEC = 
                let i1 = Intervals.Interval.Empty
                let i2 = Intervals.create (0.) (-3.)
                Intervals.add i1 i2
            let expectedEC = Intervals.create (0.) (-3.)
            Expect.equal actualEC expectedEC "Interval addition of Empty intervals is incorrect"
            
            let actualEE = 
                let i1 = Intervals.Interval.Empty
                let i2 = Intervals.Interval.Empty
                Intervals.add i1 i2
            let expectedEE = Intervals.Interval.Empty
            Expect.equal actualEE expectedEE "Interval addition of Empty intervals is incorrect"
            )

        testCase "subtract" (fun _ -> 
            let actual = 
                let i1 = Intervals.create (-5.) (5.5)
                let i2 = Intervals.create (-3.) (0.)
                Intervals.subtract i1 i2
            let expected = Intervals.create (-5.) (8.5)
            Expect.equal actual expected "Interval subtraction is incorrect"
            
            let actualCE = 
                let i1 = Intervals.create (-5.) (5.5)
                let i2 = Intervals.Interval.Empty
                Intervals.subtract i1 i2
            let expectedCE = Intervals.create (-5.) (5.5)
            Expect.equal actualCE expectedCE "Interval subtraction of Empty intervals is incorrect"
            
            let actualEC = 
                let i1 = Intervals.Interval.Empty
                let i2 = Intervals.create (-3.) (0.)
                Intervals.subtract i1 i2
            let expectedEC = Intervals.create (-3.) (0.)
            Expect.equal actualEC expectedEC "Interval subtraction of Empty intervals is incorrect"
            
            let actualEE = 
                let i1 = Intervals.Interval.Empty
                let i2 = Intervals.Interval.Empty
                Intervals.subtract i1 i2
            let expectedEE = Intervals.Interval.Empty
            Expect.equal actualEE expectedEE "Interval subtraction of Empty intervals is incorrect"
            )

        // Closed intervals include their minimal and maximal values. Therefore shared margins are intersections.
        testCase "isIntersection" (fun _ -> 
            let actual = 
                let i1 = Intervals.create (-5.) (5.5)
                let i2 = Intervals.create (-3.) (0.)
                Intervals.isIntersection i1 i2
            let expected = true
            Expect.equal actual expected "Intervals do intersect"

            let actualFalse = 
                let i1 = Intervals.create (-5.) (5.5)
                let i2 = Intervals.create (-infinity) (-6.)
                Intervals.isIntersection i1 i2
            let expectedFalse = false
            Expect.equal actual expected "Intervals do not intersect"
            
            let actualCE = 
                let i1 = Intervals.create (-5.) (5.5)
                let i2 = Intervals.Interval.Empty
                Intervals.isIntersection i1 i2
            let expectedCE = false
            Expect.equal actualCE expectedCE "Intervals do not intersect"
            
            let actualEC = 
                let i1 = Intervals.Interval.Empty
                let i2 = Intervals.create (-3.) (0.)
                Intervals.isIntersection i1 i2
            let expectedEC = false
            Expect.equal actualEC expectedEC "Empty intervals do not intersect"
            
            let actualEE = 
                let i1 = Intervals.Interval.Empty
                let i2 = Intervals.Interval.Empty
                Intervals.isIntersection i1 i2
            let expectedEE = true
            Expect.equal actualEE expectedEE "Empty intervals do intersect"
            
            let actualStr = 
                let i1 = Intervals.create "a" "d"
                let i2 = Intervals.create "de" "e"
                Intervals.isIntersection i1 i2
            let expectedStr = false
            Expect.equal actualStr expectedStr "String intervals do not intersect"
            
            let actualStrT = 
                let i1 = Intervals.create "a" "d"
                let i2 = Intervals.create "d" "e"
                Intervals.isIntersection i1 i2
            let expectedStrT = true
            Expect.equal actualStrT expectedStrT "String intervals do intersect"
            )

        
        // Closed intervals include their minimal and maximal values. Therefore shared margins are intersections.
        testCase "intersect" (fun _ -> 
            let actual = 
                let i1 = Intervals.create (-5.) (5.5)
                let i2 = Intervals.create (-3.) (0.)
                Intervals.intersect i1 i2
            let expected = Some (Intervals.create -3. 0.)
            Expect.equal actual expected "Interval intersect is calculated incorrectly"

            let actualFalse = 
                let i1 = Intervals.create (-5.) (5.5)
                let i2 = Intervals.create (-infinity) (-6.)
                Intervals.intersect i1 i2
            let expectedFalse = None
            Expect.equal actual expected "Interval intersect is calculated incorrectly"

            let actual2 = 
                let i1 = Intervals.create (-5.) (5.5)
                let i2 = Intervals.create (-infinity) (2.)
                Intervals.intersect i1 i2
            let expected2 = Some (Intervals.create -5. 2.)
            Expect.equal actual2 expected2 "Intervals do intersect"
            
            let actualCE = 
                let i1 = Intervals.create (-5.) (5.5)
                let i2 = Intervals.Interval.Empty
                Intervals.intersect i1 i2
            let expectedCE = None
            Expect.equal actualCE expectedCE "Intervals do not intersect"
            
            let actualEC = 
                let i1 = Intervals.Interval.Empty
                let i2 = Intervals.create (-3.) (0.)
                Intervals.intersect i1 i2
            let expectedEC = None
            Expect.equal actualEC expectedEC "Empty intervals do not intersect"
            
            let actualEE = 
                let i1 = Intervals.Interval.Empty
                let i2 = Intervals.Interval.Empty
                Intervals.intersect i1 i2
            let expectedEE = Some Intervals.Interval.Empty
            Expect.equal actualEE expectedEE "Empty intervals do intersect"
            
            let actualStr = 
                let i1 = Intervals.create "a" "d"
                let i2 = Intervals.create "de" "e"
                Intervals.intersect i1 i2
            let expectedStr = None
            Expect.equal actualStr expectedStr "String intervals do not intersect"
            
            let actualStrT = 
                let i1 = Intervals.create "a" "d"
                let i2 = Intervals.create "d" "e"
                Intervals.intersect i1 i2
            let expectedStrT = Some (Intervals.create "d" "d")
            Expect.equal actualStrT expectedStrT "String intervals do intersect"
            )


            
    ]

