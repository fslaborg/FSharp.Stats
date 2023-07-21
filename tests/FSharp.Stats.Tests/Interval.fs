module IntervalTests

open Expecto

open FSharp.Stats
open FSharp.Stats.Interval
open TestExtensions

[<Tests>]
let intervalTests =
    //apply tests also to Seq.range

    testList "Intervals" [
        
        testCase "create" (fun _ -> 

            let expected = Interval.Closed (-5.,5.)
            let actual = Interval.CreateClosed (-5.,5.)
            Expect.equal expected actual "Instantiation of Interval.Closed is incorrect"
        
            //let expectedError() = Intervals.create 5. -5. |> ignore
            //Expect.throws expectedError "Interval maximum must be greater than minimum"
        )
        
        testCase "ofSeq" (fun _ -> 
            let expected = Interval.Closed (-5.,5.)
            let actual = Interval.ofSeq [3.;-0.;-5.;0.;0.;5.]
            Expect.equal actual expected "Wrong interval was extracted from sequence"

            let expectedInt = Interval.Closed (5,5)
            let actualInt = Interval.ofSeq [5;5;5;5;5;]
            Expect.equal actualInt expectedInt "Wrong interval was extracted from sequence"

            let nanCase() = Interval.ofSeq [3.;nan;-0.;-5.;0.;0.;5.] |> ignore
            Expect.throws nanCase "collections containing nan should fail to return a valid interval"
        
            let expectedInf = Interval.Closed (-5.,infinity)
            let actualInf = Interval.ofSeq [3.;infinity;-0.;-5.;infinity;0.;0.;5.]
            Expect.equal actualInf expectedInf "infinity should be upper margin of interval"
        
            let expectedInfNeg = Interval.Closed (-infinity,5.)
            let actualInfNeg = Interval.ofSeq [3.;-infinity;-0.;-5.;0.;0.;-infinity;5.]
            Expect.equal actualInfNeg expectedInfNeg "-infinity should be lower margin of interval"
        
            let expectedInfs = Interval.Closed (-infinity,infinity)
            let actualInfs = Interval.ofSeq [3.;infinity;-0.;-5.;0.;0.;-infinity;5.]
            Expect.equal actualInfs expectedInfs "-infinity and infinity should be interval margins"
        
            let expectedEmpty = Interval.Empty
            let actualEmpty = Interval.ofSeq []
            Expect.equal actualEmpty expectedEmpty "Interval should be empty"
            
            let expectedStr = Interval.Closed ("aavbsd","z")
            let actualStr = Interval.ofSeq ["asd";"bcd";"aavbsd";"z"]
            Expect.equal actualStr expectedStr "Interval of strings is incorrect"
            
            let expectedChar = Interval.CreateClosed<char> ('f','r')
            let actualChar = Interval.ofSeq ['g';'f';'q';'q';'r';]
            Expect.equal actualChar expectedChar "Interval of chars is incorrect"
        )
        
        testCase "ofSeqBy" (fun _ -> 
            let expected = Interval.CreateClosed<int*float> ((3,-5.),(6,5.))
            let actual = Interval.ofSeqBy snd [0,3.;1,5;2,-0.;3,-5.;4,0.;5,0.;6,5.]
            Expect.equal actual expected "Wrong interval was extracted from indexed sequence"
            
            let expectedInt = Interval.CreateClosed<int*int> ((0,5),(4,5))
            let actualInt = Interval.ofSeqBy snd (List.indexed [5;5;5;5;5;])
            Expect.equal actualInt expectedInt "Wrong interval was extracted from sequence"

            let nanCase() = Interval.ofSeqBy snd (List.indexed [3.;nan;-0.;-5.;0.;0.;5.]) |> ignore
            Expect.throws nanCase "collections containing nan should fail to return a valid interval"
        
            let expectedInf = Interval.CreateClosed<int*float> ((3,-5.),(4,infinity))
            let actualInf = Interval.ofSeqBy snd (List.indexed [3.;infinity;-0.;-5.;infinity;0.;0.;5.])
            Expect.equal actualInf expectedInf "infinity should be upper margin of interval"
        
            let expectedInfNeg = Interval.CreateClosed<int*float> ((1,-infinity),(7,5.))
            let actualInfNeg = Interval.ofSeqBy snd (List.indexed [3.;-infinity;-0.;-5.;0.;0.;-infinity;5.])
            Expect.equal actualInfNeg expectedInfNeg "-infinity should be lower margin of interval"
        
            let expectedInfs = Interval.CreateClosed<int*float> ((6,-infinity),(1,infinity))
            let actualInfs = Interval.ofSeqBy snd (List.indexed [3.;infinity;-0.;-5.;0.;0.;-infinity;5.])
            Expect.equal actualInfs expectedInfs "-infinity and infinity should be interval margins"
            
            let expectedEmpty = Interval.Empty
            let actualEmpty = Interval.ofSeqBy snd []
            Expect.equal actualEmpty expectedEmpty "Interval should be empty"
            
            let expectedStr = Interval.CreateClosed<int*string> ((2,"a"),(4,"zz"))
            let actualStr = Interval.ofSeqBy snd (List.indexed ["asd";"bcd";"a";"z";"zz";"aavbsd"])
            Expect.equal actualStr expectedStr "Interval of strings is incorrect"
            
            let expectedChar = Interval.CreateClosed<int*char> ((1,'f'),(5,'r'))
            let actualChar = Interval.ofSeqBy snd (List.indexed  ['g';'f';'q';'q';'r';'r'])
            Expect.equal actualChar expectedChar "Interval of chars is incorrect"
        )

        testCase "values" (fun _ -> 
            let expected = 
                Interval.CreateClosed<float> (-5.,5.)
                |> Interval.values
            let actual = (-5.,5.)
            Expect.equal actual expected "Interval value assessment is incorrect"
            
            let expectedEmpty() = 
                Interval.Empty
                |> Interval.values
                |> ignore
            Expect.throws expectedEmpty "Empty intervals cannot have values"
            )

        testCase "getStart" (fun _ -> 
            let expected = 
                Interval.CreateClosed<float> (-5.,5.)
                |> Interval.getStart
            let actual = -5.
            Expect.equal actual expected "Interval minimum assessment is incorrect"
            
            let expectedEmpty() = 
                Interval.Empty
                |> Interval.getStart
                |> ignore
            Expect.throws expectedEmpty "Empty intervals cannot have starts"
            )

        testCase "getEnd" (fun _ -> 
            let actual = 
                Interval.CreateClosed<float> (-5.,5.)
                |> Interval.getEnd
            let expected = 5.
            Expect.equal actual expected "Interval maximum assessment is incorrect"
            
            let expectedEmpty() = 
                Interval.Empty
                |> Interval.getEnd
                |> ignore
            Expect.throws expectedEmpty "Empty intervals cannot have ends"
            )

        testCase "getSize" (fun _ -> 
            let actual = 
                Interval.CreateClosed<float> (-5.,5.5)
                |> Interval.getSize
            let expected = 10.5
            Expect.equal actual expected "Interval size calculation is incorrect"
            
            let expectedEmpty() = 
                Interval.Empty
                |> Interval.getSize
                |> ignore
            Expect.throws expectedEmpty "Empty intervals cannot have have a size"
            )

        testCase "getSizeBy" (fun _ -> 
            let actual = 
                Interval.CreateClosed<string*float> (("a",-5.),("b",5.5))
                |> Interval.getSizeBy snd
            let expected = 10.5
            Expect.equal actual expected "Interval size calculation is incorrect"
            
            let expectedEmpty() = 
                Interval.Empty
                |> Interval.getSizeBy id
                |> ignore
            Expect.throws expectedEmpty "Empty intervals cannot have a size"
            )

        testCase "trySize" (fun _ -> 
            let actual = 
                Interval.CreateClosed<float> (-5.,5.5)
                |> Interval.trySize
            let expected = Some 10.5
            Expect.equal actual expected "Size of interval is incorrect"
            
            let expectedEmpty = 
                Interval.Empty
                |> Interval.trySize
            Expect.equal expectedEmpty None "Empty intervals have no size"
            )

        testCase "add" (fun _ -> 
            let actual = 
                let i1 = Interval.CreateClosed<float> (-5.,5.5)
                let i2 = Interval.CreateClosed<float> (0.,3.)
                Interval.add i1 i2
            let expected = Interval.CreateClosed<float> (-5.,8.5)
            Expect.equal actual expected "Interval addition is incorrect"
            
            let actualCE = 
                let i1 = Interval.CreateClosed<float> (-5.,5.5)
                let i2 = Interval.Empty
                Interval.add i1 i2
            let expectedCE = Interval.CreateClosed<float> (-5.,5.5)
            Expect.equal actualCE expectedCE "Interval addition of Empty intervals is incorrect"
            
            let actualEC = 
                let i1 = Interval.Empty
                let i2 = Interval.CreateClosed<float> (0.,3.)
                Interval.add i1 i2
            let expectedEC = Interval.CreateClosed<float> (0.,3.)
            Expect.equal actualEC expectedEC "Interval addition of Empty intervals is incorrect"
            
            let actualEE = 
                let i1 = Interval.Empty
                let i2 = Interval.Empty
                Interval.add i1 i2
            let expectedEE = Interval.Empty
            Expect.equal actualEE expectedEE "Interval addition of Empty intervals is incorrect"
            )

        testCase "subtract" (fun _ -> 
            let actual = 
                let i1 = Interval.CreateClosed<float> (-5.,5.5)
                let i2 = Interval.CreateClosed<float> (-3.,0.)
                Interval.subtract i1 i2
            let expected = Interval.CreateClosed<float> (-5.,8.5)
            Expect.equal actual expected "Interval subtraction is incorrect"
            
            let actualCE = 
                let i1 = Interval.CreateClosed<float> (-5.,5.5)
                let i2 = Interval.Empty
                Interval.subtract i1 i2
            let expectedCE = Interval.CreateClosed<float> (-5.,5.5)
            Expect.equal actualCE expectedCE "Interval subtraction of Empty intervals is incorrect"
            
            let actualEC = 
                let i1 = Interval.Empty
                let i2 = Interval.CreateClosed<float> (-3.,0.)
                Interval.subtract i1 i2
            let expectedEC = Interval.CreateClosed<float> (-3.,0.)
            Expect.equal actualEC expectedEC "Interval subtraction of Empty intervals is incorrect"
            
            let actualEE = 
                let i1 = Interval.Empty
                let i2 = Interval.Empty
                Interval.subtract i1 i2
            let expectedEE = Interval.Empty
            Expect.equal actualEE expectedEE "Interval subtraction of Empty intervals is incorrect"
            )

        // Closed intervals include their minimal and maximal values. Therefore shared margins are intersections.
        testCase "isIntersection" (fun _ -> 
            let actual = 
                let i1 = Interval.CreateClosed<float> (-5.,5.5)
                let i2 = Interval.CreateClosed<float> (-3.,0.)
                Interval.isIntersection i1 i2
            let expected = true
            Expect.equal actual expected "Intervals do intersect"

            let actualFalse = 
                let i1 = Interval.CreateClosed<float> (-5.,5.5)
                let i2 = Interval.CreateClosed<float> (-infinity,-6.)
                Interval.isIntersection i1 i2
            let expectedFalse = false
            Expect.equal actual expected "Intervals do not intersect"
            
            let actualCE = 
                let i1 = Interval.CreateClosed<float> (-5.,5.5)
                let i2 = Interval.Empty
                Interval.isIntersection i1 i2
            let expectedCE = false
            Expect.equal actualCE expectedCE "Intervals do not intersect"
            
            let actualEC = 
                let i1 = Interval.Empty
                let i2 = Interval.CreateClosed<float> (-3.,0.)
                Interval.isIntersection i1 i2
            let expectedEC = false
            Expect.equal actualEC expectedEC "Empty intervals do not intersect"
            
            let actualEE = 
                let i1 = Interval.Empty
                let i2 = Interval.Empty
                Interval.isIntersection i1 i2
            let expectedEE = true
            Expect.equal actualEE expectedEE "Empty intervals do intersect"
            
            let actualStr = 
                let i1 = Interval.CreateClosed<string> ("a","d")
                let i2 = Interval.CreateClosed<string> ("de","e")
                Interval.isIntersection i1 i2
            let expectedStr = false
            Expect.equal actualStr expectedStr "String intervals do not intersect"
            
            let actualStrT = 
                let i1 = Interval.CreateClosed<string> ("a","d")
                let i2 = Interval.CreateClosed<string> ("d","e")
                Interval.isIntersection i1 i2
            let expectedStrT = true
            Expect.equal actualStrT expectedStrT "String intervals do intersect"
            )

        
        // Closed intervals include their minimal and maximal values. Therefore shared margins are intersections.
        testCase "intersect" (fun _ -> 
            let actual = 
                let i1 = Interval.CreateClosed<float> (-5.,5.5)
                let i2 = Interval.CreateClosed<float> (-3.,0.)
                Interval.intersect i1 i2
            let expected = Some (Interval.CreateClosed<float> (-3.,0.))
            Expect.equal actual expected "Interval intersect is calculated incorrectly"

            let actualFalse = 
                let i1 = Interval.CreateClosed<float> (-5.,5.5)
                let i2 = Interval.CreateClosed<float> (-infinity,-6.)
                Interval.intersect i1 i2
            let expectedFalse = None
            Expect.equal actual expected "Interval intersect is calculated incorrectly"

            let actual2 = 
                let i1 = Interval.CreateClosed<float> (-5.,5.5)
                let i2 = Interval.CreateClosed<float> (-infinity,2.)
                Interval.intersect i1 i2
            let expected2 = Some (Interval.CreateClosed<float> (-5.,2.))
            Expect.equal actual2 expected2 "Intervals do intersect"
            
            let actualCE = 
                let i1 = Interval.CreateClosed<float> (-5.,5.5)
                let i2 = Interval.Empty
                Interval.intersect i1 i2
            let expectedCE = None
            Expect.equal actualCE expectedCE "Intervals do not intersect"
            
            let actualEC = 
                let i1 = Interval.Empty
                let i2 = Interval.CreateClosed<float> (-3.,0.)
                Interval.intersect i1 i2
            let expectedEC = None
            Expect.equal actualEC expectedEC "Empty intervals do not intersect"
            
            let actualEE = 
                let i1 = Interval.Empty
                let i2 = Interval.Empty
                Interval.intersect i1 i2
            let expectedEE = Some Interval.Empty
            Expect.equal actualEE expectedEE "Empty intervals do intersect"
            
            let actualStr = 
                let i1 = Interval.CreateClosed<string> ("a","d")
                let i2 = Interval.CreateClosed<string> ("de","e")
                Interval.intersect i1 i2
            let expectedStr = None
            Expect.equal actualStr expectedStr "String intervals do not intersect"
            
            let actualStrT = 
                let i1 = Interval.CreateClosed<string> ("a","d")
                let i2 = Interval.CreateClosed<string> ("d","e")
                Interval.intersect i1 i2
            let expectedStrT = Some (Interval.CreateClosed<string> ("d","d"))
            Expect.equal actualStrT expectedStrT "String intervals do intersect"
            )


            
    ]

