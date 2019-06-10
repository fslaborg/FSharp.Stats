namespace FSharp.Stats

module FSIPrinters =
    open System    
    open System.Linq

    let matrix (mat: Matrix<_>) =
        //TODO: Difficult to read because of all exceptions. Simplified version!
        let colLength = mat.NumCols
        let rowLength = mat.NumRows
        //number of rows/column to print from each side
        let numberToPrint = 10

        ///print column indices and horizontal separator
        let rowHeader() = 
            if colLength > (2 * numberToPrint) then 
                let strHead = String.concat " " ("       "::(List.init numberToPrint (fun i -> sprintf "%8i " i )))
                let strLast = String.concat " " (List.init numberToPrint (fun i -> sprintf "%8i " (colLength - numberToPrint + i )))
                printfn "%s      ...  %s" strHead strLast
                printfn "%s" (String.concat "" (List.init ((2 * numberToPrint) * 10 + 7 + 9) (fun _ -> "_"))) //7=header, 9=fstItem
            else 
                printfn "%s" (String.concat " " ("       "::(List.init colLength (fun i -> sprintf "%8i " i ))))
                printfn "%s" (String.concat "" (List.init (colLength * 10 + 7) (fun _ -> "_")))

        //print internal horizonal separaor
        let placeHolder() = 
            let single = "      ..."
            if colLength > (2 * numberToPrint) then 
                 printfn "%s" (String.concat " " ("...   "::(List.init (2 * numberToPrint + 1) (fun _ -> single))))
            else printfn "%s" (String.concat " " ("...   "::(List.init colLength (fun _ -> single))))

        //print row with row index and vertical separator
        let printRow index = 
            let currentRow = mat.Row index
            let toString (rv) = 
                rv 
                |> Seq.map (fun x ->    
                    //if the value is greater than 10000. display scientific notation, else display default notation
                    match x with
                    | x when x <= -10000. -> sprintf "%8.2g" x
                    | x when x < 0.     -> sprintf "%8.2f" x
                    | x when x >= 10000.  -> sprintf "%8.2g" x
                    | _    -> sprintf "%8.3f" x)
            if colLength > (2 * numberToPrint) then
                let strHead = currentRow.[0 .. (numberToPrint - 1)]            |> toString |> String.concat "  "
                let strLast = currentRow.[colLength - numberToPrint ..] |> toString |> String.concat "  "
                let printString = sprintf "%s       ...  %s" strHead strLast
                printfn "%-5i | %s" index printString
            else printfn "%-5i | %s" index (currentRow |> toString |> String.concat "  ")
        
        //print column indices
        rowHeader()
        if rowLength > (2 * numberToPrint) then 
            //print first 10 rows
            [0 .. (numberToPrint - 1)] |> Seq.iter printRow
            //print horizontal separator
            placeHolder()
            //print last 10 rows
            [rowLength - (numberToPrint + 1) .. rowLength - 1] |> Seq.iter printRow
        else
            //print all rows
            [0 .. rowLength - 1 ] |> Seq.iter printRow

//let matrix1 = Matrix.init 7 20 (fun i j -> -(float i) + (float j))
//let matrix2 = Matrix.init 20 7 (fun i j -> -(float i) + (float j))
//let matrix3 = Matrix.init 1000 1000 (fun i j -> -(float i) + (float j))

//FSharp.Stats.FSIPrinters.matrix matrix1
//FSharp.Stats.FSIPrinters.matrix matrix2
//FSharp.Stats.FSIPrinters.matrix matrix3
