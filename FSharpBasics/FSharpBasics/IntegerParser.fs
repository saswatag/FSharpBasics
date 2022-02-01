namespace IntegerParser

module IntegerParser = 
    let tryParseIntegerFromString (inputString: string) = 
        try 
            inputString |> int |> Some
        with :? System.FormatException -> 
            None

module StringAddExplicitWorkflow = 
    open IntegerParser

    let log message = 
        printfn message

    let stringAddWorkflow string1 string2 string3 string4 = 
        let int1MayBe = string1 |> tryParseIntegerFromString
        match int1MayBe with
        | None -> 
            log $"{string1} is not and integer. Aborting add workflow"
            None
        | Some int1 ->

            log $"{string1} is an integer. Proceeding with add workflow"    
            let int2MayBe = string2 |> tryParseIntegerFromString
            match int2MayBe with
            | None -> 
                log $"{string2} is not and integer. Aborting add workflow"
                None
            | Some int2 ->

                log $"{string2} is an integer. Proceeding with add workflow"  
                let int3MayBe = string3 |> tryParseIntegerFromString
                match int3MayBe with
                | None -> 
                    log $"{string3} is not and integer. Aborting add workflow"
                    None
                | Some int3 ->

                    log $"{string3} is an integer. Proceeding with add workflow"  
                    let int4MayBe = string4 |> tryParseIntegerFromString
                    match int4MayBe with
                    | None -> 
                        log $"{string4} is not and integer. Aborting add workflow"
                        None
                    | Some int4 ->
                        log $"{string4} is an integer. Proceeding with add workflow"  
                        Some ( int1 + int2 + int3 + int4 )


module StringAddPipeIntoWorkflow = 
    open IntegerParser

    let log message = 
        printfn message

    let sum number1 number2 = 
        number1 + number2

    let pipeInto (stringValue, onSuccessfullParseInteger) = 
        let intMayBe = stringValue |> tryParseIntegerFromString
        match intMayBe with
        | None -> 
            log $"{stringValue} is not and integer. Aborting add workflow"
            None
        | Some integer -> 
            log $"{integer} is an integer. Proceeding with add workflow"
            integer |> onSuccessfullParseInteger


    let stringAddWorkflow string1 string2 string3 string4 = 
        let number1 = string1 |> tryParseIntegerFromString

        //pipeInto(string1,  |> sum string2)

    //let stringAddWorkflow string1 string2 string3 string4 = 
    //    let int1MayBe = string1 |> tryParseIntegerFromString
    //    match int1MayBe with
    //    | None -> 
    //        log $"{string1} is not and integer. Aborting add workflow"
    //        None
    //    | Some int1 ->

    //        log $"{string1} is an integer. Proceeding with add workflow"    
    //        let int2MayBe = string2 |> tryParseIntegerFromString
    //        match int2MayBe with
    //        | None -> 
    //            log $"{string2} is not and integer. Aborting add workflow"
    //            None
    //        | Some int2 ->

    //            log $"{string2} is an integer. Proceeding with add workflow"  
    //            let int3MayBe = string3 |> tryParseIntegerFromString
    //            match int3MayBe with
    //            | None -> 
    //                log $"{string3} is not and integer. Aborting add workflow"
    //                None
    //            | Some int3 ->

    //                log $"{string3} is an integer. Proceeding with add workflow"  
    //                let int4MayBe = string4 |> tryParseIntegerFromString
    //                match int4MayBe with
    //                | None -> 
    //                    log $"{string4} is not and integer. Aborting add workflow"
    //                    None
    //                | Some int4 ->
    //                    log $"{string4} is an integer. Proceeding with add workflow"  
    //                    Some ( int1 + int2 + int3 + int4 )

    