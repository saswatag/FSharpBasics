let result =
    let i, j, k = (1, 2, 3)
    // Body expression:
    i + 2*j + 3*k


let resultNew =
    let function3 (a, b) = a + b
    100 * function3 (1, 2)


///////////////////////////////////////////////////

let cylinderVolume (radius : int) (length : int) =
    
    let pi = 3.14159
    (float length) * pi * (float radius) * (float radius)

cylinderVolume 10 10

/////////////////////////////////////

let apply (transform : int -> int -> int ) y = transform y

let result3 = apply (fun x y -> x * y) 2 3

/////////////////////////////////////

let seq1 = seq { for i in 1 .. 10 -> (i, i*i) }
printfn $"List is: {seq1}"

////////////////////////////////////

let add3 number = 3. + number

let times2 number = 2. * number

let pow2 number = number ** 2.0

let operation number = 
    number
    |> add3
    |> times2
    |> pow2

operation 2

/////////////////////////

type PaymentType = 
    | BankAccount
    | Card

type BankAccountDetails =
    {
        BSB: string
        AccountNumber: string
        BankName: string
    }

type BankContactResponse =
    {
        Id: string
        Type: PaymentType
        ContactDetails: BankAccountDetails
    }

let dummyBankContactResponse = 
    [{ Id = "dummy id string 1"; Type = PaymentType.BankAccount; ContactDetails = { BSB = "222"; AccountNumber = "12345"; BankName = "ANZ" } };
     { Id = "dummy id string 2"; Type = PaymentType.BankAccount; ContactDetails = { BSB = "333"; AccountNumber = "6789"; BankName = "ING" } }]

//////////////////////////////

let add2 x = x + 2
let mult3 x = x * 3
let square x = x * x

let logMsgN msg x = printfn "%s%i" msg x; x

logMsgN "some number:" 10

let listOfFunctions = [
   mult3;
   square;
   add2;
   logMsgN "result=";
   ]

// compose all functions in the list into a single one
let allFunctions = List.reduce (>>) listOfFunctions

//test
allFunctions 5

//////////////////////////////////////////////

let fizzBuzz listOfNumbers = listOfNumbers
                                |> List.map (fun n -> (n, n%3, n%5))
                                |> List.map (function
                                    | (_,0,0) -> "FizzBuzz"
                                    | (_,0,_) -> "Fizz"
                                    | (_,_,0) -> "Buzz"
                                    | (n,_,_) -> string n + " Not a factor of 3 or 5")

let numbers = [1 .. 500]

fizzBuzz numbers

/////////////////////////////////////////

let elem1::elem2::rest = [1..10]

printfn $"elem1: {elem1}"
printfn $"elem2: {elem2}"
printfn $"rest: {rest}"

rest |> List.map (fun element -> printfn $"element: {element}")

/////////////////////////////////////////

let testFunction = 
    let x = 40 in
        let y = 50 in
            let z = x + y
            z

let testFunction' = 
    let x = 40
    x |> (fun x -> 
        let y = 50
        y |> (fun y -> 
            x + y))

let testFunction'' = 
    40 |> (fun x ->         
        50 |> (fun y -> 
            x + y))


testFunction
testFunction'
testFunction''

////////////////////////////////////////

// any function in f# take only one parameter

let someIntermediateFunction a b = 
    a + b + 10

let addXY x y =
    let partial = someIntermediateFunction x
    x + y + partial 1


addXY 10 20

//////////////////////////////////////

let tryParseInt (s: string) = 
    try 
        s |> int |> Some
    with :? System.FormatException -> 
        None

tryParseInt "3"
tryParseInt "sdsd"

/////////////////////////////////////

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

StringAddExplicitWorkflow.stringAddWorkflow "10" "20" "30" "40"
StringAddExplicitWorkflow.stringAddWorkflow "10" "20" "30" "abc"

////////////////////////////////////////////////////////////////////

open Microsoft.FSharp.Control
let getCustomerId name =
    if (name = "")
    then Error "getCustomerId failed"
    else Ok "Cust42"


///////////////////////////////////////////////////////////////////

let add1 x = x + 1

Some 2 |> Option.map add1

2 |> Option.map add1

/////////////////////////////////////

let tryParseIntegerFromStringOption (inputString: string) = 
    try 
        inputString |> int |> Some
    with :? System.FormatException -> 
        None

let tryParseIntegerFromStringResult (inputString: string) = 
    try 
        inputString |> int |> Ok
    with :? System.FormatException -> 
        Error $"Input {inputString} is not a string"

type OrderQty = OrderQty of int

let toOrderQty qty =
    if qty >= 1 then
        Some (OrderQty qty)
    else
        // only positive numbers allowed
        None

let toOrderQtyResult qty =
    if qty >= 1 then
        Ok (OrderQty qty)
    else
        Error "only positive numbers allowed"

let parseOrderQty str =
    str |> tryParseIntegerFromStringResult |> Result.bind toOrderQtyResult


type ResultBuilder() =

    member this.Bind(m, f) =
        match m with
        | Error e -> Error e
        | Ok a ->
            printfn "\tSuccessful: %A %A" a (f.GetType)
            f a

    member this.Return(x) =
        Ok x

let result = new ResultBuilder()

type OptionBuilder() =

    member this.Bind(m, f) =
        Option.bind f m

    member this.Return(x) =
        Some x

let option = new OptionBuilder()

let parseOrderResultCompExpression (str: string) =
    result {
        let! number = 
            tryParseIntegerFromStringResult str
            |> Result.map(fun x -> x + 10)
            |> Result.mapError (fun x -> "Some error while parsing")

        let! orderQuantity = 
            toOrderQtyResult number
            |> Result.mapError (fun x -> "Some error in order quantity")


        return orderQuantity        
        }
    

parseOrderResultCompExpression "2"

let parseOrderOptionCompExpression (str: string) =
    option {
        let! number = tryParseIntegerFromStringOption str
        let! orderQuantity = toOrderQty number
        return orderQuantity        
        }
    
parseOrderResultCompExpression "asdsadasd"

parseOrderOptionCompExpression "sdfsdf"

////////////////////////////////////////////

type TypesAvailable = 
    | TypeA
    | TypeB

type SomeTypeA = 
    {
        Value: string
        OtherValue: int
    }

type SomeTypeB = 
    {
        Value: string
    }

type ResponseData = 
    {
        TypeAData: SomeTypeA list
        TypeBData: SomeTypeB list
    }

type DataSource = 
    {
        TypeOfData: TypesAvailable
        Value: string
    }

let getData = 
    
    printfn $"Total iterations"

    let sourceData = 
        [
            {
                TypeOfData = TypesAvailable.TypeA
                Value = "type A value"
            };
            {
                TypeOfData = TypesAvailable.TypeB
                Value = "type B value"
            };
            {
                TypeOfData = TypesAvailable.TypeB
                Value = "type B value 2"
            };
            {
                TypeOfData = TypesAvailable.TypeA
                Value = "type A value 2"
            }
        ]
    
    {
        TypeAData = sourceData
                    |> List.map
                        (function
                            | {DataSource.TypeOfData = typeOfData; DataSource.Value = value;} when typeOfData = TypesAvailable.TypeA ->
                                { 
                                    Value = value
                                    OtherValue = 1
                                }                                
                                |> Some

                            | _ -> None
                        )
                    |> List.choose id
        
        TypeBData = sourceData
                    |> List.map
                        (function
                            | {DataSource.TypeOfData = typeOfData; DataSource.Value = value;} when typeOfData = TypesAvailable.TypeB ->   
                                { 
                                    Value = value
                                }
                                |> Some

                            | _ -> None
                        )
                    |> List.choose id
        
    }

let getData' = 

    let sourceData = 
        [
            {
                TypeOfData = TypesAvailable.TypeA
                Value = "type A value"
            };
            {
                TypeOfData = TypesAvailable.TypeB
                Value = "type B value"
            };
            {
                TypeOfData = TypesAvailable.TypeB
                Value = "type B value 2"
            };
            {
                TypeOfData = TypesAvailable.TypeA
                Value = "type A value 2"
            }
        ]

    {
        TypeAData = sourceData
                    |> List.filter
                        (function
                        | {DataSource.TypeOfData = typeOfData; DataSource.Value = _;} when typeOfData = TypesAvailable.TypeA -> true
                        | _ -> false)
                    |> List.map
                        (fun x ->
                            { 
                                Value = x.Value
                                OtherValue = 1
                            }
                        )
        
        TypeBData = sourceData
                    |> List.filter
                        (function
                        | {DataSource.TypeOfData = typeOfData; DataSource.Value = _;} when typeOfData = TypesAvailable.TypeB -> true
                        | _ -> false)
                    |> List.map
                        (fun x ->
                            { 
                                Value = x.Value
                            }
                        )
        
    }

let getData'' = 
    
    printfn $"Total iterations"

    let sourceData = 
        [
            {
                TypeOfData = TypesAvailable.TypeA
                Value = "type A value"
            };
            {
                TypeOfData = TypesAvailable.TypeB
                Value = "type B value"
            };
            {
                TypeOfData = TypesAvailable.TypeB
                Value = "type B value 2"
            };
            {
                TypeOfData = TypesAvailable.TypeA
                Value = "type A value 2"
            }
        ]

    let mutable count = 0
    let dataToReturn = 
        {
            TypeAData = sourceData
                        |> List.map
                            (function
                                | {DataSource.TypeOfData = typeOfData; DataSource.Value = value;} when typeOfData = TypesAvailable.TypeA ->                                
                                    count <- count + 1
                                    { 
                                        Value = value
                                        OtherValue = 1
                                    }                                
                                    |> Some

                                | _ -> 
                                    count <- count + 1
                                    None
                            )
                        |> List.choose id
        
            TypeBData = sourceData
                        |> List.map
                            (function
                                | {DataSource.TypeOfData = typeOfData; DataSource.Value = value;} when typeOfData = TypesAvailable.TypeB ->   
                                    count <- count + 1
                                    { 
                                        Value = value
                                    }
                                    |> Some

                                | _ -> 
                                    count <- count + 1
                                    None
                            )
                        |> List.choose id
        
        }

    (dataToReturn, count)
    
///////////////////////////////////////////////////////////////////////////////////

let divide (numerator: int) (denominator: int) =
    numerator / denominator

let compute numerator denominator = 
    try
        if numerator < denominator
        then
            "This division is only for fractions greater than 1" |> Error
        else            
            denominator
            |> divide numerator
            |> Ok        

    with
    | e -> $"Some unexpected error: {e.Message}" |> Error

let compute' numerator denominator = 
    denominator
    |> divide numerator
    |> Ok        

let funkyMessage message =
    $"I just funk up the message passed to me: {message}"

let actionOnValidResult result =
    match result with
    | value when value > 100 -> $"Can take some action on large value" |> Ok
    | _ -> "Can't action on small values " |> Error

compute 5 10
|> Result.mapError (fun errorMessage -> $"FAILURE. This is an error from computation. Details: {errorMessage}")
|> Result.map (fun result -> "SUCCESS. This is the result of computaiton: {result}")
|> Result.map (fun result -> funkyMessage result)
//|> Result.bind (actionOnValidResult)

compute 20000 30
|> Result.mapError (fun errorMessage -> $"FAILURE. This is an error from computation. Details: {errorMessage}")
|> Result.bind (actionOnValidResult)
|> Result.map (fun result -> $"SUCCESS. This is the result of computaiton: {result}")

compute' 100 0
|> Result.mapError (fun errorMessage -> $"FAILURE. This is an error from computation. Details: {errorMessage}")


    



    