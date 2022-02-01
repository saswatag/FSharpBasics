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