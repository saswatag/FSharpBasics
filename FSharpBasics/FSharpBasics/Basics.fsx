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


// allow creating only valid numbers (validity criteria here: [1,4000])
// next diplay validation error messages

module NumberWithin1To4000 = 
    type ValidNumber = private ValidNumber of int

    let (|Within1To4000|NotInRange|) (number: int) = 
        if 1 <= number && number <= 4000 then Within1To4000 else NotInRange

    let tryCreateValidNumber (number:int) = 
        match number with 
        | Within1To4000 -> Some (ValidNumber number)
        | NotInRange -> None

    let numberValue (ValidNumber number) = number    

    let displayMessage (number: ValidNumber) = $"The number {numberValue number} is within range [1,4000]"


open NumberWithin1To4000
let invalidNumber = NumberWithin1To4000.tryCreateValidNumber -1
let invalidNumber1 = NumberWithin1To4000.tryCreateValidNumber 5000
let validNumber = NumberWithin1To4000.tryCreateValidNumber 2000

let numberPrinter (number: ValidNumber option) =
    match number with
    | Some number -> NumberWithin1To4000.displayMessage number
    | None -> $"The number passed in is not in range"


[-1; 1; 2000; 4000; 5000]
    |> List.map (fun n -> NumberWithin1To4000.tryCreateValidNumber n)
    |> List.map (fun n -> numberPrinter n)
