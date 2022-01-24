// allow creating only valid numbers (validity criteria here: [1,4000])
// next diplay validation error messages
// Results TaskResult


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


// driver
[-1; 1; 2000; 4000; 5000]
    |> List.map (fun n -> NumberWithin1To4000.tryCreateValidNumber n)
    |> List.map (fun n -> numberPrinter n)
