// allow creating only valid numbers (validity criteria here: [1,4000])
// later change the validity criteria

module NumberWithin1To4000 = 
    type ValidNumber = private ValidNumber of int

    let (|Within1To4000|NotInRange|) (number: int) = 
        if 1 <= number && number <= 4000 then Within1To4000 else NotInRange

    let tryCreateValidNumber (number:int) = 
        match number with 
        | Within1To4000 -> Some (ValidNumber number)
        | NotInRange -> None

    let numberValue (ValidNumber number) = number

    let displayMessage (number: ValidNumber option) = 
        match number with
        | Some number -> $"The number {numberValue number} is in the range [1,4000]"
        | None -> $"The number passed in is not in the range [1,4000]"


open NumberWithin1To4000
let invalidNumber = NumberWithin1To4000.tryCreateValidNumber -1
let invalidNumber1 = NumberWithin1To4000.tryCreateValidNumber 5000
let validNumber = NumberWithin1To4000.tryCreateValidNumber 2000

let numberPrinter validNumber =
    NumberWithin1To4000.displayMessage validNumber

numberPrinter invalidNumber
numberPrinter validNumber
