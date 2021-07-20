#load "Parser.fsx"

open System
open Parser

type Jvalue =
    |JString of string
    |JNumber of float
    |JBool of bool
    |JNull
    |JObject of Map<string, Jvalue>
    |JArray of Jvalue list


let createParserForwardToRef<'a>() =
        
    let dummyParser: Parser<'a> =
        let innerFn _ =
            failwith "unfixed parser definition"
        {parseFn = innerFn; label = "unknown"}
    
    let parserRef = ref dummyParser
    
    // (Note: "!" is the deferencing operator)
    let innerFn input  =
        runOnInput !parserRef input
    
    let wrapperParser = {parseFn = innerFn; label = "unknown"}
    
    wrapperParser, parserRef

let jValue,jValueRef = createParserForwardToRef<JValue>()


let (>>%)  p x = 
    p |>> (fun _ -> x)

let jNull  = 
    pstring "null"
    >>% JNull
    <?> "null"

let jBool = 
    let jTrue = 
        pstring "true"
        >>% JBool true
    let jFalse = 
        pstring "false"
        >>% JBool false
    jTrue <|> jFalse
    <?> "bool"

let jUnescapedChar = 
    let label = "char"
    satisfy (fun ch -> ch <> '\\' && ch <> '\"') label

let jEscapedChar = 
    [
    // (stringToMatch, resultChar)
      ("\\\"",'\"')      // quote
      ("\\\\",'\\')      // reverse solidus
      ("\\/",'/')        // solidus
      ("\\b",'\b')       // backspace
      ("\\f",'\f')       // formfeed
      ("\\n",'\n')       // newline
      ("\\r",'\r')       // cr
      ("\\t",'\t')       // tab
    ]

    |> List.map (fun (toMatch,result) ->
        pstring toMatch >>% result)
    |> choice
    <?> "escaped char"

let jUnicodeChar = 
    let backslash = pchar '\\'
    let uchar = pchar 'u'
    let hexdigits = 
        anyOf (['0' ..'9'] @ ['A' .. 'Z'] @ ['a' .. 'f'])
    let fourhexdigits = hexdigits .>>. hexdigits .>>. hexdigits .>>. hexdigits

    //convert the parser output to a char
    let convertToChar (((h1,h2),h3),h4)= 
        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
        Int32.Parse (str, Globalization.NumberStyles.HexNumber) |> char

    backslash >>. uchar >>. fourhexdigits 
    |>> convertToChar

let quotedString =
    let quote = pchar '\"' <?> "quotes"
    let jchar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar

    quote >>. manyChars jchar .>> quote

let jString = 
    quotedString 
    |>> JString
    <?> "quoted string"


let jNumber = 
    let optSign = opt (pchar '-')

    let zero  = pstring "0"

    let digitOneNine = 
        satisfy (fun ch -> Char.IsDigit ch && ch <> '0') "1-9"

    let digit = 
        satisfy (fun ch -> Char.IsDigit ch) "digits"

    let point = pchar '.'

    let e = pchar 'e' <|> pchar 'E'

    let optPlusMinus = opt((pchar '-') <|> (pchar '+'))

    let nonZeroInt =
        digitOneNine .>>. manyChars digit
        |>> fun (first, rest ) -> string first + rest

    let intPart = zero <|> nonZeroInt

    let fractionalPart = point >>. manyChars digit

    let exponentPart = e >>. optPlusMinus .>>. manyChars1 digit

    let (|>?) opt f =
        match opt with
        |None -> ""
        |Some x-> f x

    let convertToJNumber (((optSign, intPart),fracPart),exp) = 
    
        let signStr = 
            optSign
            |>? string

        let fractionPartStr =
            fracPart
            |>? (fun digit -> "." + digit)

        let expPartStr =
            exp
            |>? fun (optSign, digits) ->
                let sign = optSign |>? string
                "e" + sign + digits

        (signStr + intPart + fractionPartStr + expPartStr)
        |> float
        |>JNumber

    optSign .>>. intPart .>>. opt fractionalPart .>>. opt exponentPart
    |>> convertToJNumber 
    <?> "number"

let jNumber_ = jNumber .>> spaces1



let jArray = 
    let left = pchar '[' .>> spaces
    let right = pchar ']' .>> spaces
    let comma = pchar ',' .>> spaces
    let value = jValue .>> spaces

    let values = sepBy value comma

    between left values right
    |>> JArray
    <?> "Array"


let jObject = 
    let left  = spaces >>. pchar '{' >>. spaces
    let right  =  pchar '}' .>> spaces
    let colon  = pchar ':' .>> spaces
    let comma = pchar ',' .>> spaces
    let key = quotedString .>> spaces
    let value = jValue .>> spaces


    let keyValue = (key .>> colon) .>>. value
    let keyValues  = sepBy keyValue comma

    between left keyValues right
    |>>Map.ofList
    |>> JObject
    <?> "Object"

jValueRef := choice 
[
jNull
jBool
jString
jNumber
jArray
jObject
]