# Derived largely from the CSV parser in the Roc repo. Thanks to Marten (qqwy),
# the original author!

interface Parser exposes [
        Parser,
        custom,
        parse,
        parseBytes,
        const,
        fail,
        alt,
        apply,
        map,
        many,
        oneOrMore,
        oneOf,
        opt,
        string,
        space,
        spaces,
        newline,
        comma,
        digits,
        digit,
        codeunit,
        dropBefore,
        dropAfter,
        sepBy,
        byte,
    ] imports []

Parser input a := input -> ParseResult input a

ParseResult input a : Result { val : a, input : input } ParseError

ParseError : [Msg Str]

custom : (input -> ParseResult input a) -> Parser input a
custom = \f -> @Parser f

parseHead : Parser input a, input -> ParseResult input a
parseHead = \@Parser parser, input -> parser input

parse : Parser input a, input, (input -> Result {} Str) -> Result a ParseError
parse = \parser, input, isComplete ->
    when parseHead parser input is
        Ok { val: val, input: rest } ->
            when isComplete rest is
                Ok {} -> Ok val
                Err msg -> Err (Msg msg)

        Err (Msg msg) -> Err (Msg msg)

parseBytes : Parser Bytes a, Bytes -> Result a ParseError
parseBytes = \parser, input ->
    parse parser input \bytes ->
        if List.isEmpty bytes then
            Ok {}
        else
            when Str.fromUtf8 bytes is
                Ok s -> Err "leftover: \(s)"
                Err _ -> Err "failed to parse as utf-8"

fail : Str -> Parser * *
fail = \msg ->
    custom \_input -> Err (Msg msg)

const : a -> Parser * a
const = \val ->
    custom \input ->
        Ok { val: val, input: input }

alt : Parser input a, Parser input a -> Parser input a
alt = \first, second ->
    custom \input ->
        when parseHead first input is
            Ok { val: val, input: rest } -> Ok { val: val, input: rest }
            Err (Msg firstErr) ->
                when parseHead second input is
                    Ok { val: val, input: rest } -> Ok { val: val, input: rest }
                    Err (Msg secondErr) ->
                        Err (Msg ("\(firstErr) or \(secondErr)"))

dropBefore : Parser input dropped, Parser input after -> Parser input after
dropBefore = \dropped, after ->
    custom \input ->
        parseHead dropped input
        |> Result.try \{ input: rest } -> parseHead after rest

dropAfter : Parser input before, Parser input dropped -> Parser input before
dropAfter = \before, dropped ->
    custom \input ->
        { val, input: rest } <- Result.try (parseHead before input)
        parseHead dropped rest
        |> Result.map \{ input: rest2 } ->
            { val, input: rest2 }

apply : Parser input (a -> b), Parser input a -> Parser input b
apply = \funParser, valParser ->
    combined = \input ->
        { val: funVal, input: rest } <- Result.try (parseHead funParser input)
        parseHead valParser rest
        |> Result.map \{ val: val, input: rest2 } ->
            { val: funVal val, input: rest2 }

    custom combined

map : Parser input a, (a -> b) -> Parser input b
map = \simpleParser, transform ->
    const transform
    |> apply simpleParser

many : Parser input a -> Parser input (List a)
many = \parser ->
    custom \input ->
        manyImpl parser [] input

manyImpl : Parser input a, List a, input -> ParseResult input (List a)
manyImpl = \parser, vals, input ->
    result = parseHead parser input

    when result is
        Err _ ->
            Ok { val: vals, input: input }

        Ok { val: val, input: inputRest } ->
            manyImpl parser (List.append vals val) inputRest

oneOrMore : Parser input a -> Parser input (List a)
oneOrMore = \parser ->
    const (\val -> \vals -> List.prepend vals val)
    |> apply parser
    |> apply (many parser)

oneOf : List (Parser input a) -> Parser input a
oneOf = \parsers ->
    custom \input ->
        List.walkUntil parsers (Err (Msg "(no possibilities)")) \_, parser ->
            when parseHead parser input is
                Ok val ->
                    Break (Ok val)

                Err problem ->
                    Continue (Err problem)

opt : Parser input a -> Parser input (Result a [Missing])
opt = \parser -> oneOf [
        parser |> map Ok,
        const (Err Missing),
    ]

sepBy : Parser input val, Parser input sep -> Parser input (List val)
sepBy = \valParser, sepParser ->
    custom \input -> sepByImpl valParser sepParser [] input

sepByImpl : Parser input a, Parser input sep, List a, input -> ParseResult input (List a)
sepByImpl = \valP, sepP, allVals, input ->
    parseHead valP input
    |> Result.try \{ val, input: rest } ->
        allVals1 = List.append allVals val

        when parseHead sepP rest is
            Err _ -> Ok { val: allVals1, input: rest }
            Ok { input: afterSep } ->
                sepByImpl valP sepP allVals1 afterSep

Bytes : List U8

string : Str -> Parser Bytes Str
string = \s ->
    Str.toUtf8 s
    |> stringRaw
    |> map (\_val -> s)

stringRaw : List U8 -> Parser Bytes (List U8)
stringRaw = \expectedString ->
    custom \input ->
        { before: start, others: inputRest } = List.split input (List.len expectedString)

        if start == expectedString then
            Ok { val: expectedString, input: inputRest }
        else
            errorString = strFromRaw expectedString
            otherString = strFromRaw start
            inputString = strFromRaw input

            Err (Msg "expected string `\(errorString)` but found `\(otherString)`.\nWhile reading: \(inputString)")

strFromRaw : Bytes -> Str
strFromRaw = \rawStr ->
    rawStr
    |> Str.fromUtf8
    |> Result.withDefault "Unexpected problem while turning a List U8 (that was originally a Str) back into a Str. This should never happen!"

strFromCodeunit : U8 -> Str
strFromCodeunit = \cu ->
    strFromRaw [cu]

codeunit : U8 -> Parser Bytes U8
codeunit = \expectedCodeUnit ->
    custom \input ->
        { before: start, others: inputRest } = List.split input 1

        when List.get start 0 is
            Err OutOfBounds ->
                errorChar = strFromCodeunit expectedCodeUnit

                Err (Msg "expected char `\(errorChar)` but input was empty.")

            Ok startCodeunit ->
                if startCodeunit == expectedCodeUnit then
                    Ok { val: expectedCodeUnit, input: inputRest }
                else
                    errorChar = strFromCodeunit expectedCodeUnit
                    otherChar = strFromRaw start
                    inputStr = strFromRaw input

                    Err (Msg "expected char `\(errorChar)` but found `\(otherChar)`.\n While reading: `\(inputStr)`")

byte = \c -> Num.toU8 c

space : Parser Bytes {}
space =
    codeunit (byte ' ')
    |> map (\_ -> {})

spaces : Parser Bytes Str
spaces =
    oneOrMore (codeunit (byte ' '))
    |> map (\l -> Str.repeat " " (List.len l))

newline : Parser Bytes {}
newline =
    codeunit (byte '\n')
    |> map (\_ -> {})

comma : Parser Bytes {}
comma =
    codeunit (byte ',')
    |> map (\_ -> {})

digit : Parser Bytes U8
digit =
    digitParsers =
        List.range 0 10
        |> List.map \digitNum ->
            digitNum
            + 48
            |> codeunit
            |> map (\_ -> digitNum)

    oneOf digitParsers

digits : Parser Bytes (Int *)
digits =
    oneOrMore digit
    |> map \digitsList ->
        digitsList
        |> List.map Num.intCast
        |> List.walk 0 (\sum, digitVal -> 10 * sum + digitVal)
