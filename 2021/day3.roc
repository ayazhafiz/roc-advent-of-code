app "day3"
    packages { pf: "../../roc/examples/interactive/cli-platform/main.roc" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Arg,
        pf.Path,
        pf.Task.{ Task },
        pf.File,
        pf.Program.{ Program },
        Parser.{
            Parser,
            many,
            spaces,
            newline,
            parse,
            oneOf,
            oneOrMore,
            map,
            apply,
            digit,
            digits,
            const,
            string,
        },
    ]
    provides [main] to pf

day = "day3"

main : Program
main = Program.withArgs \args ->
    parser = Arg.str { long: "file", short: "f" } |> Arg.program { name: day }

    when Arg.parseFormatted parser args is
        Ok file ->
            mainF file

        Err helpMenu ->
            Stderr.line helpMenu
            |> Program.exit 1

mainF = \file ->
    task =
        input <- Path.fromStr file |> File.readBytes |> Task.await
        when parseInput input is
            Ok nums -> Task.succeed nums
            Err e -> Task.fail e

    Task.attempt task \res ->
        when res is
            Ok data ->
                (part1 data |> \s -> Stdout.line "Part 1: \(s)") |> Task.await (\_ -> part2 data |> \s -> Stdout.line "Part 2: \(s)") |> Program.exit 0
            Err (Msg s) -> Stderr.line s |> Program.exit 1
            Err _ -> Stderr.line "error!" |> Program.exit 1

Line : List U64
Input : [P Line (List Line)]

parseInput : _ -> Result [P Line (List Line)] _
parseInput = \input ->
    u64 = digit |> map Num.toU64

    parseLine =
        const (\nums -> \_newline -> nums)
        |> apply (oneOrMore u64)
        |> apply newline

    parseLines =
        const (\first -> \rest -> P first rest)
        |> apply parseLine
        |> apply (many parseLine)
        
    when parse parseLines input is
        Ok a -> Ok a
        Err (Msg s) -> Err (Msg s)

part1 : Input -> Str
part1 = \P first rest ->
    walker = \sums, line ->
        List.map2 sums line (\x, y -> x + y)

    totals = List.walk rest first walker

    majority : U64
    majority = ((Num.toF64 (1 + List.len rest)) / 2) |> Num.round

    P gf ef = List.walk totals (P 0 0) \P g e, sum ->
        g1 = Num.shiftLeftBy g 1 + (if sum >= majority then 1 else 0)
        e1 = Num.shiftLeftBy e 1 + (if sum >= majority then 0 else 1)
        P g1 e1

    Num.toStr (gf * ef)

ofBinary = \bits -> List.walk bits 0 \t, n -> (Num.shiftLeftBy t 1) + n

part2 : Input -> Str
part2 = \P first rest ->
    lineSize = List.len first

    filterByMajority = \wanted ->
        help = \candidates, i ->
            if List.len candidates == 1 then
                when List.get candidates 0 is
                    Ok c -> Ok c
                    Err _ -> Err "unreachable"
            else
                walker = \sums, line ->
                    List.map2 sums line (\x, y -> x + y)

                totals = List.walk candidates (List.repeat 0 lineSize) walker

                majority : U64
                majority = ((Num.toF64 (1 + List.len candidates)) / 2) |> Num.round

                when List.get totals i is
                    Err _ -> Err "unreachable"
                    Ok bitCount ->
                        cands1 = List.keepIf candidates \bits ->
                            List.get bits i
                            |> Result.map (\n -> n == (if bitCount >= majority then wanted else 1 - wanted))
                            |> Result.withDefault Bool.false

                        help cands1 (i + 1)

        help (List.append rest first) 0

    o2Result = filterByMajority 1
    co2Result = filterByMajority 0

    when P o2Result co2Result is
        P (Ok o2) (Ok co2) -> Num.toStr (ofBinary o2 * ofBinary co2)
        _ -> "<unreachable>"
