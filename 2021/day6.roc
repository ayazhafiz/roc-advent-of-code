app "day6"
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
            comma,
            parseBytes,
            newline,
            dropAfter,
            digit,
            sepBy,
            map,
        },
    ]
    provides [main] to pf

day = "day6"

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
            Ok parsed -> Task.succeed parsed
            Err e -> Task.fail e

    Task.attempt task \res ->
        when res is
            Ok data ->
                (part1 data |> \s -> Stdout.line "Part 1: \(s)") |> Task.await (\_ -> part2 data |> \s -> Stdout.line "Part 2: \(s)") |> Program.exit 0
            Err (Msg s) -> Stderr.line s |> Program.exit 1
            Err _ -> Stderr.line "error!" |> Program.exit 1

Input : List U64

initSt = List.range 0 (8 + 1) |> List.map \_ -> 0

update = \lst, index, default, updator ->
    basis = List.get lst index |> Result.withDefault default
    List.set lst index (updator basis)

toInitCounts = \timers ->
    List.walk timers initSt \st, time ->
        update st time 0 (\count -> count + 1)

parseInput : _ -> Result Input _
parseInput = \input ->
    nat = digit |> map Num.toNat

    parser =
        nat |> sepBy comma |> dropAfter newline

    when parseBytes parser input is
        Ok a -> Ok (toInitCounts a)
        Err (Msg s) -> Err (Msg s)

simulateDay = \fish ->
    walker = \{i, result}, count ->
        result1 =
            if i == 0 then
                update result 6 0 (\counts -> counts + count)
                |> update 8 0 (\counts -> counts + count) 
            else
                update result (i - 1) 0 (\counts -> counts + count)

        {i: i + 1, result: result1}

    List.walk fish {i: 0, result: initSt} walker
    |> .result

simulateNDays = \n, counts ->
    List.walk (List.range 0 n) counts \c, _ -> simulateDay c

countFish = \counts ->
    List.walk counts 0 Num.add

part1 = \counts -> simulateNDays 80 counts |> countFish |> Num.toStr
part2 = \counts -> simulateNDays 256 counts |> countFish |> Num.toStr
