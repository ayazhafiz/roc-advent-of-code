app "day1"
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
            map,
            apply,
            digits,
            const,
            string,
        },
    ]
    provides [main] to pf

day = "day2"

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

Command : [Forward U64, Down U64, Up U64]

parseInput : _ -> Result (List Command) _
parseInput = \input ->
    parseCmd : Parser _ (U64 -> Command)
    parseCmd =
        oneOf [
            map (string "forward") (\_ -> (\n -> Forward n)),
            map (string "down") (\_ -> (\n -> Down n)),
            map (string "up") (\_ -> (\n -> Up n)),
        ]
    parseLine =
        const (\cmd -> \_spaces -> \num -> \_newline -> cmd num)
        |> apply parseCmd
        |> apply spaces
        |> apply digits
        |> apply newline
        
    when parse (many parseLine) input is
        Ok a -> Ok a
        Err (Msg s) -> Err (Msg s)

part1 : List Command -> Str
part1 = \cmds ->
    walker = \st, cmd ->
        when cmd is
            Forward n -> {st & h: st.h + n}
            Down n -> {st & d: st.d + n}
            Up n -> {st & d: st.d - n}

    {h, d} = List.walk cmds { h: 0, d: 0 } walker

    result = Num.toStr (h * d)
    result

part2 = \cmds ->
    walker = \st, cmd ->
        when cmd is
            Down n -> {st & a: st.a + n}
            Up n -> {st & a: st.a - n}
            Forward n -> { st & h: st.h + n, d: st.d + (st.a * n) }
    
    {h, d} = List.walk cmds {h: 0, d: 0, a: 0} walker

    Num.toStr (h * d)
