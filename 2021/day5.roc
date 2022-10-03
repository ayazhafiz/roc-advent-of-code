app "day5"
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
            newline,
            comma,
            parseBytes,
            dropBefore,
            dropAfter,
            map,
            apply,
            digits,
            const,
            string,
        },
        U64FlatHashDict.{
            U64FlatHashDict,
        },
    ]
    provides [main] to pf

day = "day5"

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

Point : [Point I64 I64]
Line : [Horizontal (List Point), Vertical (List Point), Diagonal (List Point)]
Input : List Line

max = \m, n -> if m > n then m else n
min = \m, n -> if m < n then m else n

toLine: _, _, _, _ -> Line
toLine = \x1, y1, x2, y2 ->
    if x1 == x2 then
        List.range (min y1 y2) ((max y1 y2) + 1)
        |> List.map (\y -> Point x1 y)
        |> Vertical
    else if y1 == y2 then
        List.range (min x1 x2) ((max x1 x2) + 1)
        |> List.map (\x -> Point x y1)
        |> Horizontal
    else
        P xO yO xE yE =
            if x1 < x2 then
                P x1 y1 x2 y2
            else
                P x2 y2 x1 y1

        dx = xE - xO

        slope : I64
        slope = ((Num.toF64 (yE - yO)) / (Num.toF64 dx)) |> Num.round

        List.range 0 (dx + 1)
        |> List.map (\xi ->
            Point (xO + xi) (yO + (xi * slope))
        )
        |> Diagonal

parseInput : _ -> Result Input _
parseInput = \input ->
    i64 = digits |> map Num.toI64

    numPair =
        const (\n1 -> \n2 -> P n1 n2)
        |> apply i64
        |> apply (dropBefore comma i64)

    parseLine =
        const (\P x1 y1 -> \P x2 y2 -> toLine x1 y1 x2 y2)
        |> apply numPair
        |> apply (dropBefore (string " -> ") numPair)
        |> dropAfter newline

    parseLines =
        many parseLine

    when parseBytes parseLines input is
        Ok a -> Ok a
        Err (Msg s) -> Err (Msg s)

update : U64FlatHashDict v, U64, v, (v -> v) -> U64FlatHashDict v
update = \dict, k, default, updator ->
    when U64FlatHashDict.get dict k is
        Ok v -> U64FlatHashDict.insert dict k (updator v)
        Err _ -> U64FlatHashDict.insert dict k default

updatePoints = \dict, points ->
    List.walk points dict \d, Point x y -> update d (Num.toU64 (x * 10000 + y)) 1 (\count -> count + 1)

countOverlaps = \dict ->
    U64FlatHashDict.walkValues dict 0 \overlaps, c ->
        if c > 1 then overlaps + 1 else overlaps

part1 = \lines ->
    dict1 = List.walk lines (U64FlatHashDict.empty 0) \dict, line ->
        when line is
            Horizontal points | Vertical points ->
                updatePoints dict points
            Diagonal _ -> dict

    countOverlaps dict1 |> Num.toStr

part2 = \lines ->
    dict1 = List.walk lines (U64FlatHashDict.empty 0) \dict, line ->
        when line is
            Horizontal points | Vertical points | Diagonal points ->
                updatePoints dict points

    countOverlaps dict1 |> Num.toStr
