app "day8"
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
            const,
            space,
            oneOf,
            parseBytes,
            many,
            oneOrMore,
            codeunit,
            dropBefore,
            dropAfter,
            newline,
            apply,
            byte,
            map,
        },
    ]
    provides [main] to pf

day = "day8"

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

Segment : [A, B, C, D, E, F, G]
Signal : Set Segment
Input : List { signals: List Signal, output: List Signal }

nOfSegment = \s ->
    when s is
        A -> 0
        B -> 1
        C -> 2
        D -> 3
        E -> 4
        F -> 5
        G -> 6

sortSegment = \s1, s2 ->
    n1 = nOfSegment s1
    n2 = nOfSegment s2
    if n1 == n2 then
        EQ
    else if n1 < n2 then
        LT
    else
        GT

parseInput : _ -> Result Input _
parseInput = \input ->
    segment : Parser _ Segment
    segment =
        oneOf [
            byte 'a' |> codeunit |> map (\_ -> A),
            byte 'b' |> codeunit |> map (\_ -> B),
            byte 'c' |> codeunit |> map (\_ -> C),
            byte 'd' |> codeunit |> map (\_ -> D),
            byte 'e' |> codeunit |> map (\_ -> E),
            byte 'f' |> codeunit |> map (\_ -> F),
            byte 'g' |> codeunit |> map (\_ -> G),
        ]

    signal = oneOrMore segment |> map (\l -> List.sortWith l sortSegment |> Set.fromList)

    signalB : Parser _ Signal
    signalB = signal |> dropAfter space

    signalA : Parser _ Signal
    signalA = space |> dropBefore signal

    line =
        const (\signals -> \output -> {signals, output})
        |> apply (many signalB)
        |> dropAfter (codeunit (byte '|'))
        |> apply (many signalA)
        |> dropAfter newline

    parser =
        many line

    when parseBytes parser input is
        Ok a -> Ok a
        Err (Msg s) -> Err (Msg s)

digitLengths : Dict Nat (List U64)
digitLengths =
    Dict.empty
    |> Dict.insert 2 [1]
    |> Dict.insert 3 [7]
    |> Dict.insert 4 [4]
    |> Dict.insert 5 [2, 3, 5]
    |> Dict.insert 6 [6, 9, 0]
    |> Dict.insert 7 [8]

unreachable1 : {} -> a
unreachable2 : {} -> a
unreachable3 : {} -> a

dictGetU = \dict, k ->
    when Dict.get dict k is
        Ok v -> v
        _ -> unreachable1 {}

dictGetU1 = \dict, k ->
    when Dict.get dict k is
        Ok v -> v
        _ -> unreachable2 {}

listGetU = \l, i ->
    when List.get l i is
        Ok v -> v
        _ -> unreachable3 {}

setDiffIs = \s1, s2, d -> Set.len (Set.difference s1 s2) == d

solveSignals : List Signal -> Dict U64 (Set Segment)
solveSignals = \signals ->
    # Figure out 1, 7, 4, 8
    assoc1748 =
        List.walk signals Dict.empty \assoc, signal ->
            n = (Set.len signal)

            candidates = dictGetU digitLengths n

            if List.len candidates == 1 then
                number = listGetU candidates 0
                Dict.insert assoc number signal
            else
                assoc

    solve = \assocBasis, solvingFor, basisN, basisOnlyHas, candOnlyHas ->
        basisSignal = dictGetU1 assocBasis basisN
        List.walk signals assocBasis \assoc, signal ->
            if setDiffIs basisSignal signal basisOnlyHas &&
                setDiffIs signal basisSignal candOnlyHas then
                Dict.insert assoc solvingFor signal
            else
                assoc

    assoc1748
    |> solve 3 7 0 2 #-- |7 \ 3| = 0 and |3 \ 7| = 2
    |> solve 2 3 1 1 #-- |3 \ 2| = 1 and |2 \ 3| = 1
    |> solve 5 2 2 2 #-- |2 \ 5| = 2 and |5 \ 2| = 2
    |> solve 6 5 0 1 #-- |5 \ 6| = 0 and |6 \ 5| = 1
    |> solve 9 4 0 2 #-- |4 \ 9| = 0 and |9 \ 4| = 2
    |> solve 0 3 1 2 #-- |3 \ 0| = 1 and |0 \ 3| = 2

countOccur = \signals, outputs ->
    List.walk outputs 0 \accum, output ->
        if Set.contains signals output then
            accum + 1
        else
            accum

countOccurences1478 = \solved, outputs ->
    signals = Set.fromList [
        dictGetU solved 1,
        dictGetU solved 4,
        dictGetU solved 7,
        dictGetU solved 8,
    ]
    countOccur signals outputs

invert : Dict k v -> Dict v k
invert = \dict ->
    Dict.walk dict Dict.empty \new, k, v ->
        Dict.insert new v k

buildOutputNum = \{signals, output} ->
    solved = solveSignals signals |> invert
    List.walk output 0 \n, out ->
        n * 10 + (dictGetU solved out)

part1 = \input ->
    List.walk input 0 (\accum, {signals, output} ->
        solved = solveSignals signals
        accum1 = countOccurences1478 solved output
        accum + accum1
    )
    |> Num.toStr

part2 = \input ->
    List.walk input 0 (\accum, line -> accum + buildOutputNum line)
    |> Num.toStr
