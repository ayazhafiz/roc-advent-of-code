app "day4"
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
            parseBytes,
            oneOf,
            oneOrMore,
            opt,
            dropBefore,
            dropAfter,
            sepBy,
            map,
            apply,
            digit,
            digits,
            const,
            string,
            codeunit,
        },
    ]
    provides [main] to pf

day = "day4"

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

Square : [Marked, Sq U64]
Board : List (List Square)
Input : [Bingo (List U64) (List Board)]

parseInput : _ -> Result Input _
parseInput = \input ->
    u64 = digits |> map Num.toU64
    square = u64 |> map Sq

    parseSepSquare =
        (opt spaces)
        |> dropBefore square

    parseBoardLine =
        oneOrMore parseSepSquare
        |> dropAfter newline

    parseBoard =
        newline
        |> dropBefore (many parseBoardLine)

    parseNumbers =
        u64
        |> sepBy (codeunit (Num.toU8 ','))
        |> dropAfter newline

    parseLines =
        const (\nums -> \boards -> Bingo nums boards)
        |> apply parseNumbers
        |> apply (many parseBoard)

    when parseBytes parseLines input is
        Ok a -> Ok a
        Err (Msg s) -> Err (Msg s)

printBoard = \board ->
    List.map board (\row ->
        List.map row (\sq ->
            when sq is
                Marked -> "X"
                Sq n -> Num.toStr n)
        |> Str.joinWith " ")
    |> Str.joinWith "\n"

markBoard = \num, board ->
    List.map board \row ->
        List.map row \square ->
            when square is
                Marked -> Marked
                Sq n -> if n == num then Marked else Sq n

announce = \num, boards ->
    List.map boards (\b -> markBoard num b)

isMarked = \sq -> sq == Marked

isWinner = \board ->
    winnerRow = \b, row ->
        List.get b row
        |> Result.map (\r -> List.all r isMarked)
        |> Result.withDefault Bool.false

    winnerCol = \b, col ->
        List.all b \row ->
            List.get row col
            |> Result.map isMarked
            |> Result.withDefault Bool.false

    maxRowCol = List.len board
    range = List.range 0 (maxRowCol - 1)

    List.any range (\i -> ( winnerRow board i ) || (winnerCol board i))

calculateScore = \board, winningNum ->
    sumUnmarked = List.walk board 0 \sum, row -> List.walk row sum \sum1, sq ->
        when sq is
            Marked -> sum1
            Sq unmarked -> sum1 + unmarked

    sumUnmarked * winningNum

part1 = \Bingo nums boardsInit ->
    boardsRes = List.walkUntil nums (NoWinner boardsInit) \boardSt, num ->
        when boardSt is
            Winner b n -> Break (Winner b n)
            NoWinner boards ->
                boards1 = announce num boards
                when List.findFirst boards1 isWinner is
                    Ok b -> Break (Winner b num)
                    Err _ -> Continue (NoWinner boards1)

    when boardsRes is
        Winner b n ->
            calculateScore b n |> Num.toStr
        NoWinner _ -> List.map boardsInit printBoard |> Str.joinWith "LOSERS" |> Str.concat "THE BOARDS:\n"

part2 = \Bingo nums boardsInit ->
    boardsRes = List.walkUntil nums (StillPlaying boardsInit) \boardSt, num ->
        when boardSt is
            Last b n -> Break (Last b n)
            StillPlaying boards ->
                boards1 =
                    List.dropIf boards isWinner
                    |> \bs -> announce num bs
                when (P (List.findFirst boards1 isWinner) (List.len boards1)) is
                    P (Ok winner) 1 -> Break (Last winner num)
                    P _ _ -> Continue (StillPlaying boards1)
    

    when boardsRes is
        Last b n ->
            calculateScore b n |> Num.toStr
        StillPlaying _ -> "no winner!"
