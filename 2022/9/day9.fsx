open System
open System.IO


let showPath positions =
    let board =
        [| for _ in 1..30 do
               yield
                   [| for _ in 1..30 do
                          yield '_' |] |]

    for i in 0 .. (List.length positions - 1) do
        let pos = positions[i]
        board[ snd pos + 15 ].SetValue('#', fst pos + 15)

    board[ 15 ].SetValue('s', 15)

    Array.rev board
    |> Array.map (fun x -> Array.map string x |> String.concat "")
    |> String.concat "\n"
    |> Console.WriteLine

    positions

let parseInstructions (headPos: int * int) ins =
    match Seq.toList ins with
    | 'R' :: _ :: amount ->
        [ for i in 1 .. (String.Concat amount |> int) do
              yield (fst headPos + i, snd headPos) ]
    | 'L' :: _ :: amount ->
        [ for i in 1 .. (String.Concat amount |> int) do
              yield (fst headPos - i, snd headPos) ]
    | 'U' :: _ :: amount ->
        [ for i in 1 .. (String.Concat amount |> int) do
              yield (fst headPos, snd headPos + i) ]
    | 'D' :: _ :: amount ->
        [ for i in 1 .. (String.Concat amount |> int) do
              yield (fst headPos, snd headPos - i) ]
    | _ -> failwith ("invalid instruction")

let rec parseAllInstructions (startPos: int * int) (allIns: List<string>) =
    match allIns with
    | [] -> []
    | x :: xs ->
        let newPos = parseInstructions startPos x
        newPos :: parseAllInstructions (List.last newPos) xs

let touching (tailPos: int * int) (headPos: int * int) =
    (fst tailPos - 1 = fst headPos
     || fst tailPos + 1 = fst headPos
     || fst tailPos = fst headPos)
    && (snd tailPos - 1 = snd headPos
        || snd tailPos + 1 = snd headPos
        || snd tailPos = snd headPos)

let rec moveTailToHead (tailPos: int * int) (headPos: int * int) =
    match tailPos with
    | _ when touching tailPos headPos -> []
    | (x, y) when x = fst headPos ->
        (x, (if y > snd headPos then y - 1 else y + 1))
        :: moveTailToHead (x, (if y > snd headPos then y - 1 else y + 1)) headPos
    | (x, y) when y = snd headPos ->
        ((if x > fst headPos then x - 1 else x + 1), y)
        :: moveTailToHead ((if x > fst headPos then x - 1 else x + 1), y) headPos
    | (x, y) when x < fst headPos && y < snd headPos -> (x + 1, y + 1) :: moveTailToHead (x + 1, y + 1) headPos
    | (x, y) when x > fst headPos && y > snd headPos -> (x - 1, y - 1) :: moveTailToHead (x - 1, y - 1) headPos
    | (x, y) when x > fst headPos && y < snd headPos -> (x - 1, y + 1) :: moveTailToHead (x - 1, y + 1) headPos
    | (x, y) when x < fst headPos && y > snd headPos -> (x + 1, y - 1) :: moveTailToHead (x + 1, y - 1) headPos
    | (x, y) -> failwith (sprintf "invalid position combination: (%d, %d) -> (%d, %d)" x y (fst headPos) (snd headPos))

let playTailCatchup (headPos: List<int * int>) =
    List.fold (fun acc headPos -> List.append (moveTailToHead (List.head acc) headPos) acc) [ (0, 0) ] headPos
    |> List.rev

let rec repeatWith f amount input =
    match amount with
    | 0 -> input
    | x -> f input |> repeatWith f (x - 1)

let main () =
    File.ReadAllLines "input.txt"
    |> Array.toList
    |> parseAllInstructions (0, 0)
    |> List.fold (fun acc x -> List.append acc x) []
    |> repeatWith playTailCatchup 9
    |> Seq.distinct
    |> Seq.toList
    // |> showPath
    |> List.length
    |> Console.WriteLine

main ()
