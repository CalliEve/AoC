open System
open System.IO

let rec transpose grid =
    match grid with
    | [] -> []
    | [] :: _ -> []
    | xs -> List.map List.head xs :: transpose (List.map List.tail xs)

let rec calcScore x list =
    match list with
    | [] -> 0
    | y :: _ when fst y >= x -> 1
    | _ :: ys -> 1 + calcScore x ys

let rec checkRow row =
    match row with
    | (x, y) :: xs -> (x, y * calcScore x xs) :: checkRow xs
    | [] -> []

let main () =
    File.ReadAllLines "input.txt"
    |> Array.toList
    |> List.map Seq.toList
    |> List.map (fun l -> List.map (fun x -> (int x, 1)) l)
    |> List.map checkRow
    |> List.map List.rev
    |> List.map checkRow
    |> transpose
    |> List.map checkRow
    |> List.map List.rev
    |> List.map checkRow
    |> List.map (fun l -> List.map (fun (_, seen) -> seen) l)
    |> List.map List.max
    |> List.max
    |> Console.WriteLine

main ()
