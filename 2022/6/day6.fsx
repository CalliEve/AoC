open System
open System.IO

let rec hasDuplicateInFourTeen (input: List<char>) =
    match input with
    | x :: xs when List.contains x xs -> true
    | _ :: xs -> hasDuplicateInFourTeen xs
    | [] -> false

let rec findUniqueFour (acc: List<char>) (input: List<char>) =
    match input with
    | x :: xs when List.length acc < 14 || hasDuplicateInFourTeen (x :: acc[..12]) -> findUniqueFour (x :: acc) xs
    | x :: _ -> x :: acc |> List.rev
    | _ -> acc

let main () =
    File.ReadAllLines "input.txt"
    |> Array.head
    |> Seq.toList
    |> findUniqueFour []
    |> List.length
    |> Console.WriteLine

main ()
