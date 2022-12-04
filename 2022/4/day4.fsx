open System
open System.IO

exception Oops of string

let overlap (left: List<int>) (right: List<int>) =
    match left with
    | a :: b :: [] -> (b >= right[0] && a <= right[1]) || (a <= right[1] && b >= right[0])
    | _ -> raise (Oops("no group of two"))

let main () =
    File.ReadAllLines "input.txt"
    |> Array.toList
    |> List.map (fun x ->
        x.Split ','
        |> Array.map (fun (x: string) -> x.Split '-' |> Array.toList |> List.map int))
    |> List.map (fun l -> if overlap l[0] l[1] then 1 else 0)
    |> List.sum
    |> Console.WriteLine

main ()
