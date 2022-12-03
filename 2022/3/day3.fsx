open System
open System.IO

exception Oops of string

let scores: list<char> = List.append [ 'a' .. 'z' ] [ 'A' .. 'Z' ]

let rec splitGroups list =
    match list with
    | [] -> []
    | a :: b :: c :: xs -> [ a; b; c ] :: splitGroups xs
    | _ -> raise (Oops("no group of three"))

let rec getInCommon (left: string) (middle: string) (right: string) =
    match left with
    | "" -> raise (Oops("no common found"))
    | x when middle.Contains x[0] && right.Contains x[0] -> x[0]
    | _ -> getInCommon left[1..] middle right

let getCharValue (c: char) =
    List.findIndex (fun x -> c.Equals x) scores + 1

let main () =
    File.ReadAllLines "input.txt"
    |> Array.toList
    |> splitGroups
    |> List.map (fun l -> getInCommon l[0] l[1] l[2])
    |> List.map getCharValue
    |> List.sum
    |> Console.WriteLine

main ()
