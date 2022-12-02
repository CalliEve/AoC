open System
open System.IO

let inputList file =
    File.ReadAllLines file
    |> Array.map (fun x -> x.Split " " |> Array.toList)
    |> Array.toList

let youLose other =
    match other with
    | "A" -> 3
    | "B" -> 1
    | "C" -> 2
    | _ -> 0

let youDraw other =
    match other with
    | "A" -> 4
    | "B" -> 5
    | "C" -> 6
    | _ -> 0

let youWin other =
    match other with
    | "A" -> 8
    | "B" -> 9
    | "C" -> 7
    | _ -> 0

let getScore (strategy: List<String>) =
    match strategy with
    | a :: b :: [] when b.Equals "X" -> youLose a
    | a :: b :: [] when b.Equals "Y" -> youDraw a
    | a :: b :: [] when b.Equals "Z" -> youWin a
    | _ -> 0

let main () =
    inputList "input.txt" |> List.map getScore |> List.sum |> Console.WriteLine

main ()
