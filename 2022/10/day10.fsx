open System
open System.IO

let rec visualizeLine c line =
    match line with
    | [] -> []
    | x :: xs when x = c || x = (c - 1) || x = (c + 1) -> '#' :: visualizeLine (c + 1) xs
    | _ :: xs -> '.' :: visualizeLine (c + 1) xs

let visualize list =
    List.map (fun l -> visualizeLine 0 l) list
    |> List.map List.toArray
    |> List.map String.Concat
    |> String.concat "\n"

let executeCommand (command: string) =
    match command.Split " " |> Array.toList with
    | "noop" :: [] -> (1, 0)
    | "addx" :: x :: [] -> (2, (int x))
    | _ -> failwith "invalid command"

let rec run count reg commands =
    match commands with
    | [] -> []
    | _ when count > 240 -> []
    | x :: xs ->
        let length, commandRes = executeCommand x

        run (count + length) (reg + commandRes) xs
        |> List.append (List.replicate length reg)

let main () =
    File.ReadAllLines "input.txt"
    |> Array.toList
    |> run 0 1
    |> List.chunkBySize 40
    |> visualize
    |> Console.WriteLine

main ()
