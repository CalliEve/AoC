open System
open System.IO

exception Oops of string

let rec splitGroups list =
    match list with
    | _ :: b :: _ :: ' ' :: xs -> b :: splitGroups xs
    | _ :: b :: _ :: xs -> b :: splitGroups xs
    | _ -> []

let rec intoStacks stacks input =
    match input with
    | [] -> stacks
    | ' ' :: xs -> List.head stacks :: intoStacks (List.tail stacks) xs
    | x :: xs -> (x :: List.head stacks) :: intoStacks (List.tail stacks) xs

let rec splitInput (input: List<string>) (acc: List<string>) =
    match input with
    | [] -> []
    | "" :: xs -> [ acc; xs ]
    | x :: xs -> splitInput xs (x :: acc)

let parseInstructions (input: string) =
    match input.Split " " |> Array.toList with
    | _ :: amount :: _ :: from :: _ :: towards :: [] -> [ int amount; int from - 1; int towards - 1 ]
    | _ -> raise (Oops("invalid instructions"))

let rec moveCrates (stacks: List<char>[]) (instructions: List<int>) =
    match instructions with
    | [ amount; from; towards ] ->
        let inMove = stacks[from][0 .. amount - 1]
        Array.set stacks from (stacks[from][amount..])
        Array.set stacks towards (List.append inMove stacks[towards])
        stacks
    | _ -> raise (Oops("invalid crate move"))

let run (input: List<string>) : List<char>[] =
    match splitInput input [] with
    | ss :: is :: [] ->
        let inputStacks = List.map (fun l -> splitGroups (Seq.toList l)) ss |> List.tail
        let instructions = List.map parseInstructions is

        let stacks =
            inputStacks
            |> List.fold
                intoStacks
                [ for _ in 1 .. (List.head inputStacks |> List.length) do
                      yield [] ]
            |> List.toArray

        List.fold moveCrates stacks instructions
    | _ -> raise (Oops("invalid input"))

let main () =
    File.ReadAllLines "input.txt"
    |> Array.toList
    |> run
    |> Array.map List.head
    |> String.Concat
    |> Console.WriteLine

main ()
