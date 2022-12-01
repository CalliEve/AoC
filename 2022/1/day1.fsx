open System
open System.IO

let rec intLists (acc: List<List<int>>) item =
    match item with
    | "" -> ([] :: acc)
    | x -> (((int x) :: (List.head acc)) :: (List.tail acc))

let inputList file =
    File.ReadAllLines file |> Array.toList |> List.fold intLists [ [] ]

let sumAll (input: List<List<int>>) = input |> List.map List.sum

let main () =
    inputList "input.txt"
    |> sumAll
    |> List.sort
    |> List.rev
    |> List.take 3
    |> List.sum
    |> Console.WriteLine

main ()
