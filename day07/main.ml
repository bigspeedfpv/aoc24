type op =
  | Add of int
  | Multiply of int
  | Concat of int

let parse_input input =
  input
  |> String.split_on_char '\n'
  |> Aoc.filter_empty
  |> List.map (fun line ->
    match String.split_on_char ':' line with
    | [ expected; steps ] ->
      let expected = int_of_string expected in
      let steps =
        steps |> String.trim |> String.split_on_char ' ' |> List.map int_of_string
      in
      expected, steps
    | _ -> failwith @@ Printf.sprintf "Invalid line %s" line)
;;

let execute =
  let rec execute' acc = function
    | [] -> acc
    | x :: xs ->
      (match x with
       | Add n -> execute' (acc + n) xs
       | Multiply n -> execute' (acc * n) xs
       | Concat n -> execute' (int_of_string @@ string_of_int acc ^ string_of_int n) xs)
  in
  execute' 0
;;

let run_solver input solver =
  parse_input input
  |> List.fold_left
       (fun acc (target, nums) ->
         match solver target (List.rev nums) [] with
         | true -> acc + target
         | false -> acc)
       0
;;

let part1 input =
  (* this is not tail-recursive but it runs in small enough space complexity
     that stack size should never be a problem (famous last words?)
     nums should be reversed as this traverses forwards then executes backwards *)
  let rec find_solution target nums steps =
    match nums with
    | [] -> execute steps = target
    | x :: xs ->
      find_solution target xs (Multiply x :: steps)
      || find_solution target xs (Add x :: steps)
  in
  run_solver input find_solution
;;

let part2 input =
  (* same note as above :3 *)
  let rec find_solution target nums steps =
    match nums with
    | [] -> execute steps = target
    | x :: xs ->
      find_solution target xs (Multiply x :: steps)
      || find_solution target xs (Add x :: steps)
      || find_solution target xs (Concat x :: steps)
  in
  run_solver input find_solution
;;

let () =
  let input = In_channel.input_all stdin in
  Printf.printf "Part 1: %d\n%!" @@ part1 input;
  Printf.printf "Part 2: %d\n%!" @@ part2 input
;;
