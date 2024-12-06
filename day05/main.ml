type page_state =
  | NotFound
  | Found of int

let parse_rules rules =
  rules
  |> String.split_on_char '\n'
  |> List.map (fun line ->
    match String.split_on_char '|' line with
    | [ first; second ] -> int_of_string first, int_of_string second
    | _ -> failwith @@ Printf.sprintf "invalid rule %s" line)
;;

let part1 input =
  let parts = Str.split (Str.regexp "\n\n") input in
  let rules = parse_rules @@ List.nth parts 0 in
  let test_update update =
    let rec test_rule idx state rule = function
      | [] ->
        (match state with
         | Found a, Found b -> a < b
         | _ -> true)
      | x :: xs ->
        (match x with
         | x when x = fst rule -> test_rule (idx + 1) (Found idx, snd state) rule xs
         | x when x = snd rule -> test_rule (idx + 1) (fst state, Found idx) rule xs
         | _ -> test_rule (idx + 1) state rule xs)
    in
    List.map (fun rule -> test_rule 0 (NotFound, NotFound) rule update) rules
    |> List.for_all (fun a -> a)
  in
  List.nth parts 1
  |> String.split_on_char '\n'
  |> Aoc.filter_empty
  |> List.map (fun update -> update |> String.split_on_char ',' |> List.map int_of_string)
  |> List.filter test_update
  |> List.map (fun update -> List.nth update @@ (List.length update / 2))
  |> Aoc.sum
;;

let () =
  let input = In_channel.input_all stdin in
  Printf.printf "Part 1: %d\n" @@ part1 input
;;
(*Printf.printf "Part 2: %d\n" @@ part2 reports*)
