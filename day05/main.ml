type page_state =
  | NotFound
  | Found of int

let parse_input input =
  let parse_rules rules =
    rules
    |> String.split_on_char '\n'
    |> List.map (fun line ->
      match String.split_on_char '|' line with
      | [ first; second ] -> int_of_string first, int_of_string second
      | _ -> failwith @@ Printf.sprintf "invalid rule %s" line)
  in
  let parse_updates updates =
    updates
    |> String.split_on_char '\n'
    |> Aoc.filter_empty
    |> List.map (fun update ->
      update |> String.split_on_char ',' |> List.map int_of_string)
  in
  match Str.split (Str.regexp "\n\n") input with
  | [ rules; updates ] -> parse_rules rules, parse_updates updates
  | _ -> failwith "invalid input"
;;

let check_update rules update =
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
;;

let part1 input =
  let rules, updates = parse_input input in
  updates
  |> List.filter (check_update rules)
  |> List.map (fun update -> List.nth update @@ (List.length update / 2))
  |> Aoc.sum
;;

let part2 input =
  let rules, updates = parse_input input in
  let fix_update update =
    let rule a b el = (a, b) = (fst el, snd el) in
    update
    |> List.sort (fun a b ->
      (* This is pretty horribly inefficient because it loops through
         the entire list up to twice per item, but it's fast enough
         (both parts ~165ms total on my machine) so I'll accept it for now *)
      if List.exists (rule a b) rules
      then 1
      else if List.exists (rule b a) rules
      then -1
      else 0)
  in
  updates
  |> List.filter (fun u -> check_update rules u = false) (* only get invalid updates *)
  |> List.map fix_update
  |> List.map (fun u -> List.nth u @@ (List.length u / 2))
  |> Aoc.sum
;;

let () =
  let input = In_channel.input_all stdin in
  Printf.printf "Part 1: %d\n" @@ part1 input;
  Printf.printf "Part 2: %d\n" @@ part2 input
;;
