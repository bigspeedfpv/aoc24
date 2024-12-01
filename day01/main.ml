let part1 left right =
  let sorted_left = List.sort ( - ) left in
  let sorted_right = List.sort ( - ) right in
  List.combine sorted_left sorted_right
  |> List.map (fun (l, r) -> abs (r - l))
  |> List.fold_left (fun acc n -> acc + n) 0
;;

let () =
  let input = Aoc.read_stdin () in
  let left, right =
    input
    |> List.filter_map (fun line ->
      match line with
      | "" -> None
      | line ->
        Some (int_of_string (String.sub line 0 5), int_of_string (String.sub line 8 5)))
    |> List.split
  in
  Printf.printf "Part 1: %d\n" @@ part1 left right
;;
