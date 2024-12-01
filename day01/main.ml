let part1 left right =
  let sorted_left = List.sort ( - ) left in
  let sorted_right = List.sort ( - ) right in
  List.combine sorted_left sorted_right
  |> List.map (fun (l, r) -> abs (r - l))
  |> List.fold_left (fun acc n -> acc + n) 0
;;

let part2 left right =
  let count_occurrences num =
    let rec count_occurrences' times = function
      | [] -> times
      | x :: xs ->
        let times = if x = num then times + 1 else times in
        count_occurrences' times xs
    in
    count_occurrences' 0 right
  in
  left
  |> List.map (fun n ->
    let occurrences = count_occurrences n in
    n * occurrences)
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
  Printf.printf "Part 1: %d\n" @@ part1 left right;
  Printf.printf "Part 12: %d\n" @@ part2 left right
;;
