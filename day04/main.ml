let get_char input width x y =
  try Some (Array.get input (((width + 1) * y) + x)) with
  | Invalid_argument _ -> None
;;

let part1 input width =
  let directions = [ 1, 0; 1, 1; 0, 1; -1, 1; -1, 0; -1, -1; 0, -1; 1, -1 ] in
  let check_direction x y offset_x offset_y =
    let rec check_direction' x y found left =
      match left with
      | 0 -> found = "XMAS"
      | _ ->
        (match get_char input width x y with
         | None -> false
         | Some c ->
           check_direction'
             (x + offset_x)
             (y + offset_y)
             (found ^ String.make 1 c)
             (left - 1))
    in
    check_direction' x y "" 4
  in
  let height = Array.length input / width in
  let id i = i in
  List.init height id
  |> List.map (fun y -> List.init width id |> List.map (fun x -> x, y))
  |> List.flatten
  |> List.map (fun (x, y) ->
    directions
    |> List.filter (fun (offset_x, offset_y) -> check_direction x y offset_x offset_y)
    |> List.length)
  |> Aoc.sum
;;

let part2 input width =
  let directions = [ 1, 1; -1, 1 ] in
  let check_pos x y offset_x offset_y =
    match get_char input width x y with
    | Some 'A' ->
      (match
         ( get_char input width (x + offset_x) (y + offset_y)
         , get_char input width (x - offset_x) (y - offset_y) )
       with
       | Some 'M', Some 'S' | Some 'S', Some 'M' -> true
       | _ -> false)
    | _ -> false
  in
  let height = Array.length input / width in
  let id i = i in
  List.init height id
  |> List.map (fun y -> List.init width id |> List.map (fun x -> x, y))
  |> List.flatten
  |> List.filter (fun (x, y) ->
    directions
    |> List.filter (fun (offset_x, offset_y) -> check_pos x y offset_x offset_y)
    |> List.length
    = 2)
  |> List.length
;;

let () =
  (* convert input to a char array array, rather than a char list list, for (hopefully) faster indexing *)
  let input = In_channel.input_all stdin |> String.to_seq in
  let width = input |> Seq.take_while (fun c -> c != '\n') |> Seq.length in
  let input = Array.of_seq input in
  Printf.printf "Part 1: %d\n" @@ part1 input width;
  Printf.printf "Part 2: %d\n" @@ part2 input width
;;
