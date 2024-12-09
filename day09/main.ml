type tape_item =
  | Data of int
  | Empty

let show_tape tape =
  tape
  |> Array.to_list
  |> List.map (function
    | Empty -> "(empty)"
    | Data a -> Printf.sprintf "(data %d)" a)
  |> String.concat " "
;;

let parse_input input =
  let rec add_data ~width data tape =
    match width with
    | 0 -> tape
    | w -> add_data ~width:(w - 1) data (data :: tape)
  in
  let rec parse_input is_space file_id tape = function
    | [] -> List.rev tape
    | x :: xs ->
      let width = int_of_char x - int_of_char '0' in
      let data, file_id =
        match is_space with
        | true -> Empty, file_id
        | false -> Data file_id, file_id + 1
      in
      let tape = add_data ~width data tape in
      parse_input (not is_space) file_id tape xs
  in
  let tape =
    input |> String.trim |> Aoc.explode |> parse_input false 0 [] |> Array.of_list
  in
  print_endline @@ show_tape tape;
  tape
;;

let part1 input =
  let tape = parse_input input in
  let rec advance_empty ptr =
    match tape.(ptr) with
    | Empty -> ptr
    | _ -> advance_empty @@ (ptr + 1)
  in
  let rec advance_data ptr =
    match tape.(ptr) with
    | Data _ -> ptr
    | _ -> advance_data @@ (ptr - 1)
  in
  let rec rearrange tape empty_ptr data_ptr =
    Printf.printf "empty_ptr %d data_ptr %d\n%!" empty_ptr data_ptr;
    if empty_ptr >= data_ptr
    then ()
    else (
      tape.(empty_ptr) <- tape.(data_ptr);
      tape.(data_ptr) <- Empty;
      rearrange tape (advance_empty empty_ptr) (advance_data data_ptr))
  in
  let rec sum_values idx acc tape =
    Printf.printf "idx %d acc %d " idx acc;
    match tape with
    | [] ->
      print_endline "done";
      acc
    | Empty :: xs ->
      print_endline "empty";
      sum_values (idx + 1) acc xs
    | Data n :: xs ->
      Printf.printf "data %d\n%!" n;
      sum_values (idx + 1) (acc + (n * idx)) xs
  in
  rearrange tape (advance_empty 0) (advance_data (Array.length tape - 1));
  sum_values 0 0 @@ Array.to_list tape
;;

let part2 _input = 0

let () =
  let input = In_channel.input_all stdin in
  Printf.printf "Part 1: %d\n" @@ part1 input;
  Printf.printf "Part 2: %d\n" @@ part2 input
;;
