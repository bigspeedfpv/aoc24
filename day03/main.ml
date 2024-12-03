let part1 input =
  let re = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let rec search ?(groups = []) start input =
    try
      let start = Str.search_forward re input start + 1 in
      let a = int_of_string @@ Str.matched_group 1 input in
      let b = int_of_string @@ Str.matched_group 2 input in
      search ~groups:((a, b) :: groups) start input
    with
    | Not_found -> groups
  in
  input |> search 0 |> List.map (fun (a, b) -> a * b) |> List.fold_left ( + ) 0
;;

let part2 input =
  let on = Str.regexp {|do()|} in
  let off = Str.regexp {|don't()|} in
  let re = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let rec search ?(groups = []) ?(is_on = true) start input =
    try
      if Str.string_match on input start
      then search ~groups ~is_on:true (start + 1) input
      else if Str.string_match off input start
      then search ~groups ~is_on:false (start + 1) input
      else (
        let groups =
          match is_on with
          | false -> groups
          | true ->
            if Str.string_match re input start
            then (
              let a = int_of_string @@ Str.matched_group 1 input in
              let b = int_of_string @@ Str.matched_group 2 input in
              (a, b) :: groups)
            else groups
        in
        try search ~groups ~is_on (start + 1) input with
        | Invalid_argument _ -> groups)
    with
    | Not_found -> groups
  in
  input |> search 0 |> List.map (fun (a, b) -> a * b) |> List.fold_left ( + ) 0
;;

let () =
  let input = In_channel.input_all stdin in
  Printf.printf "Part 1: %d\n" @@ part1 input;
  Printf.printf "Part 2: %d\n" @@ part2 input
;;
