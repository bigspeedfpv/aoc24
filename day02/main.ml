type difference =
  { increasing : bool
  ; difference : int
  }

let rec find_distances prev ds = function
  | [] -> ds
  | x :: xs ->
    let d = { increasing = x > prev; difference = abs (x - prev) } in
    find_distances x (d :: ds) xs
;;

let check_report report =
  let rec differences_ok ?target = function
    | [] -> true
    | x :: xs ->
      if x.difference >= 1 && x.difference <= 3
      then (
        match target with
        | Some target ->
          if x.increasing = target then differences_ok ~target xs else false
        | None -> differences_ok ~target:x.increasing xs)
      else false
  in
  let differences =
    match report with
    | [] | [ _ ] -> failwith "report empty"
    | x :: xs -> find_distances x [] xs
  in
  match differences_ok differences with
  | true -> 1
  | false -> 0
;;

let part1 reports = reports |> List.map check_report |> Aoc.sum

(* TODO: maybe make this not bad lol *)
let part2 reports =
  let rec check_report' ?(idx = 0) report =
    if idx = List.length report
    then 0
    else (
      let spliced = List.filteri (fun i _ -> i != idx) report in
      match check_report spliced with
      | 1 -> 1
      | _ -> check_report' ~idx:(idx + 1) report)
  in
  reports |> List.map check_report' |> Aoc.sum
;;

let () =
  let input = Aoc.read_stdin () in
  let reports =
    input |> List.map (fun l -> l |> String.split_on_char ' ' |> List.map int_of_string)
  in
  Printf.printf "Part 1: %d\n" @@ part1 reports;
  Printf.printf "Part 2: %d\n" @@ part2 reports
;;
