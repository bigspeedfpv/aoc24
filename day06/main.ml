type space =
  | Empty of bool (* true if visited *)
  | Blocked

type direction =
  | North
  | South
  | East
  | West

module Guard = struct
  type t =
    { x : int
    ; y : int
    ; direction : direction
    }

  let compare a b =
    let dir_tag = function
      | North -> 0
      | South -> 1
      | East -> 2
      | West -> 3
    in
    match Stdlib.compare a.x b.x with
    | 0 ->
      (match Stdlib.compare a.y b.y with
       | 0 -> dir_tag a.direction - dir_tag b.direction
       | c -> c)
    | c -> c
  ;;
end

(* I assume this is bad practice? I'm only doing this so
   I can do Sets, otherwise Guard would be a bare record *)
open Guard

let get x y board = board.(y).(x)

let parse_board input =
  (* ew *)
  let guard = ref Guard.{ x = 0; y = 0; direction = North } in
  let board =
    input
    |> String.split_on_char '\n'
    |> Aoc.filter_empty
    |> List.mapi (fun y line ->
      line
      |> Aoc.explode
      |> List.mapi (fun x c ->
        match c with
        | '.' -> Empty false
        | '#' -> Blocked
        | '^' ->
          guard := { x; y; direction = North };
          Empty true
        | c -> failwith @@ Printf.sprintf "Invalid character %c" c)
      |> Array.of_list)
    |> Array.of_list
  in
  let width = Array.length @@ Array.get board 0 in
  let height = Array.length board in
  board, width, height, !guard
;;

let list_of_board board = board |> Array.to_list |> List.map Array.to_list

let mark_seen x y board =
  let row = Array.get board y in
  Array.set row x (Empty true)
;;

let advance board w h guard =
  let move guard =
    match guard.direction with
    | North -> { guard with y = guard.y - 1 }
    | South -> { guard with y = guard.y + 1 }
    | East -> { guard with x = guard.x + 1 }
    | West -> { guard with x = guard.x - 1 }
  in
  let turn guard =
    let direction =
      match guard.direction with
      | North -> East
      | East -> South
      | South -> West
      | West -> North
    in
    { guard with direction }
  in
  let rec find_next_pos guard =
    let next_state = move guard in
    if next_state.x < 0 || next_state.x >= w || next_state.y < 0 || next_state.y >= h
    then None
    else (
      match get next_state.x next_state.y board with
      | Blocked -> find_next_pos @@ turn guard
      | Empty _ -> Some next_state)
  in
  find_next_pos guard
;;

let rec traverse board width height guard =
  match advance board width height guard with
  | Some guard ->
    mark_seen guard.x guard.y board;
    traverse board width height guard
  | None -> board
;;

let part1 input =
  let board, width, height, guard = parse_board input in
  traverse board width height guard
  |> list_of_board
  |> List.flatten
  |> List.filter (function
    | Empty true -> true
    | _ -> false)
  |> List.length
;;

module GuardSet = Set.Make (Guard)

let part2 input =
  let board, width, height, guard = parse_board input in
  let rec test board guard seen_states =
    let has_seen guard = GuardSet.mem guard seen_states in
    match advance board width height guard with
    | Some guard when has_seen guard ->
      (* if we've been to this place before the guard will just follow the same path *)
      true
    | Some guard ->
      mark_seen guard.x guard.y board;
      test board guard (GuardSet.add guard seen_states)
    | None ->
      (* guard has exited the board *)
      false
  in
  let deep_copy = Array.(map copy) in
  let rec test_all ok = function
    | [] -> ok
    | (x, y) :: xs ->
      let blocked_board = deep_copy board in
      assert (blocked_board != board);
      blocked_board.(y).(x) <- Blocked;
      let ok =
        match test blocked_board guard GuardSet.empty with
        | true -> ok + 1
        | false -> ok
      in
      test_all ok xs
  in
  let valid_positions =
    traverse board width height guard
    |> list_of_board
    |> List.mapi (fun y row ->
      List.mapi
        (fun x -> function
          | Empty true -> Some (x, y)
          | _ -> None)
        row)
    |> List.flatten
    |> List.filter_map (fun x -> x)
    |> List.filter (fun (x, y) -> x <> guard.x || y <> guard.y)
  in
  test_all 0 valid_positions
;;

let () =
  let input = In_channel.input_all stdin in
  Printf.printf "Part 1: %d\n%!" @@ part1 input;
  (* Part 2 is pretty terribly slow as it loops through like a million and a half times.
     It runs in probably ~10s on my pc. This is fine for now *)
  Printf.printf "Part 2: %d\n%!" @@ part2 input
;;
