let parse_input input =
  (* assuming 62 station channels (A-Za-z0-9).
     map of channel to list of locations *)
  let map = Hashtbl.create 64 in
  (* EW! mutability is gross but i'm getting lazy *)
  let width = ref 0 in
  let height = ref 0 in
  input
  |> Aoc.nonempty_lines
  |> List.iteri (fun y row ->
    height := y;
    row
    |> Aoc.explode
    |> List.iteri (fun x spc ->
      width := x;
      match spc with
      | '.' -> ()
      | c ->
        let ls = Hashtbl.find_opt map c |> Aoc.or_default [] in
        Hashtbl.replace map c ((x, y) :: ls)));
  map, !width, !height
;;

(* as with 06, only necessary for set *)
module Antinode = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with
    | 0 -> Stdlib.compare y0 y1
    | c -> c
  ;;
end

module AntinodeSet = Set.Make (Antinode)

let part1 input =
  let map, width, height = parse_input input in
  let find_antinodes positions =
    let rec find_antinodes' found = function
      | [] -> found
      | (x, y) :: xs ->
        let found =
          List.fold_left
            (fun acc (x', y') ->
              if x = x' && y = y'
              then acc
              else (
                let anx, any = x + (x - x'), y + (y - y') in
                if Aoc.in_range 0 width anx && Aoc.in_range 0 height any
                then AntinodeSet.add (anx, any) acc
                else acc))
            found
            positions
        in
        find_antinodes' found xs
    in
    find_antinodes' AntinodeSet.empty positions
  in
  map
  |> Hashtbl.to_seq
  |> Seq.fold_left
       (fun acc (_, pos) -> AntinodeSet.union acc @@ find_antinodes pos)
       AntinodeSet.empty
  |> AntinodeSet.cardinal
;;

let part2 input =
  let map, width, height = parse_input input in
  let find_antinodes positions =
    let rec generate_antinodes x y dx dy acc =
      if Aoc.in_range 0 width x && Aoc.in_range 0 height y
      then generate_antinodes (x + dx) (y + dy) dx dy (AntinodeSet.add (x, y) acc)
      else acc
    in
    let rec find_antinodes' found = function
      | [] -> found
      | (x, y) :: xs ->
        let found =
          List.fold_left
            (fun acc (x', y') ->
              if x = x' && y = y'
              then acc
              else (
                let dx, dy = x - x', y - y' in
                AntinodeSet.empty |> generate_antinodes x y dx dy |> AntinodeSet.union acc))
            found
            positions
        in
        find_antinodes' found xs
    in
    find_antinodes' AntinodeSet.empty positions
  in
  map
  |> Hashtbl.to_seq
  |> Seq.fold_left
       (fun acc (_, pos) -> AntinodeSet.union acc @@ find_antinodes pos)
       AntinodeSet.empty
  |> AntinodeSet.cardinal
;;

let () =
  let input = In_channel.input_all stdin in
  Printf.printf "Part 1: %d\n" @@ part1 input;
  Printf.printf "Part 2: %d\n" @@ part2 input
;;
