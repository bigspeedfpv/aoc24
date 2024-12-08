let filter_empty = List.filter (fun line -> String.trim line <> "")

let read_stdin () =
  stdin |> In_channel.input_all |> String.split_on_char '\n' |> filter_empty
;;

let sum = List.fold_left ( + ) 0
let explode s = List.init (String.length s) (String.get s)
let nonempty_lines input = input |> String.split_on_char '\n' |> filter_empty

let or_default default = function
  | Some v -> v
  | None -> default
;;

let in_range min max v = v >= min && v <= max
