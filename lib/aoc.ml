let filter_empty = List.filter (fun line -> String.trim line <> "")

let read_stdin () =
  stdin |> In_channel.input_all |> String.split_on_char '\n' |> filter_empty
;;

let sum = List.fold_left ( + ) 0
let explode s = List.init (String.length s) (String.get s)
