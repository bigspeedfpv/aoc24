let read_stdin () =
  stdin
  |> In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (function
    | "" -> false
    | _ -> true)
;;
