open Stdlib
open Re

let read_file_as_array filename =
  let ic = open_in filename in
  let lines = In_channel.input_lines ic |> Array.of_list in
  close_in ic;
  lines

let parse s reg =
  let r = Re.Pcre.regexp reg in
  Re.Group.all (Re.exec r s)

let ios = int_of_string
let soi = string_of_int
let foi = float_of_int
let iof = int_of_float

let _ =
  let lines = read_file_as_array "input.txt" in
