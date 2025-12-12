open Stdlib
open Re

let read_file_as_array filename =
  let ic = open_in filename in
  let lines = In_channel.input_lines ic |> Array.of_list in
  close_in ic;
  lines
;;

let parse s reg =
  let r = Re.Pcre.regexp reg in
  Re.Group.all (Re.exec r s)
;;

let matches s reg : string list =
  let r = Re.Pcre.regexp reg in
  Re.matches r s
;;

let blocks lines =
  let rec aux i current acc =
    if i = Array.length lines
    then List.rev (List.rev current :: acc)
    else if lines.(i) = ""
    then aux (i + 1) [] (List.rev current :: acc)
    else aux (i + 1) (lines.(i) :: current) acc
  in
  if Array.length lines = 0 then [] else aux 0 [] []
;;

let blocks_array lines = blocks lines |> List.map Array.of_list |> Array.of_list

let transpose a =
  Array.init
    (Array.length a.(0))
    (fun i -> Array.init (Array.length a) (fun ii -> a.(ii).(i)))
;;

let string_to_array s = Array.init (String.length s) (String.get s)
let ios = int_of_string
let soi = string_of_int
let foi = float_of_int
let iof = int_of_float
let ioc x = ios (String.make 1 x)

