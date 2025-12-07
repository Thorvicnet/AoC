open Stdlib
open Re

let parse s reg =
  let r = Re.Pcre.regexp reg in
  Re.Group.all (Re.exec r s)
;;

let read_file_as_array filename =
  let ic = open_in filename in
  let lines = In_channel.input_lines ic |> Array.of_list in
  close_in ic;
  lines
;;

let ios = int_of_string
let soi = string_of_int
let foi = float_of_int
let iof = int_of_float

(* p1 *)

(* lost it, and as it used Hashtbl it is not a great loss *)

(* p2 *)

exception Split of (int * int) * (int * int)

let _ =
  let lines = read_file_as_array "input.txt" in
  let cline = ref 0 in
  let ranges = ref [] in
  while lines.(!cline) <> "" do
    let res = parse lines.(!cline) "(\\d+)-(\\d+)" in
    let rec find_ranges (r : int * int) : (int * int) list =
      try
        [ List.fold_left
            (fun (a, b) (c, d) ->
               if b < c
               then a, b
               else if a > d
               then a, b
               else if a >= c && b <= d
               then -1, -1
               else if a < c && b <= d
               then a, c - 1
               else if b > d && a >= c
               then d + 1, b
               else raise (Split ((a, c - 1), (d + 1, b))))
            r
            !ranges
        ]
      with
      | Split (r1, r2) -> find_ranges r1 @ find_ranges r2
    in
    List.iter
      (fun x -> if x = (-1, -1) then () else ranges := x :: !ranges)
      (find_ranges (ios res.(1), ios res.(2)));
    incr cline
  done;
  let count = List.fold_left (fun acc (a, b) -> acc + b - a + 1) 0 !ranges in
  Printf.printf "p2: %i\n" count
;;
