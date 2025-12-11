(* prelude *)

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

(* p1 *)

let _ =
  let lines = read_file_as_array "input.txt" in
  let hti = Hashtbl.create 242 in
  let ht = Hashtbl.create 242 in
  Array.iteri
    (fun i x ->
       let st = String.split_on_char ':' x in
       Hashtbl.add hti (List.hd st) i)
    lines;
  let n = Array.length lines in
  Hashtbl.add hti "out" n;
  Hashtbl.add ht n 1;
  let adj =
    Array.init (Array.length lines) (fun i ->
      String.split_on_char ':' lines.(i)
      |> List.tl
      |> List.hd
      |> String.split_on_char ' '
      |> List.tl
      |> List.map (Hashtbl.find hti))
  in
  let rec dfs ci gi =
    if ci = gi
    then 1
    else (
      match Hashtbl.find_opt ht ci with
      | Some a -> a
      | None ->
        let count = List.fold_left (fun acc x -> acc + dfs x gi) 0 adj.(ci) in
        Hashtbl.add ht ci count;
        count)
  in
  let you = Hashtbl.find hti "you" in
  let out = n in
  dfs you out |>
  Printf.printf "p1: %d\n"

(* p2 *)

let _ =
  let lines = read_file_as_array "input.txt" in
  let hti = Hashtbl.create 242 in
  let ht = Hashtbl.create 242 in
  Array.iteri
    (fun i x ->
       let st = String.split_on_char ':' x in
       Hashtbl.add hti (List.hd st) i)
    lines;
  let n = Array.length lines in
  Hashtbl.add hti "out" n;
  Hashtbl.add ht n 1;
  let adj =
    Array.init (Array.length lines) (fun i ->
      String.split_on_char ':' lines.(i)
      |> List.tl
      |> List.hd
      |> String.split_on_char ' '
      |> List.tl
      |> List.map (Hashtbl.find hti))
  in
  let rec dfs ci gi =
    if ci = gi
    then 1
    else if ci = n
    then 0
    else (
      match Hashtbl.find_opt ht ci with
      | Some a -> a
      | None ->
        let count = List.fold_left (fun acc x -> acc + dfs x gi) 0 adj.(ci) in
        Hashtbl.add ht ci count;
        count)
  in
  let svr = Hashtbl.find hti "svr" in
  let dac = Hashtbl.find hti "dac" in
  let fft = Hashtbl.find hti "fft" in
  let out = n in
  let r1 = dfs dac out in
  Hashtbl.clear ht;
  let r2 = dfs fft dac in
  Hashtbl.clear ht;
  let r3 = dfs svr fft in
  Printf.printf "p2: %d\n" (r1 * r2 * r3)
;;

