open Stdlib
open Re
open Prelude

(* parse *)

let lines = read_file_as_array "input.txt"
let hti = Hashtbl.create 242
let n = Array.length lines

let _ =
  Array.iteri
    (fun i x ->
       let st = String.split_on_char ':' x in
       Hashtbl.add hti (List.hd st) i)
    lines;
  Hashtbl.add hti "out" n
;;

let adj =
  Array.init (Array.length lines) (fun i ->
    String.split_on_char ':' lines.(i)
    |> List.tl
    |> List.hd
    |> String.split_on_char ' '
    |> List.tl
    |> List.map (Hashtbl.find hti))
;;

(* p1 *)

let _ =
  let ht = Hashtbl.create 242 in
  Hashtbl.add ht n 1;
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
  dfs you out |> Printf.printf "p1: %d\n"
;;

(* p2 *)

let _ =
  let ht = Hashtbl.create 242 in
  Hashtbl.add ht n 1;
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
