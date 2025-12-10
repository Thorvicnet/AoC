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

let bintoint (a : bool array) =
  let res = ref 0 in
  Array.iteri (fun i x -> if x then res := !res + (1 lsl i)) a;
  !res
;;

exception Found of int

let bfs (goal : int) (ops : int array) (lnum : int) =
  if goal = 0
  then 0
  else (
    let visited = Array.make (1 lsl lnum) false in
    visited.(0) <- true;
    let q = Queue.create () in
    Queue.add (0, 0) q;
    try
      while not (Queue.is_empty q) do
        let c, i = Queue.take q in
        Array.iter
          (fun x ->
             let next = c lxor x in
             if next = goal
             then raise (Found (i + 1))
             else if not visited.(next)
             then (
               Queue.add (next, i + 1) q;
               visited.(next) <- true))
          ops
      done;
      raise Not_found
    with
    | Found x -> x)
;;

let _ =
  let lines = read_file_as_array "input.txt" in
  Array.fold_left
    (fun acc line ->
       let tmp = parse line "\\[.+\\]" in
       let n = String.length tmp.(0) - 2 in
       let lights = bintoint (Array.init n (fun i -> tmp.(0).[i + 1] = '#')) in
       let tmp = matches line "\\([^\\(]+\\)" |> Array.of_list in
       let ops =
         Array.init (Array.length tmp) (fun i ->
           let nums = matches tmp.(i) "\\d+" |> Array.of_list in
           Array.fold_left (fun acc x -> acc + (1 lsl ios x)) 0 nums)
       in
       acc + bfs lights ops n)
    0
    lines
  |> Printf.printf "p1: %i\n"
;;

(* p2 *)

(* I tried doing p2 with a bfs, hashtbl and array, a bfs, hashtbl and list but did not work, so scipy came to the rescue *)
