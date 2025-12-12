open Stdlib
open Re
open Prelude

let deduplicate a =
  let ht = Hashtbl.create 42 in
  let res = ref [] in
  List.iter
    (fun x ->
       if Hashtbl.mem ht x
       then ()
       else (
         Hashtbl.add ht x ();
         res := x :: !res))
    a;
  !res
;;

let deduplicate_array a =
  let ht = Hashtbl.create 42 in
  let res = ref [] in
  Array.iter
    (fun x ->
       if Hashtbl.mem ht x
       then ()
       else (
         Hashtbl.add ht x ();
         res := x :: !res))
    a;
  !res |> Array.of_list
;;

(* p1 *)

type undo =
  { pos : int
  ; shape : int
  ; state : int
  }

let stringtobin s =
  let n = String.length s in
  let res = Array.make n false in
  for i = 0 to n - 1 do
    if s.[i] = '#' then res.(i) <- true
  done;
  res
;;

let rot (x : bool array array) =
  let n = Array.length x in
  let res = Array.make_matrix n n false in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      res.(i).(j) <- res.(j).(n - 1 - i)
    done
  done;
  res
;;

let allstates (x : bool array array) =
  Array.init 8 (fun i ->
    let x = if i < 5 then x else transpose x in
    (List.init i (fun _ -> rot) |> List.fold_left Fun.compose Fun.id) x)
  |> deduplicate_array
;;

let _ =
  let lines = read_file_as_array "input.txt" in
  let splits = blocks lines |> Array.of_list in
  let shapesnum = 5 in
  (* let shapes = *)
  (*   Array.init shapesnum (fun i -> *)
  (*     List.tl splits.(i) |> List.map stringtobin |> Array.of_list) *)
  (*   |> Array.map allstates *)
  (* in  *)
  (* () *)
  let shapessize =
    Array.init (shapesnum + 1) (fun i ->
      List.tl splits.(i)
      |> List.fold_left
           (fun acc x ->
              acc
              + String.fold_left (fun acc2 x2 -> if x2 = '#' then acc2 + 1 else acc2) 0 x)
           0)
  in
    Array.iter (Printf.printf "%i\n") shapessize;
  let ins = splits.(shapesnum + 1) in
  List.fold_left
    (fun acc x ->
       let [ dims; sn ] = String.split_on_char ':' x in
       let [ dimx; dimy ] = String.split_on_char 'x' dims in
       let nums = List.tl (String.split_on_char ' ' sn) in
       let nn =
         List.mapi (fun i x -> ios x * shapessize.(i)) nums |> List.fold_left ( + ) 0
       in
       if ios dimx * ios dimy > nn then acc + 1 else acc)
    0
    ins |> Printf.printf "%i\n"
;;

(* p2 *)
