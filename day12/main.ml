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

type gift = char array array

type move =
  { pos : int * int
  ; shape : int
  ; state : int
  }

let stringtolist s = List.init (String.length s) (fun i -> s.[i])
let stringtoarray = Fun.compose Array.of_list stringtolist

let rot (x : gift) =
  let n = Array.length x in
  let res = Array.make_matrix n n '.' in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      res.(j).(n - 1 - i) <- x.(i).(j)
    done
  done;
  res
;;

let all_states (x : gift) =
  Array.init 8 (fun i ->
    let x = if i < 5 then x else transpose x in
    (List.init i (fun _ -> rot) |> List.fold_left Fun.compose Fun.id) x)
  |> deduplicate_array
;;

let lines = read_file_as_array "input.txt"
(* let lines = read_file_as_array "test.txt" *)
let splits = blocks lines |> Array.of_list
let shapesnum = 6

let shapes =
  Array.init shapesnum (fun i ->
    List.tl splits.(i) |> List.map stringtoarray |> Array.of_list)
  |> Array.map all_states
;;

let board_init dimx dimy = Array.init dimy (fun _ -> Array.make dimx '.')

exception Impossible

let board_check board (m : move) =
  let x0, y0 = m.pos in
  let gift = shapes.(m.shape).(m.state) in
  let h = Array.length gift in
  let w = Array.length gift.(0) in
  let dimy = Array.length board in
  let dimx = Array.length board.(0) in
  if x0 < 0 || y0 < 0 || x0 + w > dimx || y0 + h > dimy
  then false
  else (
    try
      for i = 0 to h - 1 do
        for j = 0 to w - 1 do
          if gift.(i).(j) = '#' && board.(y0 + i).(x0 + j) = '#' then raise Impossible
        done
      done;
      true
    with
    | Impossible -> false)
;;

let board_move board (m : move) =
  let x0, y0 = m.pos in
  let gift = shapes.(m.shape).(m.state) in
  let h = Array.length gift in
  let w = Array.length gift.(0) in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if gift.(i).(j) = '#' then board.(y0 + i).(x0 + j) <- '#'
    done
  done
;;

let board_undo board (m : move) =
  let x0, y0 = m.pos in
  let gift = shapes.(m.shape).(m.state) in
  let h = Array.length gift in
  let w = Array.length gift.(0) in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if gift.(i).(j) = '#' then board.(y0 + i).(x0 + j) <- '.'
    done
  done
;;

exception Found

let rec tetris (board, dimx, dimy) (nums, shapei) si =
  if shapei = shapesnum then raise Found;
  let ns = List.hd nums in
  if ns = 0
  then tetris (board, dimx, dimy) (List.tl nums, shapei + 1) 0
  else (
    let sx = si mod dimx in
    let sy = si / dimx in
    for y = sy to dimy - 3 do
      let x_start = if y = sy then sx else 0 in
      for x = x_start to dimx - 3 do
        for state = 0 to Array.length shapes.(shapei) - 1 do
          let m = { pos = x, y; shape = shapei; state } in
          if board_check board m
          then (
            board_move board m;
            (try
               tetris
                 (board, dimx, dimy)
                 ((ns - 1) :: List.tl nums, shapei)
                 (x + (y * dimx))
             with
             | Found -> raise Found);
            board_undo board m)
        done
      done
    done)
;;

let _ =
  let ins = splits.(shapesnum) in
  List.fold_left
    (fun acc x ->
       let [ dims; sn ] = String.split_on_char ':' x in
       let [ dimx; dimy ] = String.split_on_char 'x' dims in
       let dimx = ios dimx in
       let dimy = ios dimy in
       let nums = List.tl (String.split_on_char ' ' sn) |> List.map ios in
       Printf.printf "solved one\n"; flush stdout;
       acc
       +
       try
         tetris (board_init dimx dimy, dimx, dimy) (nums, 0) 0;
         0
       with
       | Found -> 1)
    0
    ins
  |> Printf.printf "%i\n"
;;

(* p2 *)
let _ = Fun.id
