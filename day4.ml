let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let check board (x, y) =
  let dimx = Array.length board.(0) in
  let dimy = Array.length board in
  let poss =
    List.filter
      (fun (j, i) -> 0 <= j && j < dimx && 0 <= i && i < dimy)
      [ x + 1, y + 1
      ; x + 1, y
      ; x, y + 1
      ; x, y - 1
      ; x - 1, y
      ; x - 1, y - 1
      ; x - 1, y + 1
      ; x + 1, y - 1
      ]
  in
  if
    List.fold_left (fun acc (j, i) -> if board.(i).(j) = '@' then 1 + acc else acc) 0 poss
    < 4
  then (board.(y).(x)<-'#'; 1)
  else 0
;;

let _ =
  let lines = ref [] in
  try
    while true do
      let line = read_line () in
      if line = "" then raise End_of_file;
      lines := line :: !lines
    done
  with
  | End_of_file ->
    lines := List.rev !lines;
    let board = Array.of_list (List.map (fun s -> Array.of_list (explode s)) !lines) in
    let acc = ref 0 in
    for k = 0 to 1000 do
    for x = 0 to Array.length board.(0) - 1 do
      for y = 0 to Array.length board - 1 do
        if board.(y).(x) = '@' then acc := !acc + check board (x, y)
      done
    done
    done;
    Printf.printf "%i" !acc
;;
