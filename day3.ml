let ioc x = int_of_string (String.make 1 x)

let ipow x e = int_of_float (Float.pow (float_of_int x) (float_of_int e))

let maxf st en s =
  let max = ref (0, -1) in
  for i = st to String.length s - en - 1 do
    let c = s.[i] in
    if ioc c > fst !max then max := ioc c, i
  done;
  !max
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
    let ans = ref 0 in
    List.iter
      (fun s ->
        let st = ref 0 in
        for i = 1 to 12 do
          let a,b = maxf !st (12-i) s in
          st := b + 1;
          ans := !ans + a * ipow 10 (12-i)
        done)
      !lines;
    Printf.printf "%d" !ans
;;
