let count src dest =
  let acc = ref 0 in
  for i = src to dest do
    if i mod 100 = 0 then acc := !acc + 1
  done;
  !acc
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
    let acc = ref 0 in
    let p = ref 50 in
    List.iter
      (fun a ->
         let c, n = Scanf.sscanf a "%c%d" (fun c n -> c, n) in
         let next = if c = 'L' then !p - n else !p + n in
         Printf.printf "next : %i, !p : %i, !acc : %i %s\n" next !p !acc a;
         acc := !acc + if next > !p then count (!p + 1) next else count next (!p - 1);
         p := next)
      !lines;
    Printf.printf "passwd: %i\n" !acc
;;
