exception Found
exception Not_Found

let test s =
  let n = String.length s in
  try
    for size = 1 to n / 2 do
      try
      if n mod size = 0 then
      let pattern = String.sub s 0 size in
      for start = 1 to n / size - 1 do
        if pattern <> String.sub s (start * size) size then raise Not_Found
      done;
      raise Found
      with Not_Found -> ()
    done;
    false
  with
  | Found -> true
;;

let _ =
  Printf.printf
    "%i\n"
    (let lines = ref [] in
     try
       while true do
         let line = read_line () in
         if line = "" then raise End_of_file;
         lines := line :: !lines
       done
     with
     | End_of_file ->
       lines := List.rev !lines;
       let line = List.hd !lines in
       let a =
         List.map (fun x -> String.split_on_char '-' x) (String.split_on_char ',' line)
       in
       List.fold_left
         (fun acc x -> if test (string_of_int x) then acc + x else acc)
         0
         (List.flatten
            (List.map
               (fun l ->
                  match l with
                  | [ start; last ] ->
                    List.init
                      (int_of_string last - int_of_string start + 1)
                      (fun i -> i + int_of_string start)
                  | _ -> failwith "corrupt")
               a)))
;;
