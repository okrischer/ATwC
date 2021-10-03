let sf_hash = Hashtbl.create 100000;;
Hashtbl.add sf_hash [-1] (Array.make 6 0)
let found = ref false

let code sf =
  (Array.fold_right (fun x y -> x + y) sf 0) mod 100000

let shiftR (sf1: int array) sf2 j =
  let equals = ref true in
  for i = 0 to 5 do
    equals := sf1.(i) = sf2.((i+j) mod 6);
  done;
  !equals

let shiftL (sf1: int array) sf2 j =
  let equals = ref true in
  for i = 0 to 5 do
    equals := sf1.(i) = sf2.((j-i+6) mod 6);
  done;
  !equals

let rec ident (sf1: int array) (sf2: int array) indices = match indices with
  | [] -> false
  | j::js -> shiftR sf1 sf2 j || shiftL sf1 sf2 j || ident sf1 sf2 js 

let are_ident sf1 sf2 =
  let indices = ref [] in
    for i = 0 to 5 do
      if sf1.(0) = sf2.(i)
        then indices := i::!indices
    done;
    ident sf1 sf2 !indices

let rec check_matches matches sf = match matches with
  | [] -> false
  | m::ms -> are_ident m sf || check_matches ms sf

let find_ident hash sf =
  let matches = Hashtbl.find_all sf_hash [hash] in
  check_matches matches sf

let main () =
  let n = read_int () in
  for i = 1 to n do
    let sf = Array.make 6 0 in
    let line = read_line () in
    Scanf.sscanf line "%d %d %d %d %d %d" (fun a b c d e f -> 
      sf.(0) <- a; sf.(1) <- b; sf.(2) <- c;
      sf.(3) <- d; sf.(4) <- e; sf.(5) <- f);
    let hash = code sf in
    if Hashtbl.mem sf_hash [hash]
      then found := find_ident hash sf;
    Hashtbl.add sf_hash [hash] sf;
    if !found then print_endline "Twin snowflakes found.";
    if !found then exit 0
  done;
  print_endline "No two snowflakes are alike.";;

main ()