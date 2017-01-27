let range i j =
  let rec aux n acc =
    if n < i then acc else aux (n - 1) (n :: acc)
  in aux j []

let rec fsum l =
  match l with
  | [] -> 0.0
  | h :: t -> h +. (fsum t)

let numer i =
  if mod_float (float_of_int i) 2.0 = 0.0
    then 4.0
    else -4.0

let denom i =
  1.0 +. (float_of_int i) *. 2.0

let fround x = floor (x +. 0.5)

let pi = 4.0 *. atan 1.0

let estimate_pi iterations =
  range 0 iterations |> List.map (fun i -> (numer i) /. (denom i)) |> fsum

let main () =
  let iterations = 1 lsl 17 in
  let estimation = estimate_pi iterations in
    print_string "Pi: ";
    print_float pi;
    print_endline "";
    print_string "Estimation after ";
    print_int iterations;
    print_string " iterations: ";
    print_float estimation;
    print_endline "";
    print_string
      (if (fround (estimation *. 10000.0) = fround (pi *. 10000.0))
        then "Correct"
        else "Incorrect");
    print_endline ""

let _ = main ()
