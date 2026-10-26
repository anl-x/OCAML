(* 
	Questions : 
	 1. https://fplaunchpad.org/ocaml_nptel/M06-L07-practice.html#problem-13-runlengthencode
	 2. https://fplaunchpad.org/ocaml_nptel/M06-L07-practice.html#problem-14-runningsum
 *)

let compose_all fs =
  List.fold_left
    (fun acc f -> fun x -> f (acc x))
    (fun x -> x)
    fs
;;

compose_all [ (fun x -> x + 1) ] 10
;;

let run_length_encode xs =
  List.fold_right
    (fun x acc ->
      match acc with
      | (entry, freq) :: rest when entry = x ->
          (entry, freq + 1) :: rest
      | _ -> (x, 1) :: acc)
    xs []
;;

run_length_encode [ 1; 2; 1 ]
