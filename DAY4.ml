let make_running_total () =
  let sum = ref 0 in 
  (fun (n : int ) -> sum := !sum + n ; !sum)
  
;; 

let running_total = make_running_total ()

;;

running_total(1)

;;
running_total(1)
;;
let c = running_total(1)
;;

print_endline (string_of_int @@ c)


