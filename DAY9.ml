

type 'a stream = Cons of 'a * ( unit -> 'a stream)
let hd (Cons (x , _ )) = x
let tl (Cons (_ , xs)) = xs ()

let rec take n s = if n = 0 then [] else hd s :: take (n-1) (tl s)
let rec zip f s1 s2 = Cons (f (hd s1) (hd s2), fun () ->  (zip f (tl s1) (tl s2)))
let rec map f s = Cons ( f (hd s) , fun () ->  (map f (tl s)))

let rec fibo = Cons ( 1,
					 fun () ->  (Cons (1, fun () ->  (zip ( + ) fibo (tl fibo)))))

type 'a lstream = LCons of 'a *  'a lstream Lazy.t
let lhd (LCons (x , _ )) = x
let ltl (LCons (_ , xs)) = Lazy.force xs

let rec ltake n s = if n = 0 then [] else lhd s :: ltake (n-1) (ltl s)
let rec lzip f s1 s2 = LCons (f (lhd s1) (lhd s2), lazy (lzip f (ltl s1) (ltl s2)))
let rec lmap f s = LCons ( f (lhd s) , lazy (lmap f (ltl s)))

let rec lfibo = LCons ( 1,
					 lazy (LCons (1, lazy (lzip ( + ) lfibo (ltl lfibo)))))

let time_it f = 
	let t0 = Sys.time () in 
	let r = f () in  (r , (Sys.time () -. t0) *. 1000. )
	
	
	let n  = 33

let _ = let (_ , timings) = time_it @@ (fun () -> ltake n lfibo) in
    print_endline @@ string_of_float timings

let _ = let (_ , timings) = time_it @@ (fun () -> take n fibo) in
    print_endline @@ string_of_float timings

    



