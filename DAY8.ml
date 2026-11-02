type 'a stream = Cons of 'a * ( unit -> 'a stream)

let rec zeroes = Cons (0 , fun () -> zeroes)

let hd (Cons (x , _ )) = x
let tl (Cons (_ , xs )) = xs ()

let rec take n s = if n = 0 then [] else hd s :: take (n-1) (tl s)
let rec filter p s  = if (p (hd s)) then Cons ((hd s ), fun () ->  filter p (tl s)) else filter p (tl s)

let rec map f xs = Cons (f (hd xs) , fun () -> map f (tl xs))

let rec from n = Cons (n , fun () -> from (n+1)) 

let rec sieve s =
    let fx = hd s
    in Cons (fx , fun () -> sieve (filter (fun x -> (x mod fx <> 0)) (tl s) ))
    
    
;;

