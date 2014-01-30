(* Списки с произвольным доступом *)
type 'a seq = Nil | Zero of ('a * 'a) seq | One of 'a * ('a * 'a) seq

let rec cons : 'a . 'a -> 'a seq -> 'a seq =
  fun x s -> match s with
  | Nil -> One(x, Nil)
  | Zero ps -> One(x, ps)
  | One (y, ys) -> Zero(cons (x, y) ys);;

let rec uncons : 'a . 'a seq -> 'a * 'a seq = function
  | One(x, Nil) -> x, Nil
  | One(x, xs) -> x, Zero xs
  | Zero xs -> let (x, y), xs' = uncons xs in x, One(y, xs')
  | Nil -> failwith "empty"

let head s = let x, _ = uncons s in x

let tail s = let _, xs = uncons s in xs

(* lookup 4 (One(1, One((2, 3), One(((4, 5), (6, 7)), Nil)))) *)
let rec lookup : 'a. int -> 'a seq -> 'a =
  fun idx s -> match idx, s with 
  | _, Nil -> failwith "subscript"
  | 0, One (x, xs) -> x
  | i, One (x, xs) -> lookup (i - 1) (Zero xs)
  | i, Zero xs -> let x, y = lookup (i / 2) xs in
        if i mod 2 == 0 then x else y
