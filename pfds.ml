(* set interface *)
module type Set = sig
  type t
  type set
  val empty : set
  val member : t -> set -> bool
  val insert : t -> set -> set
end;;

(* heap interface *)
module type Heap = sig
  type t
  type heap
  val empty : heap
  val isEmpty : heap -> bool
  val insert : t -> heap -> heap
  val merge : heap -> heap -> heap
  val findMin : heap -> t
  val deleteMin : heap -> heap
end;;

(* ordered interface *)
module type Ordered = sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val leq : t -> t -> bool
end;;

(* queue interface *)
module type Queue = sig
  type 'a queue
  val empty : 'a queue
  val isEmpty : 'a queue -> bool
  val append : 'a queue -> 'a -> 'a queue
  val head : 'a queue -> 'a
  val tail : 'a queue -> 'a queue
end;;

(* unbalanced set functor *)
module UnbalancedSet(Element : Ordered) : (Set with type t := Element.t) = struct
  type tree = E | T of tree * Element.t * tree
  type set = tree

  let empty = E

  let rec member n bst = 
    match bst with
      | E -> false
      | T (left, x, right) ->
          if Element.lt n x then
            member n left
          else if Element.gt n x then
            member n right
          else true

  let rec insert n bst = 
    match bst with
      | E -> T (E, n, E)
      | T (left, x, right) ->
          if Element.lt n x then
            T (insert n left, x, right)
          else if Element.gt n x then
            T (left, x, insert n right)
          else T(left, x, right)
end;;

(* red-black trees *)
module RedBlackSet(Element : Ordered) : (Set with type t := Element.t) = struct
  type color = R | B
  type tree = E | T of color * tree * Element.t * tree
  type set = tree

  let empty = E

  let rec member n bst = 
    match bst with
      | E -> false
      | T (_, left, x, right) ->
          if Element.lt n x then
            member n left
          else if Element.gt n x then
            member n right
          else true

  let balance col lt el rt = 
    match (col, lt, el, rt) with
      | B, T (R, T (R, a, x, b), y, c), z, d
      | B, T (R, a, x, T (R, b, y, c)), z, d
      | B, a, x, T (R, T (R, b, y, c), z, d)
      | B, a, x, T (R, b, y, T (R, c, z, d)) ->
          T (R, T (B, a, x, b), y, T (B, c, z, d))
      | _ -> T (col, lt, el, rt)

  let insert x s = 
    let rec ins = function
      | E -> T (R, E, x, E)
      | T (color, a, y, b) ->
          if Element.lt x y then
            balance color (ins a) y b
          else if Element.lt y x then
            balance color a y (ins b)
          else s in
    let t = ins s in
      match t with
        | E -> E
        | T (_, a, y, b) ->
            T (B, a, y, b)
end;;

(* leftist heaps *)
module LeftistHeap(Element : Ordered) = struct
  type tree = E | T of int * Element.t * tree * tree
  type heap = tree

  let empty = E

  let isEmpty = function
    | E -> true
    | _ -> false

  let rank = function
    | E -> 0
    | T (r, _, _, _) -> r

  let makeT x a b = 
    if rank a >= rank b then
      T (rank b + 1, x, a, b)
    else
      T (rank a + 1, x, b, a)

  let rec merge h1 h2 = 
    match (h1, h2) with
      | E, h -> h
      | h, E -> h
      | T (_, x, a1, b1), T (_, y, a2, b2) ->
          if Element.lt x y then
            makeT x a1 (merge b1 h2)
          else
            makeT y a2 (merge h1 b2)

  let insert x h = merge (T (1, x, E, E)) h

  let findMin = function
    | E -> failwith "empty"
    | T (_, x, _, _) -> x

  let deleteMin = function
    | E -> failwith "empty"
    | T (_, _, a, b) -> merge a b

end;;


module BinomialHeap(Element : Ordered) = struct
  type tree = Node of int * Element.t * tree list
  type heap = tree

  let empty = []

  let isEmpty = function
    | [] -> true
    | _ -> false

  let rank (Node (r, _, _)) = r

  let root (Node (r, x, c)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) = 
    if Element.leq x1 x2 then
      Node (r + 1, x1, t2 :: c1)
    else
      Node (r + 1, x2, t1 :: c2)

  let rec insTree t ts = 
    match ts with
      | [] -> [t]
      | t' :: ts' ->
          if rank t < rank t' then
            t :: ts
          else
            insTree (link t t') ts'

  let insert x ts = insTree (Node (0, x, [])) ts

  let rec merge ts1 ts2 = 
    match (ts1, ts2) with
      | t, [] -> t
      | [], t -> t
      | t1 :: ts1', t2 :: ts2' ->
          if rank t1 < rank t2 then
            t1 :: merge ts1' ts2
          else if rank t2 < rank t1 then
            t2 :: merge ts1 ts2'
          else
            insTree (link t1 t2) (merge ts1' ts2')

  let rec removeMinTree = function
    | [] -> failwith "empty"
    | [t] -> t, []
    | t :: ts ->
        let t', ts' = removeMinTree ts in
          if Element.leq (root t) (root t') then 
            t, ts
          else
            t', t :: ts'

  let findMin ts = 
    let t, _ = removeMinTree ts in
      root t

  let deleteMin ts = 
    let Node (_, x, ts1), ts2 = removeMinTree ts in
      merge (List.rev ts1) ts2
end;;

(* simple queue *)
module BankersQueue = struct
  type 'a queue = 'a list * 'a list;;

  let empty = [], []

  let isEmpty = function 
    | [], _ -> true
    | _, _ -> false

  let checkf = function
    | [], r -> List.rev r, []
    | _ as q -> q

  let head = function
    | [], _ -> failwith "empty"
    | x :: f, _ -> x

  let tail = function
    | [], _ -> failwith "empty"
    | x :: f, r -> checkf (f, r)

  let append ((f, r), x) = 
    checkf (f, x :: r)
end;;

(* splay heaps *)
module SplayHeap(Element : Ordered) : (Heap with type t := Element.t)= struct
  type heap = E | T of heap * Element.t * heap

  let empty = E

  let isEmpty = function
    | E -> true
    | _ -> false

  let rec partition el t = 
    match t with
      | E -> E, E
      | T (a, x, b) ->
          if x <= el then
            match b with
              | E -> t, E
              | T (b1, y, b2) ->
                  if y <= el then
                    let small, big = partition el b2 in
                      T (T (a, x, b), y, small), big
                  else
                    let small, big = partition el b1 in
                      T(a, x, small), T (big, y, b2)
          else
            match a with
              | E -> E, t
              | T (a1, y, a2) ->
                  if y <= el then
                    let small, big = partition el a2 in
                      T (a1, y, small), T (big, x, b)
                  else
                    let small, big = partition el a1 in
                      small, T (big, y, T (a2, x, b))

  let insert x t =
    let a, b = partition x t in
      T (a, x, b)

  let rec merge t1 t2 = 
    match t1 with
      | E -> t2
      | T (a, x, b) ->
          let ta, tb = partition x t2 in
            T (merge ta a, x, merge tb b)

  let rec findMin = function
    | E -> failwith "empty"
    | T (E, x, b) -> x
    | T (a, x, b) -> findMin a

  let rec deleteMin = function
    | E -> failwith "empty"
    | T (E, x, b) -> b
    | T (T (E, x, b), y, c) -> T (b, y, c)
    | T (T (a, x, b), y, c) -> T (deleteMin a, x, T (b, y, c))
end;;


(* samples module *)
module Samples = struct
  module Ints = struct
    type t = int
    let eq = (=)
    let lt = (<)
    let gt = (>)
    let leq = (<=)
  end;;

  module Seq = struct
    type 'a lst = Nil | Cons of 'a * 'a lst

    type 'a seq = Nil' | Cons' of 'a * ('a * 'a) seq

    let rec sizeL = function
      | Nil -> 0
      | Cons (_, xs) -> 1 + sizeL xs

    let rec sizeS : 'a. 'a seq -> int = function
      | Nil' -> 0
      | Cons' (_, xs) -> 1 + 2 * sizeS xs
  end;;
  module IntSet = UnbalancedSet(Ints);;
  (*let my_tree = T (T (T (E, 4, E), 6, T (E, 7, E)), 8, T (E, 12, E));;*)
  IntSet.insert 8 IntSet.empty;;

  (*let my_leftist_heap = T(1, 1, T (1, 3, E, E), E)*)

  (* lazy evaluations *)

  let rec fib n = 
    if n < 2 then n
    else
      fib (n - 1) + fib (n - 2)

  let rec memo_fib = 
    let cache = Hashtbl.create 10 in
      fun n ->
        try Hashtbl.find cache n
        with Not_found -> begin
            if n < 2 then n
            else 
              let f = memo_fib (n-1) + memo_fib (n-2) in
                Hashtbl.add cache n f; f
          end

  (* dumb range function *)
  let rec range n = 
    if n <= 0 then []
    else List.append (range (n - 1)) [n]

  let fibs n = 
    List.map fib (range n)

  let lazy_fibns n =
    Stream.from
      (fun i -> if i = n then None
        else Some (fib n))

  let memo_fibns n = 
    Stream.from
      (fun i -> if i = n then None
        else Some (memo_fib n))
end;; 

(* streams *)
module Streams = struct
  type 'a stream = Nil | Cons of 'a * 'a stream Lazy.t;;

  let empty_stream = Nil;;

  let stream_cons x s = Cons (x, lazy s);;

  let stream_first = function
    | Nil -> failwith "empty stream"
    | Cons (a, _) -> a

  let stream_rest = function
    | Nil -> failwith "empty stream"
    | Cons (_, lazy b) -> b

  let rec stream_append s1 s2 = 
    match s1, s2 with
      | Nil, s -> s
      | Cons (a, lazy b), _ ->
          Cons (a, lazy (stream_append b s2))

  let rec stream_rev = function
    | Nil -> Nil
    | Cons (a, lazy b) -> 
        stream_append (stream_rev b) (Cons (a, lazy empty_stream))

  let (++) = stream_append;;
end;;

(* lazy banker's queue *)
module LazyBankersQueue = struct
  open Streams

  type 'a queue = int * 'a stream * int * 'a stream;;

  let empty = [0, empty_stream, 0, empty_stream]

  let isEmpty (lenf, _, _, _) = (lenf = 0)

  let check (lenf, f, lenr, r as q)= 
    if lenf <= lenr then q
    else lenf + lenr, f ++ stream_rev r, 0, empty_stream

  let append (lenf, f, lenr, r) x = 
    check (lenf, f, lenr + 1, stream_cons x r)

  let head = function
    | _, Nil, _, _ -> failwith "empty"
    | lenf, Cons (x, _), _, _ -> x

  let tail = function
    | _, Nil, _, _ -> failwith "empty"
    | lenf, Cons (x, lazy f'), lenr, r -> check (lenf - 1, f', lenr, r)
end;;

(* real-time queue *)

module RealTimeQueue = struct
  open Streams

  type 'a queue = 'a stream * 'a list * 'a stream;;

  let empty = empty_stream, [], empty_stream

  let isEmpty = function
    | Nil, _, _ -> true
    | _ -> false
 
  let rec rotate = function
    | Nil, y :: _, a -> Cons (y, lazy a)
    | Cons (x, lazy xs), y :: ys, a ->
        Cons (x, lazy (rotate (xs, ys, Cons (y, lazy a))))
    | _ -> failwith "unexpected case"
 
  let exec = function
    | f, r, Cons (_, lazy s) -> f, r, s
    | f, r, Nil -> 
        let f' = rotate (f, r, empty_stream) in
          f', [], f'

  let append (f, r, s) x = exec (f, x :: r , s);;

  let head = function
    | Nil, _, _ -> failwith "empty"
    | Cons (x, lazy f), _, _ -> x

  let tail = function
    | Nil, _, _ -> failwith "empty"
    | Cons (x, lazy f), r, s -> exec (f, r, s)

end;;

module HoodMelvilleQueue = struct
  type 'a rotation_state =
        Idle 
      | Reversing of int * 'a list * 'a list * 'a list * 'a list
      | Appending of int * 'a list * 'a list
      | Done of 'a list

  type 'a queue = int * 'a list * 'a rotation_state * int * 'a list

  let empty = [0, [], Idle, 0, []]

  let isEmpty (lenf, f, state, lenr, r) = (lenf = 0)

  let exec = function
    | Reversing (ok, x :: f, f', y :: r, r') ->
        Reversing (ok + 1, f, x :: f', r, y :: r')
    | Reversing (ok, [], f', [y], r') ->
        Appending (ok, f', y :: r')
    | Appending (0, f', r') -> Done (r')
    | Appending (ok, x :: f', r') -> Appending (ok - 1, f', x :: r')
    | _ as state -> state

  let invalidate = function
    | Reversing (ok, f, f', r, r') -> Reversing (ok - 1, f, f', r, r')
    | Appending (0, f', x :: r') -> Done r'
    | Appending (ok, f', r') -> Appending (ok - 1, f', r')
    | _ as s -> s

  let exec2 (lenf, f, state, lenr, r) =
    match exec (exec state) with
      | Done newf -> lenf, newf, Idle, lenr, r
      | _ as newstate -> lenf, f, newstate, lenr, r

  let check (lenf, f, state, lenr, r as q) = 
    if lenr <= lenf then
      exec2 q
    else
      let newstate = Reversing (0, f, [], r, []) in
        exec2 (lenf + lenr, f, newstate, 0, [])

  let append (lenf, f, state, lenr, r) x = 
    check (lenf, f, state, lenr + 1, x :: r)

  let head = function
    | _, [], _, _, _ -> failwith "empty"
    | _, x :: f, _, _, _ -> x

  let tail = function
    | _, [], _, _, _ -> failwith "empty"
    | lenf, x :: f, state, lenr, r ->
        check (lenf - 1, f, invalidate state, lenr, r)
end;;

module CatenableList(Q : Queue) = struct
  type 'a cat = Empty | Cat of 'a * 'a cat Lazy.t Q.queue

  let empty = Empty

  let isEmpty = function
    | Empty -> true
    | _ -> false

  let link t s = 
    match t with
      | Cat (x, q) -> Cat (x, Q.append q s)
      | Empty -> Empty

  let rec linkAll q =
    let (lazy t) = Q.head q in
    let q' = Q.tail q in
      if Q.isEmpty q then t
      else link t (lazy (linkAll q'))

  let (++) l1 l2 = 
    match l1, l2 with
      | l, Empty -> l
      | Empty, l -> l
      | a, b -> link a (lazy b)

  let cons x xs = Cat(x, Q.empty) ++ xs
  let append xs x = xs ++ Cat(x, Q.empty)

  let head = function
    | Empty -> failwith "empty"
    | Cat(x, _) -> x

  let tail = function
    | Empty -> failwith "empty"
    | Cat(x, q) -> if Q.isEmpty q then Empty else linkAll q
end;;
