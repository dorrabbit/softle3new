(* Exercise 2.6 *)

let usdrate = 111.12;;
let yen_of_usdollar usd =
  int_of_float (floor (usd *. usdrate +. 0.5))
;;
let usdollar_of_yen yen =
  (floor ((float_of_int yen) /. usdrate *. 100.0 +. 0.5)) /. 100.0
;;
let yen_of_usdollar_message usd =
  string_of_float usd ^ " dollars are " ^ string_of_int(yen_of_usdollar usd) ^ " yen."
;;
let capitalize moji =
  if (int_of_char moji >= int_of_char 'a' && int_of_char moji <= int_of_char 'z')
      then char_of_int ((int_of_char moji) - 32)
  else moji
;;

(* Exercise 3.3 *)
let geo_mean (a, b) = sqrt (a *. b);;

(* Exercise 3.7 *)
let rec pow1 (x, n) =
  if n=0 then 1.0
  else x *. pow1 (x, n-1)
;;
let rec pow2 (x, n) =
  if n=0 then 1.0
  else if n mod 2=0 then let powdata = pow2 (x, n/2) in powdata *. powdata
  else let powdata = pow2 (x, n/2) in x *. powdata *. powdata
;;

(* Exercise 3.8 *)
let rec powi (x, n, res) =
  if n=0 then res
  else powi (x, n-1, x*.res)
;;

(* Exercise 3.11 *)
let rec gcd (m, n) =
  if m<=n then if m=0 then n
               else gcd (m, n mod m)
  else if n=0 then m
       else gcd (m mod n, n)
;;
let rec comb (n, m) =
  if (m=0||m=n) then 1
  else comb(n-1, m)+comb(n-1,m-1)
;;
let fib_iter num =
  let rec fib_iter_pre (num, res) =
    if num=1 then res
    else fib_iter_pre (num-1, res*num) in
  fib_iter_pre (num, 1)
;;
let max_ascii sent =
  let rec max_ascii_pre (num, max, sent) =
    if num = String.length sent then max
    else if int_of_char sent.[num] > max then max_ascii_pre(num+1, int_of_char sent.[num], sent)
    else max_ascii_pre(num+1, max, sent) in
  char_of_int (max_ascii_pre (0, 0, sent))
;;

(* Exercise 3.12 *)
let rec arctan_one n =
  if n<0 then 0.
  else arctan_one (n-1) +. 1. /. (float_of_int (4 * n + 1)) -. 1. /. (float_of_int (4 * n + 3))
;;
    
(* Exercise 4.1 *)
let integral f a b =
  let rec integral_pre f a b n sum=
    let dx = (b -. a) /. (float_of_int n) in
    if n=0 then sum
    else integral_pre f (a+.dx) b (n-1) (sum +. (((f a)+.(f (a+.dx)))*.dx)/.2.) in
  integral_pre f a b (int_of_float 1e6) 0.
;;

(* Exercise 4.4 *)
let uncurry f (a, b) = f a b
;;

(* Exercise 4.5 *)
let rec repeat f n x =
  if n>0 then repeat f (n-1) (f x) else x
;;
let fib_repeat n =
  let (fibn, _) = repeat (fun (x, s) -> (x+s, x)) (n-1) (1, 0) 
  in fibn
;;

(* Exercise 4.7 *)
(*s k k 1 = k 1 (k 1)
          = 1 (because k 1 x = 1 in any x)*)
let k x y = x
;;
let s x y z = x z (y z)
;;
let second x y = k (s k k) x y
;;

(* prepare *)
let hd (x::rest) = x;;
let tl (x::rest) = rest;;
let null = function [] -> true | _ -> false;;
let rec nth n l =
  if n = 1 then hd l else nth (n - 1) (tl l);;
let rec take n l =
  if n = 0 then [] else (hd l) :: (take (n - 1) (tl l));;
let rec drop n l =
  if n = 0 then l else drop (n - 1) (tl l);;

(* Exercise 5.2 *)
let rec sum_list l =
  if null l then 0
  else hd l + sum_list (tl l)
;;
let rec max_list l =
  if null (tl l) then hd l
  else if hd l>hd (tl l) then max_list (hd l :: tl (tl l))
                         else max_list (hd (tl l) :: tl (tl l))
;;
(* matchを使用することで関数のパターンの場合分けと変数の定義が同時に行えるため、
   書きやすく読みやすい。 *)

(* Exercise 5.3 *)
let rec downto0 n =
  if n<0 then []
  else n :: (downto0 (n-1))
;;
let rec roman romlist num =
  if romlist=[] then ""
  else
  let (uni, chara) = hd romlist in
  let times = num / uni in
  let rec charatimes times chara str =
    if times=0 then str
    else chara ^ (charatimes (times-1) chara str) in
  (charatimes times chara "") ^ (roman (tl romlist) (num mod uni))
;;
let rec concat list_list =
  if list_list=[] then []
  else (hd list_list) @ (concat (tl list_list))
;;
let rec zip lista listb =
  match lista with
    [] -> []
  | a :: tla -> match listb with
                  [] -> []
                | b :: tlb -> (a, b) :: (zip tla tlb)
;;
let rec filter f l =
  match l with
    [] -> []
  | x :: t -> if (f x) then x :: (filter f t)
              else filter f t
;;
let rec belong a s =
  match s with
    [] -> false
  | x :: t -> if x=a then true
                     else belong a t
;;
let rec intersect s1 s2 =
  match s1 with
    [] -> []
  | x :: t -> if belong x s2 then x :: intersect t s2
                             else intersect t s2
;;
let rec union s1 s2 =
  match s1 with
    [] -> s2
  | x :: t -> if belong x s2 then union t s2
                             else x :: union t s2
;;
let rec diff s1 s2 =
  match s1 with
    [] -> []
  | x :: t -> if belong x s2 then diff t s2
                             else x :: diff t s2
;;

(* Exercise 5.6 *)
let rec quicker l sorted = match l with
    [] -> sorted
  | x :: xs ->
      let rec partition left right = function
        [] -> quicker left (x :: (quicker right sorted))
      | y :: ys -> if x<y then partition left (y :: right) ys
                   else partition (y :: left) right ys
      in partition [] [] xs
;;

(* Exercise 5.7 *)
let squares r =
let squares_judge x y r =
  pow2 (float_of_int x, 2) +. pow2 (float_of_int y, 2) = float_of_int r in
let rec squares_pre y r =
  let x = int_of_float (sqrt (float_of_int r -. pow2 (float_of_int y, 2))) in
  if y<0 then []
  else if squares_judge x y r then (x, y) :: squares_pre (y-1) r
                              else squares_pre (y-1) r in
  let starty = int_of_float (sqrt (float_of_int r /. 2.)) in
  squares_pre starty r
;;

(* Exercise 5.8 *)
let map2 f l =
  let rec map_pre f l res =
    match l with
      [] -> res
    | x :: t -> f x :: map_pre f t res in
  map_pre f l []
;;

(* Exercise 6.2 *)
type nat = Zero | OneMoreThan of nat;;
let rec int_of_nat = function
    Zero -> 0
  | OneMoreThan nat2 -> 1 + (int_of_nat nat2)
;;
let rec add m n =
  match m with Zero -> n | OneMoreThan m' -> OneMoreThan (add m' n)
;;
let rec mul m n =
  match m with
    Zero -> Zero
  | OneMoreThan m' -> add (mul m' n) n
;;
let rec monus m n =
  match n with
    Zero -> m
  | OneMoreThan n' -> match m with
                        Zero -> Zero
                      | OneMoreThan m' -> monus m' n'
;;

(* Exercise 6.3 *)
let rec minus m n =
  match n with
    Zero -> Some m
  | OneMoreThan n' -> match m with
                        Zero -> None
                      | OneMoreThan m' -> minus m' n'
;;

(* Exercise 6.6 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
let comptree = Br(1, Br(2, Br(4, Lf, Lf), Br(5, Lf, Lf)), Br(3, Br(6, Lf, Lf), Br(7, Lf, Lf)));;
let rec reflect = function
    Lf -> Lf
  | Br (x, left, right) -> Br (x, reflect right, reflect left)
;;
(* preorder(reflect(t)) =  reverse(postorder(t))
   inorder(reflect(t)) = reverse(inorder(t))
   postorder(reflect(t)) = reverse(preorder(t)) *)

(* Exercise 6.9 *)
type 'a seq = Cons of 'a * (unit -> 'a seq);;
let rec from n = Cons (n, fun () -> from (n + 1));;
let head (Cons (x, _)) = x;;
let tail (Cons (_, f)) = f ();;
let rec take n s =
  if n = 0 then [] else head s :: take (n - 1) (tail s);;

let rec sift n (Cons (x, tail)) =
  if x mod n!=0 then Cons(x, fun () -> sift n (tail()))
                else sift n (tail())
;;
let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f())));;
let primes = sieve (from 2);;
let rec nthseq n (Cons (x, f)) =
  if n = 1 then x else nthseq (n - 1) (f());;
(* let myprime = nthseq (1029288922+3000) primes;; *)

(* Exercise 6.10 *)
type ('a, 'b) sum = Left of 'a | Right of 'b;;

let f1 (a, s) =
  match s with
    Left b -> Left (a, b)
  | Right c -> Right (a, c)
;;
let f2 (ab, cd) =
  match cd with
    Left c -> (match ab with
                 Left a -> Left (Left (a, c))
               | Right b -> Right (Right (b, c)))
  | Right d -> (match ab with
                  Left a -> Right (Left (a, d))
                | Right b -> Left (Right (b, d)))
;;
let f3 (f, g) = function
                  Left a -> f a
                | Right b -> g b
;;

(* Exercise 7.2 *)
let incr x = x := !x + 1
;;

(* Exercise 7.4 *)
let fact_imp n =
  let i = ref n and res = ref 1 in
  while (!i>0) do
  res := !res * !i;
  i := !i - 1;
  done;
  !res;;

(* Exercise 7.6 *)
(*
# let x = ref [];;
val x : '_a list ref = {contents = []}
となる。'aが'_aとなっている。これは一度だけ任意の型に置換できる型変数であり、
一度決まってしまうと二度と別の型として使用できない。
そのため、
# (2 :: !x, true :: !x);; に対し、
Error: This expression has type int list
       but an expression was expected of type bool list
       Type int is not compatible with type bool
というエラーを吐かれる。
これは2 :: !xで'_aがintに置換されたため、
その後true :: !xでboolとintをconsしようとしたことになるため。
よって異なる型をconsすることは防がれる。
*)

(* Exercise 7.8 *)
let rec change =
  function
    (_, 0) -> []
  | ((c :: rest) as coins, total) ->
    if c > total then change (rest, total)
    else
      (try
        c :: change (coins, total - c)
        with Failure "change" -> change (rest, total))
  | _ -> raise (Failure "change")
;;
