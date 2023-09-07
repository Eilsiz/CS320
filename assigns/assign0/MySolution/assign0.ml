(* ****** ****** *)
(*
Assign0: Warmup!
*)
(* ****** ****** *)

(*
fun fact(x: int): int =
  if x > 0 then x * fact(x-1) else 1
*)


(*
Assign0-1: 10 points
Please find the first integer N such that the
evaluation of fact(N) in OCaml yields an Overflow
exception.
*)

let rec fact x =
  if x > 0 then
    x * fact (x - 1)
  else
    1

let rec find_overflow n =
  try
    let _ = fact n in
    find_overflow (n + 1)
  with
  | _ -> n

let () =
  let n = find_overflow 1 in
  Printf.printf "The first N for which fact(N) causes an Overflow exception is: %d\n" n

(*result is N = 21*)
(* ****** ****** *)

(*
Assign0-2: 10 points
Please implement a function that tests whether a
given natural number is a prime:
fun isPrime(n0: int): bool
*)

let is_prime n0 =
  if n0 <= 1 then
    false
  else if n0 <= 3 then
    true
  else if n0 mod 2 = 0 || n0 mod 3 = 0 then
    false
  else
    let rec is_prime_helper n i =
      if i * i > n then
        true
      else if n mod i = 0 then
        false
      else
        is_prime_helper n (i + 2)
    in
    is_prime_helper n0 5


(* ****** ****** *)

(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

let int2str i0 =
  string_of_int i0


(* ****** ****** *)

(*
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
*)

let str2int cs = int_of_string cs

(* ****** ****** *)

(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
*)


let rec string_rev cs =
  match cs with
  | "" -> ""
  | _ -> (String.sub cs (String.length cs - 1) 1) ^ string_rev (String.sub cs 0 (String.length cs - 1))


(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0.ml] *)
