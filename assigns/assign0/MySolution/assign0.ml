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
let rec fact (x :int) =
  if x > 0 then
    x * fact (x - 1)
  else
    1

let rec find_overflow (y :int): int =
  if fact(y) = 0 then
   y 
  else
      find_overflow (y + 1)

  (*Printf.printf "The first N for which fact(N) causes an Overflow exception is: %d\n" ;;*)

(*result is N = 64*)
(* ****** ****** *)

(*
Assign0-2: 10 points
Please implement a function that tests whether a
given natural number is a prime:
fun isPrime(n0: int): bool
*)

let isPrime (n0:int) : bool =
  let rec isPrimeHelper n0 i =
    if i = 1 then true
    else if n0 mod i = 0 then false
    else isPrimeHelper n0 (i-1)
  in
  isPrimeHelper n0 (n0-1)

(* ****** ****** *)

(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)
(* ****** ****** *)
let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
;;
(* ****** ****** *)

let int2str (i0: int): string =
  let rec int2str_helper i result =
    if i = 0 then
      if result = "" then "0" else result
    else
      let digit = i mod 10 in
      let digit_char = chr (digit + ord '0') in
      let new_result = str (digit_char) ^ result in
      int2str_helper (i / 10) new_result
  in
  int2str_helper i0 ""


(* ****** ****** *)

(*
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
*)

(* ****** ****** *)
(*let ord = Char.code*)
let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
;;
(* ****** ****** *)

let str2int (cs: string): int =
  let rec str2int_helper cs index result =
    if index >= (string_length cs) then
      result
    else
      let digit = string_get(cs, index) in
      let digit_int = ord digit - ord '0' in  
      let new_result = (10 * result) + digit_int in
      str2int_helper cs (index + 1) new_result
  in
  str2int_helper cs 0 0;;
    

(* ****** ****** *)

(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
*)


let string_rev (cs: string): string =
  let len = string_length cs in
  let result = string_init len (fun i -> string_get(cs, len - 1 - i)) in
  result;;


(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0.ml] *)

