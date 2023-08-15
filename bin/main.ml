(* region: Easy *)
(* 1. Tail of a List *)
let rec last = function [] -> None | [ x ] -> Some x | _ :: tl -> last tl

(* 2. Last Two Elements of a List *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tl -> last_two tl

(* 3. N'th Element of a List *)
let rec nth n = function
  | [] -> None
  | hd :: _ when n = 0 -> Some hd
  | _ :: tl -> nth (n - 1) tl

(* 4. Length of a List *)
let length list =
  let rec aux n = function [] -> n | _ :: tl -> aux (n + 1) tl in
  aux 0 list

(* 5. Reverse a List *)
let rev list =
  let rec aux out = function [] -> out | hd :: tl -> aux (hd :: out) tl in
  aux [] list

(* 6. Palindrome *)
let is_palindrome list = list = rev list
(* endregion: Easy *)
