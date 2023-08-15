(* region: Lists *)
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

(* 7. Flatten a List *)
type 'a node = One of 'a | Many of 'a node list

let flatten list =
  let rec aux list out =
    match list with
    | [] -> out
    | One hd :: tl -> aux tl (out @ [ hd ])
    | Many hds :: tl -> aux tl (aux hds out)
  in
  aux list []

(* 8. Eliminate Duplicates *)
let rec compress = function
  | a :: (b :: _ as tl) -> if a = b then compress tl else a :: compress tl
  | last -> last

(* 9. Pack Consecutive Duplicates *)
let pack list =
  let rec aux curr acc = function
    | [] -> []
    | [ x ] -> (x :: curr) :: acc (* because of consecutiveness it works *)
    | a :: (b :: _ as tl) ->
        if a = b then aux (a :: curr) acc tl else aux [] ((a :: curr) :: acc) tl
  in
  rev (aux [] [] list)

(* 10. Run-Length Encoding *)
let encode list =
  List.map (fun lst -> (List.length lst, List.hd lst)) (pack list)

(* 11  Modified Run-Length Encoding *)
type 'a rle = One of 'a | Many of int * 'a

let encode list =
  List.map
    (fun lst ->
      if List.length lst = 1 then One (List.hd lst)
      else Many (List.length lst, List.hd lst))
    (pack list)

(* 12. Decode a Run-Length Encoded List *)
let decode list =
  let rec unwrap out len x =
    if len = 0 then out else unwrap (out @ [ x ]) (len - 1) x
  in
  let rec aux out = function
    | [] -> out
    | One hd :: tl -> aux (out @ [ hd ]) tl
    | Many (len, x) :: tl -> aux (unwrap out len x) tl
  in
  aux [] list

(* 13. Run-Length Encoding of a List (Direct Solution) as 11 *)
(* 14. Duplicate the Elements of a List *)
let rec duplicate = function [] -> [] | hd :: tl -> hd :: hd :: duplicate tl

(* 15. Replicate the Elements of a List a Given Number of Times *)
let replicate list n =
  let rec append_to_n n out hd =
    if n = 0 then out else append_to_n (n - 1) (out @ [ hd ]) hd
  in

  let rec aux out n = function
    | [] -> out
    | hd :: tl -> aux (append_to_n n out hd) n tl
  in
  aux [] n list

(* 16. Drop Every N'th Element From a List *)
let drop list n =
  let rec maybe_remove idx n hd out =
    if idx mod n = 0 then out else out @ [ hd ]
  in
  let rec aux out idx n = function
    | [] -> out
    | hd :: tl -> aux (maybe_remove idx n hd out) (idx + 1) n tl
  in
  aux [] 1 n list

(* 17. Split a List Into Two Parts; The Length of the First Part Is Given *)
let split list n =
  let rec aux idx frst_part = function
    | [] -> (frst_part, [])
    | hd :: tl as lst ->
        if idx = 0 then (frst_part, lst)
        else aux (idx - 1) (frst_part @ [ hd ]) tl
  in
  aux n [] list

(* 18. Extract a Slice From a List *)
let slice list l r =
  let rec aux idx out l r = function
    | [] -> out
    | hd :: tl ->
        if idx >= l && idx <= r then aux (idx + 1) (out @ [ hd ]) l r tl
        else aux (idx + 1) out l r tl
  in
  aux 0 [] l r list

(* 19. Rotate a List N Places to the Left *)
let rotate list n =
  let rec aux idx out n = function
    | [] -> out
    | hd :: tl ->
        if idx < n then aux (idx + 1) out n tl
        else aux (idx + 1) (out @ [ hd ]) n tl
  in
  aux 0 [] n list

(* 20. Remove the K'th Element From a List *)
let remove_at n list =
  let rec aux idx out n = function
    | [] -> out
    | hd :: tl ->
        if idx = n then aux (idx + 1) out n tl
        else aux (idx + 1) (out @ [ hd ]) n tl
  in
  aux 0 [] n list

(* 21. Insert an Element at a Given Position Into a List *)
let rec insert_at elem idx = function
  | [] -> [ elem ]
  | hd :: tl as lst ->
      if idx = 0 then elem :: lst else hd :: insert_at elem (idx - 1) tl

(* 22. Create a List Containing All Integers Within a Given Range *)
let range l r =
  let rec aux out r l = if r >= l then aux (r :: out) (r - 1) l else out in
  if l < r then aux [] r l else rev (aux [] l r)

(* 23. Extract a Given Number of Randomly Selected Elements From a List *)
let rand_select list n =
  let rec get out idx = function
    | [] -> raise Exit
    | hd :: tl ->
        if idx = 0 then (hd, out @ tl) else get (hd :: out) (idx - 1) tl
  in
  let rec aux n out list len =
    if n = 0 then out
    else
      let picked, rest = get [] (Random.int len) list in
      aux (n - 1) (picked :: out) rest (len - 1)
  in
  aux n [] list (List.length list)

(* 24. Lotto: Draw N Different Random Numbers From the Set 1..M *)
let lotto_select n m = rand_select (range 1 m) n

(* 25. Generate a Random Permutation of the Elements of a List *)
let permutation list = rand_select list (List.length list)

(* 26. Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List *)
let rec extract k list =
  if k = 0 then [ [] ]
  else
    match list with
    | [] -> []
    | hd :: tl ->
        let with_h = List.map (fun lst -> hd :: lst) (extract (k - 1) tl) in
        let without_h = extract k tl in
        with_h @ without_h

(* Too hard :( 27. Group the Elements of a Set Into Disjoint Subsets *)
(* Too hard :( 28. Sorting a List of Lists According to Length of Sublists *)

(* endregion: Lists *)
