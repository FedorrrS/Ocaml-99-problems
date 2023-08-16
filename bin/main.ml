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

(* region: Arithmetic*)
(* 29. Determine Whether a Given Integer Number Is Prime *)
let is_prime n =
  let rec not_divisor_of div =
    div * div > n || (n mod div <> 0 && not_divisor_of (div + 1))
  in
  n <> 1 && not_divisor_of 2

(* 30. Determine the Greatest Common Divisor of Two Positive Integer Numbers *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(* 31. Determine Whether Two Positive Integer Numbers Are Coprime *)
let coprime a b = gcd a b = 1

(* 32. Calculate Euler's Totient Function Φ(m) *)
let phi m =
  let rec aux counter m = function
    | [] -> counter
    | hd :: tl ->
        if coprime hd m then aux (counter + 1) m tl else aux counter m tl
  in

  aux 0 m (range 1 (m - 1))

(* 33. Determine the Prime Factors of a Given Positive Integer *)
let factors n =
  let rec aux n d =
    if n = 1 then []
    else if n mod d = 0 then d :: aux (n / d) d
    else aux n (d + 1)
  in
  aux n 2

(* 34. Determine the Prime Factors of a Given Positive Integer (2) *)
let factors n =
  let list = factors n in
  List.map (fun lst -> (List.hd lst, List.length lst)) (pack list)

(* 35. Calculate Euler's Totient Function Φ(m) (Improved) *)
let phi_improved m =
  (* wondered how it is not in std lol *)
  let rec pow n = function
    | 0 -> 1
    | 1 -> n
    | n ->
        let b = pow n (n / 2) in
        b * b * if n mod 2 = 0 then 1 else n
  in
  let rec aux mul = function
    | [] -> mul
    | (p, m) :: tl -> aux (mul * (p - 1) * pow p (m - 1)) tl
  in
  aux 1 (factors m)

(* 36. Compare the Two Methods of Calculating Euler's Totient Function *)
let timeit f n =
  let until = Sys.time () in
  let _ = f n in
  let after = Sys.time () in
  after -. until

(* 37. A List of Prime Numbers *)
let rec all_primes a b =
  if a > b then []
  else
    let lst = all_primes (a + 1) b in
    if is_prime a then a :: lst else lst

(* 38. Goldbach's Conjecture *)
let goldbach n =
  let primes_of_n = all_primes 2 n in
  let rec aux = function
    | [] -> raise Exit (* dunno how to handle this *)
    | hd :: tl -> if is_prime (n - hd) then (hd, n - hd) else aux tl
  in
  aux primes_of_n

(* 39. A List of Goldbach Compositions *)
let goldbach_list a b =
  let rec aux a b =
    if a > b then []
    else if a mod 2 = 0 then (a, goldbach a) :: aux (a + 2) b
    else aux (a + 1) b
  in
  aux (a + 1) b

(* endregion: Arithmetic*)

(* region: Logic and Codes *)
(* 40. Truth Tables for Logical Expressions (2 Variables) done in 41 *)
(* 41. Truth Tables for Logical Expressions *)
type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let table variables expr =
  let rec eval value_of_variables = function
    | Var x ->
        List.assoc x
          value_of_variables (* dunno how to use pretty Jane Street maps :( )*)
    | Not expr -> not (eval value_of_variables expr)
    | And (expr1, expr2) ->
        eval value_of_variables expr1 && eval value_of_variables expr2
    | Or (expr1, expr2) ->
        eval value_of_variables expr1 || eval value_of_variables expr2
  in

  let rec create_table value_of_variables expr = function
    | [] -> [ (value_of_variables, eval value_of_variables expr) ]
    | hd :: tl ->
        create_table (value_of_variables @ [ (hd, true) ]) expr tl
        @ create_table (value_of_variables @ [ (hd, false) ]) expr tl
  in
  create_table [] expr variables

(* 42. Gray Code *)
(* example for n = 3
   n = 1: 0  -> n = 2: 00  -> n = 3: 000
          1            01            001
          1            11            011
          0            10            010
                                     110
                                     111
                                     101
                                     100
*)
let gray n =
  let rec gray_reflected idx list =
    if idx < n then
      let upper, lower =
        List.fold_left
          (fun (acc1, acc2) b -> (("0" ^ b) :: acc1, ("1" ^ b) :: acc2))
          ([], []) list
      in
      gray_reflected (idx + 1) (List.rev_append upper lower)
    else list
  in
  gray_reflected 1 [ "0"; "1" ]

(* Definitely kidding me, somewhere in future.. 43. Huffman Code *)

(* endregion: Logic and Codes *)

(* region: Binary Trees *)
(* 43. Construct Completely Balanced Binary Trees *)
type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

(* Difference of nodes number of left and right subtrees is not greater than one *)
let gen_tree l r out =
  let rec gen_subtree out subtree =
    List.fold_left (fun lst t -> Node ('x', subtree, t) :: lst) out r
  in
  List.fold_left gen_subtree out l

let rec cbal_tree n =
  if n = 0 then [ Empty ]
  else if n mod 2 = 1 then
    let t = cbal_tree (n / 2) in
    gen_tree t t []
  else
    let tl = cbal_tree ((n / 2) - 1) in
    let tr = cbal_tree (n / 2) in
    gen_tree tr tl [] |> gen_tree tl tr

(* 43. Symmetric Binary Trees *)
let is_symmetric tree =
  let rec is_mirror l r =
    match (l, r) with
    | Empty, Empty -> true
    | Node (_, l1, r1), Node (_, l2, r2) -> is_mirror l1 r2 && is_mirror r1 l2
    | _ -> false
  in
  match tree with Empty -> true | Node (_, l, r) -> is_mirror l r

(* 44. Binary Search Trees (Dictionaries) *)
let construct list =
  let rec push tree value =
    match tree with
    | Empty -> Node (value, Empty, Empty)
    | Node (already_in, l, r) ->
        if value = already_in then tree
        else if value > already_in then Node (already_in, l, push r value)
        else Node (already_in, push l value, r)
  in
  List.fold_left push Empty list

(* 45. Generate-and-Test Paradigm *)
let sym_cbal_trees n = List.filter is_symmetric (cbal_tree n)

(* 46. Construct Height-Balanced Binary Trees *)
(* Height difference of left and right subtrees is not greater than one*)
let rec hbal_tree n =
  if n = 0 then [ Empty ]
  else if n = 1 then [ Node ('x', Empty, Empty) ]
  else
    let tl = hbal_tree (n - 1) in
    let tr = hbal_tree (n - 2) in
    gen_tree tr tl [] |> gen_tree tl tr |> gen_tree tl tl

(* For better times.. 47. Construct Height-Balanced Binary Trees With a Given Number of Nodes *)

(* 48. Count the Leaves of a Binary Tree *)
let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r

(* 49. Collect the Leaves of a Binary Tree in a List *)
let leaves tree =
  let rec aux out = function
    | Empty -> out
    | Node (v, Empty, Empty) -> v :: out
    | Node (_, l, r) -> aux out l @ aux out r
  in
  aux [] tree

(* 50. Collect the Internal Nodes of a Binary Tree in a List *)
let internals tree =
  let rec aux out = function
    | Empty | Node (_, Empty, Empty) -> out
    | Node (v, l, r) -> aux (aux (v :: out) r) l
  in
  aux [] tree

(* 51. Collect the Nodes at a Given Level in a List *)
let at_level tree lvl =
  let rec aux out curr_lvl = function
    | Empty -> out
    | Node (v, l, r) ->
        if curr_lvl = lvl then v :: out
        else aux out (curr_lvl + 1) l @ aux out (curr_lvl + 1) r
  in
  aux [] 1 tree

(* Hardy.. 52. Construct a Complete Binary Tree *)

(* endregion: Binary Trees *)
