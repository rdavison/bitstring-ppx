(* Named pattern
 * $Id: test_50_named_pattern.ml 187 2012-01-17 12:39:09Z richard.wm.jones@gmail.com $
 *)

open Printf
open Bitstring

(* A byte+length Pascal string. *)
let pascal_string =
  [%bitstring ? len [@l 8], str [@l len*8] [@string]]

let () =
  let bits = bitstring_of_string "\022Mary had a little lamb" in
  match%bitstring bits with
  | [%pascal_string] ->
      () (*printf "it's a Pascal string, len = %d, string = %S\n" len str*)
  | _ ->
      eprintf "not matching error\n";
      exit 1
