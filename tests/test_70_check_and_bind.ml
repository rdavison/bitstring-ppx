(* Test check() and bind().
 * $Id: test_70_check_and_bind.ml 187 2012-01-17 12:39:09Z richard.wm.jones@gmail.com $
 *)

open Printf
open Bitstring

let bits = [%bitstring 101 [@l 16], 202 [@l 16]]

let () =
  match%bitstring bits with
  | i [@l 16] [@check (i = 101)] [@bind (i*4)],
    j [@l 16] [@check (j = 202)] ->
      if i <> 404 || j <> 202 then
        failwith (sprintf "70_check_and_bind: failed: %d %d" i j)
  | _ ->
      failwith "70_check_and_bind: match failed"
