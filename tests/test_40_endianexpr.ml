(* Endianness expressions
 * $Id: test_40_endianexpr.ml 187 2012-01-17 12:39:09Z richard.wm.jones@gmail.com $
 *)

open Printf
open Bitstring

let () =
  let rec loop = function
    | (e, expected) :: rest ->
        let bits = [%bitstring
          expected [@l 32] [@endian e],
          expected [@l 32] [@endian e],
          expected [@l 32] [@endian e]
        ] in
        (match%bitstring bits with
         | actual [@l 32] [@endian e],
           actual [@l 32] [@endian e],
           actual [@l 32] [@endian e] ->
             if actual <> expected then
               failwith (sprintf "actual %ld <> expected %ld" actual expected)
         | _ as bits ->
             hexdump_bitstring stderr bits; exit 1
        );
        loop rest
    | [] -> ()
  in
  loop [
    BigEndian, 0xa1b2c3d4_l;
    BigEndian, 0xa1d4c3b2_l;
    LittleEndian, 0xa1b2c3d4_l;
    LittleEndian, 0xa1d4c3b2_l;
    NativeEndian, 0xa1b2c3d4_l;
    NativeEndian, 0xa1d4c3b2_l;
  ]
