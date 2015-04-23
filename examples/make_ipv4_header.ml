(* Create an IPv4 header.
 * $Id: make_ipv4_header.ml 142 2008-07-17 15:45:56Z richard.wm.jones $
 *)

open Printf

let version = 4
let hdrlen = 5				(* no options *)
let tos = 16
let length = 64				(* total packet length *)
let identification = 0
let flags = 0
let fragoffset = 0
let ttl = 255
let protocol = 17			(* UDP *)
let checksum = 0
let source = 0xc0a80202_l		(* 192.168.2.2 *)
let dest = 0xc0a80201_l			(* 192.168.2.1 *)
let options = Bitstring.empty_bitstring
let payload_length = (length - hdrlen*4) * 8
let payload = Bitstring.create_bitstring payload_length

let header =
  [%bitstring
    version [@l 4], hdrlen [@l 4], tos [@l 8], length [@l 16],
    identification [@l 16], flags [@l 3], fragoffset [@l 13],
    ttl [@l 8], protocol [@l 8], checksum [@l 16],
    source [@l 32],
    dest [@l 32]
(*
  Not implemented at the moment XXX
    options : -1 : bitstring;
    payload : payload_length : bitstring
*)
  ]

let () = Bitstring.bitstring_to_file header "ipv4_header_out.dat"
