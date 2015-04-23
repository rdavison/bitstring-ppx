(* Parse and display an IPv4 header from a file.
 * $Id: ipv4_header.ml 142 2008-07-17 15:45:56Z richard.wm.jones $
 *)

open Printf

let header = Bitstring.bitstring_of_file "ipv4_header.dat"

let () =
  match%bitstring header with
  | version [@l 4], hdrlen [@l 4], tos [@l 8], length [@l 16],
    identification [@l 16], flags [@l 3], fragoffset [@l 13],
    ttl [@l 8], protocol [@l 8], checksum [@l 16],
    source [@l 32],
    dest [@l 32],
    options [@l (hdrlen-5)*32] [@bitstring],
    payload [@l -1] [@bitstring]
    when version = 4 ->

      printf "IPv%d:\n" version;
      printf "  header length: %d * 32 bit words\n" hdrlen;
      printf "  type of service: %d\n" tos;
      printf "  packet length: %d bytes\n" length;
      printf "  identification: %d\n" identification;
      printf "  flags: %d\n" flags;
      printf "  fragment offset: %d\n" fragoffset;
      printf "  ttl: %d\n" ttl;
      printf "  protocol: %d\n" protocol;
      printf "  checksum: %d\n" checksum;
      printf "  source: %lx  dest: %lx\n" source dest;
      printf "  header options + padding:\n";
      Bitstring.hexdump_bitstring stdout options;
      printf "  packet payload:\n";
      Bitstring.hexdump_bitstring stdout payload

  | version [@l 4] ->
      eprintf "cannot parse IP version %d\n" version

  | _ as header ->
      eprintf "data is smaller than one nibble:\n";
      Bitstring.hexdump_bitstring stderr header
