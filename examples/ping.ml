(* Read in IPv4 and IPv6 ping packets and display them.
 * $Id: ping.ml 142 2008-07-17 15:45:56Z richard.wm.jones $
 *)

open Printf

let display pkt =
  match%bitstring pkt with
  (* IPv4 packet header *)
  | 4 [@l 4], hdrlen [@l 4], tos [@l 8], length [@l 16],
    identification [@l 16], flags [@l 3], fragoffset [@l 13],
    ttl [@l 8], protocol [@l 8], checksum [@l 16],
    source [@l 32],
    dest [@l 32],
    options [@l (hdrlen-5)*32] [@bitstring],
    payload [@l -1] [@bitstring] ->

      printf "IPv4:\n";
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

  (* IPv6 packet header *)
  | 6 [@l 4], tclass [@l 8], flow [@l 20],
    length [@l 16], nexthdr [@l 8], ttl [@l 8],
    source [@l 128] [@bitstring],
    dest [@l 128] [@bitstring],
    payload [@l -1] [@bitstring] ->

      printf "IPv6:\n";
      printf "  traffic class: %d\n" tclass;
      printf "  flow label: %d\n" flow;
      printf "  packet (payload) length: %d bytes\n" length;
      printf "  next header: %d\n" nexthdr;
      printf "  ttl: %d\n" ttl;
      printf "  source address:\n";
      Bitstring.hexdump_bitstring stdout source;
      printf "  destination address:\n";
      Bitstring.hexdump_bitstring stdout dest;
      printf "packet payload:\n";
      Bitstring.hexdump_bitstring stdout payload

  | version [@l 4] ->
      eprintf "unknown IP version %d\n" version;
      exit 1

  | _ as pkt ->
      eprintf "data is smaller than one nibble:\n";
      Bitstring.hexdump_bitstring stderr pkt;
      exit 1

let () =
  let pkt = Bitstring.bitstring_of_file "ping.ipv4" in
  display pkt;
  let pkt = Bitstring.bitstring_of_file "ping.ipv6" in
  display pkt
