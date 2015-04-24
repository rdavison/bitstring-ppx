(* Test functions which construct and extract fixed-length ints
 * of various sizes.
 * $Id$
 *)

open Printf

open Bitstring

let () =
  for i = 0 to 129 do
    let zeroes = zeroes_bitstring i in
    let bits = (
      [%bitstring
        zeroes [@l i] [@bitstring],
        true [@l 1],
        2 [@l 2] [@littleendian],
        2 [@l 2] [@bigendian],
        2 [@l 2] [@nativeendian],
        3 [@l 3] [@littleendian],
        3 [@l 3] [@bigendian],
        3 [@l 3] [@nativeendian],
        0x5a [@l 8] [@littleendian],
        0x5a [@l 8] [@bigendian],
        0x5a [@l 8] [@nativeendian],
        0xa5a5 [@l 16] [@littleendian],
        0xa5a5 [@l 16] [@bigendian],
        0xa5a5 [@l 16] [@nativeendian],
        0xeeddcc [@l 24] [@littleendian],
        0xeeddcc [@l 24] [@bigendian],
        0xeeddcc [@l 24] [@nativeendian],
        0x48888888 [@l 31] [@littleendian],
        0x48888888 [@l 31] [@bigendian],
        0x48888888 [@l 31] [@nativeendian],
        0xaabbccdd_l [@l 32] [@littleendian],
        0xaabbccdd_l [@l 32] [@bigendian],
        0xaabbccdd_l [@l 32] [@nativeendian],
        0xaabbccddeeff_L [@l 48] [@littleendian],
        0xaabbccddeeff_L [@l 48] [@bigendian],
        0xaabbccddeeff_L [@l 48] [@nativeendian],
        0x0011aabbccddeeff_L [@l 64] [@littleendian],
        0x0011aabbccddeeff_L [@l 64] [@bigendian],
        0x0011aabbccddeeff_L [@l 64] [@nativeendian]
      ]
    ) in
    match%bitstring bits with
    | _ [@l i] [@bitstring],
      a [@l 1],
      b0 [@l 2] [@littleendian],
      b1 [@l 2] [@bigendian],
      b2 [@l 2] [@nativeendian],
      c0 [@l 3] [@littleendian],
      c1 [@l 3] [@bigendian],
      c2 [@l 3] [@nativeendian],
      d0 [@l 8] [@littleendian],
      d1 [@l 8] [@bigendian],
      d2 [@l 8] [@nativeendian],
      e0 [@l 16] [@littleendian],
      e1 [@l 16] [@bigendian],
      e2 [@l 16] [@nativeendian],
      f0 [@l 24] [@littleendian],
      f1 [@l 24] [@bigendian],
      f2 [@l 24] [@nativeendian],
      g0 [@l 31] [@littleendian],
      g1 [@l 31] [@bigendian],
      g2 [@l 31] [@nativeendian],
      h0 [@l 32] [@littleendian],
      h1 [@l 32] [@bigendian],
      h2 [@l 32] [@nativeendian],
      j0 [@l 48] [@littleendian],
      j1 [@l 48] [@bigendian],
      j2 [@l 48] [@nativeendian],
      k0 [@l 64] [@littleendian],
      k1 [@l 64] [@bigendian],
      k2 [@l 64] [@nativeendian]
      ->
        if a <> true
           || b0 <> 2
           || b1 <> 2
           || b2 <> 2
           || c0 <> 3
           || c1 <> 3
           || c2 <> 3
           || d0 <> 0x5a
           || d1 <> 0x5a
           || d2 <> 0x5a
           || e0 <> 0xa5a5
           || e1 <> 0xa5a5
           || e2 <> 0xa5a5
           || f0 <> 0xeeddcc
           || f1 <> 0xeeddcc
           || f2 <> 0xeeddcc
           || g0 <> 0x48888888
           || g1 <> 0x48888888
           || g2 <> 0x48888888
           || h0 <> 0xaabbccdd_l
           || h1 <> 0xaabbccdd_l
           || h2 <> 0xaabbccdd_l
           || j0 <> 0xaabbccddeeff_L
           || j1 <> 0xaabbccddeeff_L
           || j2 <> 0xaabbccddeeff_L
           || k0 <> 0x0011aabbccddeeff_L
           || k1 <> 0x0011aabbccddeeff_L
           || k2 <> 0x0011aabbccddeeff_L
        then (
          eprintf "15_extract_int: match failed %b %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %ld %ld %ld %Ld %Ld %Ld %Ld %Ld %Ld\n"
            a b0 b1 b2 c0 c1 c2 d0 d1 d2 e0 e1 e2 f0 f1 f2 g0 g1 g2 h0 h1 h2 j0 j1 j2 k0 k1 k2;
          exit 1
        )
    | _ ->
        failwith "15_extract_int"
  done
