(* GIF header parser.
 * $Id: gif.ml 142 2008-07-17 15:45:56Z richard.wm.jones $
 *)

open Printf

let () =
  if Array.length Sys.argv <= 1 then
    failwith "usage: gif input.gif";
  let filename = Sys.argv.(1) in
  let bits = Bitstring.bitstring_of_file filename in

  match%bitstring bits with
  | ("GIF87a"|"GIF89a") [@l 6*8] [@string], (* GIF magic. *)
    width [@l 16] [@littleendian],
    height [@l 16] [@littleendian],
    colormap [@l 1],                  (* Has colormap? *)
    colorbits [@l 3],                 (* Color res = colorbits+1 *)
    sortflag [@l 1],
    bps [@l 3],                               (* Bits/pixel = bps+1 *)
    bg [@l 8],                                (* Background colour. *)
    aspectratio [@l 8] ->
      printf "%s: GIF image:\n" filename;
      printf "  size %d %d\n" width height;
      printf "  has global colormap? %b\n" colormap;
      printf "  colorbits %d\n" (colorbits+1);
      printf "  global colormap is sorted? %b\n" sortflag;
      printf "  bits/pixel %d\n" (bps+1);
      printf "  background color index %d\n" bg;
      printf "  aspect ratio %d\n" aspectratio

  | _ ->
      eprintf "%s: Not a GIF image\n" filename
