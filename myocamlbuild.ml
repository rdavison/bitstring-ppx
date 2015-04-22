open Ocamlbuild_plugin

let () =
  dispatch begin function
  | Before_options ->
      Options.use_ocamlfind := true
  | After_rules ->
      flag_and_dep ["ocaml"; "link"; "bitstring_c"] (A"src/bitstring_c.o");
      dep ["c"; "endianness"; "compile"] ["src/byteswap.h"];
      flag [ "c"; "endianness"; "compile" ]
        (S[A"-ccopt"; A(Printf.sprintf "-DWORDS_BIGENDIAN=%d" (if Sys.big_endian then 1 else 0));
           A"-ccopt"; A"-I"; A"-ccopt"; A"src"])
  | _ ->
      ()
  end
