open Ocamlbuild_plugin

let () =
  dispatch begin function
  | Before_options ->
      Options.use_ocamlfind := true
  | After_rules ->
      flag_and_dep ["ocaml"; "compile"; "use_bitstring"] & S[A"-ppx"; P"src/ppx_bitstring.byte"]
  | _ ->
      ()
  end
