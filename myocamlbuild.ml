open Ocamlbuild_plugin

let () =
  dispatch begin function
  | Before_options ->
      Options.use_ocamlfind := true
  | After_rules ->
      rule
        "create test.bmpp"
        ~dep:"tools/create_test_pattern.byte"
        ~prod:"tests/test.bmpp"
        (fun _ _ -> Cmd (S[A"tools/create_test_pattern.byte"; A"tests/test.bmpp"]));
      mark_tag_used "use_test_bmpp";
      dep ["use_test_bmpp"] ["tests/test.bmpp"];
      flag_and_dep ["ocaml"; "compile"; "use_ppx_bitstring"] & S[A"-ppx"; P"src/ppx_bitstring.byte"]
  | _ ->
      ()
  end
