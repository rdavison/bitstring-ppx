#### master

- Replace `camlp4` by `ppx`
- Update `tests/`, `examples/`, `cli-tools/`, `benchmarks/` to `ppx`
- Replace `autoconf` build system with `ocamlbuild`
- Remove `Bitstring_config` module
- Replace C bits by `ocplib-endian`
- Use `-safe-string`
- Remove `Bitstring.{set, put, clear}` (as these is incomptible with using
  `-safe-string`)

#### 22 Apr 2015 Cambridge

- Initial import of `ocaml-bistring` 2.0.4
