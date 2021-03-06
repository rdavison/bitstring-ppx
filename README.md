# WORK IN PROGRESS ! WORK IN PROGRESS !

> The `ppx_bitstring` project adds Erlang-style bitstrings and matching over
> bitstrings as a `ppx` syntax extension and library for OCaml.  It is directly
> based on a similar syntax extension for `camlp4` written by Richard
> W.M. Jones.

You can use this module to both parse and generate binary formats, files and
protocols.

Bitstring handling is added as primitives to the language, making it
exceptionally simple to use and very powerful.

Here is how to parse the header from a GIF image:

```ocaml
let bits = Bitstring.bitstring_of_file "image.gif" in
match%bitstring bits with
| ("GIF87a"|"GIF89a") [@l 6*8] [@string], (* GIF magic. *)
  width [@l 16] [@littleendian],
  height [@l 16] [@littleendian] ->
    printf "%s: GIF image is %d x %d pixels" filename width height
| _ ->
    eprintf "%s: Not a GIF image\n" filename
```

This example shows how to parse the first few fields in a Linux ELF binary (see
`<elf.h>`):

```ocaml
let bits = Bitstring.bitstring_of_file "/bin/ls" in
match%bitstring bits with
| 0x7f [@l 8], "ELF" [@l 24] [@string], (* ELF magic number *)
  e_ident [@l 12*8] [@bitstring],    (* ELF identifier *)
  e_type [@l 16] [@littleendian],    (* object file type *)
  e_machine [@l 16] [@littleendian]  (* architecture *)
  ->
    printf "This is an ELF binary, type %d, arch %d\n"
      e_type e_machine
```

And an IPv4 packet:

```ocaml
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
```

Bitstring handles integers, strings, sub-bitstrings, big-, little- and
native-endianness, signed and unsigned types, variable-width fields, fields with
arbitrary bit alignment.

# LICENSE

The library is licensed under the LGPL v2 or later, with the OCaml
linking exception.  See the file COPYING.LIB for full terms.

Programs are licensed under the GPL v2 or later.  See the file COPYING
for full terms.

All examples and tests are public domain.
