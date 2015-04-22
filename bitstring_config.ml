(* Bitstring library.
 * Copyright (C) 2008 Red Hat Inc., Richard W.M. Jones
 *
 * bitstring_config.ml.  Generated from bitstring_config.ml.in by configure.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version,
 * with the OCaml linking exception described in COPYING.LIB.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 * $Id: bitstring_config.ml.in 163 2008-08-27 12:54:57Z richard.wm.jones $
 *)

(* This file contains general configuration settings, set by the
 * configure script.
 *)

let nativeendian = Bitstring_types.LittleEndian

let package = "ocaml-bitstring"
let version = "2.0.4"
let ocamllibdir = "/Users/nojb/.opam/4.02.1/lib/ocaml"

let diff = "/usr/bin/diff"
