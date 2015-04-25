# Bitstring syntax extension.
# Copyright (C) 2008 Red Hat Inc., Richard W.M. Jones
# Copyright (C) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
#
# $Id: Makefile.in 197 2012-08-10 11:52:44Z richard.wm.jones@gmail.com $

OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind
PREFIX = $(shell opam config var prefix)

bitstring:
	$(OCAMLBUILD) src/bitstring.cma src/bitstring.cmxa

ppx_bitstring:
	$(OCAMLBUILD) src/ppx_bitstring.byte src/ppx_bitstring.native

%.byte %.native %.otarget:
	$(OCAMLBUILD) $@

# Tests

check: test

test: tests/tests.otarget
	@for f in $(shell cat tests/tests.itarget); do \
	  echo Running $$f; \
	  ./$$f; \
	done
#	@for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

# Examples.

examples: examples/examples.otarget
#	@for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

# Install.

install: uninstall
	opam-installer --prefix=$(PREFIX) opam.install

uninstall:
	opam-installer --prefix=$(PREFIX) -u opam.install

clean:
	$(OCAMLBUILD) -clean

doc:
	$(OCAMLBUILD) -docflags -colorize-code,-css-style,style.css,-sort doc/api.docdir/index.html
	cp doc/style.css api.docdir/

gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp api.docdir/* .gh-pages/
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

.PHONY: doc clean install uninstall
