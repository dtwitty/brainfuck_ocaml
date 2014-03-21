brainfuck_ocaml
===============

A brainfuck interpreter and optimizing compiler

Usage: ./bf [-iO] <brainfuck file>

If -i is set, interprets the file directly otherwise compiles to out.c

Setting -O turns on optimizations

Optimizations inspired by http://nayuki.eigenstate.org/page/optimizing-brainfuck-compiler
