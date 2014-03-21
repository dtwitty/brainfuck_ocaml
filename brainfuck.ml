open ParseTree
open Printf

(* Parsing *)
let parse filename = 
	let ch = open_in filename in 
	let lexbuf = Lexing.from_channel ch in
	Parser.main Lexer.token lexbuf

(* Utility *)
let putchar c = 
	let () = print_char c in
	flush stdout;;

let print_tree tree = 
	let rec print_elem = function
		| INCR_DATA(offset, amt) -> printf "(+, %d, %d)" offset amt
		| INCR_PTR(amt) -> printf "(>, %d)" amt
		| PUT_CHR -> putchar '.'
		| GET_CHR -> putchar ','
		| LOOP(l) -> 
			let () = putchar '[' in
			let () = print_list l in
			putchar ']'
		| ASSIGN(offset, amt) -> printf "(=, %d, %d)" offset amt
		| ADD_MULT(offset, amt) -> printf "(*, %d, %d)" offset amt
	and print_list l = List.iter print_elem l in
	print_list tree

let make_char n = 
	let k = n mod 256 in
	let c = if k < 0 then 256 - k else k in
	char_of_int c

let sort_block l =
	(* Place 0 offset at beginning of block, keep others in order *)
	let f a b = 
		match a, b with
			| INCR_DATA(_, _), INCR_DATA(0, amt) -> 1
			| INCR_DATA(0, amt), INCR_DATA(_, _) -> -1
			| INCR_DATA(off1, _), INCR_DATA(off2, _) -> compare off1 off2
			| _, _ -> 0 in
	List.stable_sort f l

let shrink l = 
	(* Transform basic block into smallest form *)
	let rec loop curr_ptr tree =
		match tree with
			| head :: tail -> 
				begin
				match head with
					| INCR_DATA(offset, amt) ->
						INCR_DATA(curr_ptr + offset, amt) ::
						(loop curr_ptr tail)
					| INCR_PTR(amt) -> loop (curr_ptr + amt) tail
					| ASSIGN(offset, amt) -> 
						ASSIGN(curr_ptr + offset, amt) ::
						(loop curr_ptr tail)
					| _ -> failwith "not a basic block"
				end
			| _ -> if curr_ptr <> 0 then [ INCR_PTR(curr_ptr) ] else [] in
	sort_block(loop 0 l);;

let rec minimize tree = 
	(* Transform all basic blocs into smallest form *)
	let rec loop curr_run rest = 
		match rest with
			| head :: tail -> 
				begin
				match head with
					| GET_CHR | PUT_CHR | LOOP(_)
					| ADD_MULT(_,_) ->
						(shrink (List.rev curr_run)) @
						(head :: (loop [] tail))
					| _ -> loop (head :: curr_run) tail
				end
			| _ -> (shrink (List.rev curr_run)) in
	loop [] tree;;

let rec optimize_mult tree = 
	(* Transform looped adds into adding multiples *)
	let only_incrs = 
		List.for_all (function INCR_DATA(_,_) -> true | _ -> false) in
	let incr_to_mult = 
		let f = function
			INCR_DATA(offset, amt) -> ADD_MULT(offset, amt) | l -> l in
		List.map f in
	match tree with
		| LOOP(l) :: tail -> begin
			let ol = optimize_mult l in
			match ol with
				| INCR_DATA(0, -1) :: xs when only_incrs xs ->
					incr_to_mult(xs) @
					(ASSIGN(0, 0) :: optimize_mult tail)
				| _ -> LOOP(ol) :: optimize_mult tail
			end
		| head :: tail -> head :: optimize_mult tail
		| [] -> []

let rec optimize_assign tree = 
	(* Merge assigns and increments *)
	match tree with
		| ASSIGN(off1, _) :: ASSIGN(off2, amt) :: tail 
		| INCR_DATA(off1, _) :: ASSIGN(off2, amt) :: tail
		  when off1 = off2 -> ASSIGN(off1, amt) :: optimize_assign tail
		| ASSIGN(off1, amt1) :: INCR_DATA(off2, amt2) :: tail
		  when off1 = off2 -> ASSIGN(off1, amt1 + amt2) :: optimize_assign tail
		| head :: tail -> head :: optimize_assign tail
		| _ -> tree

let rec optimize_loop tree =
	(* Optimizes all tree levels, handles most basic loops *)
	match tree with
		| LOOP(l) :: tail -> begin
			let ol = optimize(l) in
			match ol with
				| [ INCR_DATA(0, v)] -> ASSIGN(0, 0) :: optimize_loop tail
				| _ -> LOOP(ol) :: optimize_loop tail
			end
		| head :: tail -> head :: optimize_loop tail
		| _ -> tree

and optimize tree = 
	(* Perform all optimizations until steady state *)
	let opt = tree in
	let opt = optimize_loop opt in
	let opt = minimize opt in
	let opt = optimize_assign opt in
	let opt = optimize_mult opt in
	if opt = tree then tree else optimize opt

let exec tree = 
	(* Interpret the tree *)
	let stack = Array.make 1000000 (make_char(0)) in 
	let ptr = ref 1000 in 
	let rec exec_stmt = function
			| INCR_PTR(amt) -> ptr := !ptr + amt
			| INCR_DATA(offset, amt) -> 
				let initial = int_of_char(stack.(!ptr + offset)) in
				let final = make_char(initial + amt) in
				stack.(!ptr + offset) <- final
			| ASSIGN(offset, amt) -> stack.(!ptr + offset) <- make_char(amt)
			| ADD_MULT(offset, amt) -> 
				let off = !ptr + offset in
				let old = int_of_char(stack.(off)) in
				let op = int_of_char(stack.(!ptr)) * amt in
				stack.(off) <- make_char (old + op)
			| PUT_CHR -> putchar stack.(!ptr)
			| GET_CHR -> begin
				try 
					stack.(!ptr) <- (input_char stdin)
				with End_of_file -> stack.(!ptr) <- make_char(0)
				end
			| LOOP(l) -> 
				while stack.(!ptr) <> make_char(0) do
					exec_list l
				done
	and exec_list l = List.iter exec_stmt l in
	exec_list tree

let print_c tree out =
	(* translate the tree to C and print to out channel *)
	let () = fprintf out "#include <stdio.h>\n" in
	let () = fprintf out "#include <string.h>\n" in
	let () = fprintf out "\n" in
	let () = fprintf out "char read() {\n" in
	let () = fprintf out "int temp = getchar();\n" in
	let () = fprintf out "return (char)(temp != EOF ? temp : 0);\n" in
	let () = fprintf out "}\n" in
	let () = fprintf out "\n" in
	let () = fprintf out "int main(int argc, char **argv) {\n" in
	let () = fprintf out "unsigned char stack[1000000];\n" in
	let () = fprintf out "unsigned char *p = &stack[1000];\n" in
	let () = fprintf out "memset(stack, 0, sizeof(stack));\n" in
	let () = fprintf out "\n" in
	let rec print_elem = function
		| INCR_PTR(amt) -> fprintf out "p += %d;\n" amt
		| INCR_DATA(offset, amt) -> fprintf out "p[%d] += %d;\n" offset amt
		| ASSIGN(offset, amt) -> fprintf out "p[%d] = %d;\n" offset amt
		| ADD_MULT(offset, amt) -> fprintf out "p[%d] += p[0] * %d;\n" offset amt
		| PUT_CHR -> fprintf out "putchar(p[0]);\n"
		| GET_CHR -> fprintf out "p[0] = read();\n"
		| LOOP(l) -> 
			let () = fprintf out "while(p[0]) {\n" in
			let () = print_list l in
			fprintf out "}\n"
	and print_list l = List.iter print_elem l in
	let () = print_list tree in
	fprintf out "}\n";;

let help () = 
	let () = printf "Brainfuck compiler/interpreter useage: " in
	let () = printf "cmd [-iO] <brainfuck file>\n" in
	let () = printf "if -i is set, interprets the file directly " in
	let () = printf "otherwise compiles to out.c\n" in
	let () = printf "setting -O turns on optimizations\n" in
	printf ""

let main () =
	let args = Sys.argv in
	let len = Array.length args in
	if len < 2 then
		help ()
	else
		let largs = List.rev(List.tl(Array.to_list args)) in
		match largs with
			| filename :: flags ->
				let tree = parse(filename) in
				let (inter, opti) = (List.mem "-i" flags, List.mem "-O" flags) in
				let prog = if opti then optimize tree else tree in
				if inter then exec prog else print_c prog (open_out "out.c")
			| _ -> failwith "but how?";;

main ();;