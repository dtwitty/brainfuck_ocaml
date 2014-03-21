{
	open Parser
}

rule token = parse
	  '>' { INCR_PTR_T }
	| '<' { DECR_PTR_T }
	| '+' { INCR_DATA_T }
	| '-' { DECR_DATA_T }
	| '.' { PUT_CHR_T }
	| ',' { GET_CHR_T }
	| '[' { OPEN_LOOP_T}
	| ']' { CLOSE_LOOP_T }
	| eof { EOF_T }
	| _   { token lexbuf}