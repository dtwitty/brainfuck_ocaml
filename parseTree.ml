type stmt = 
	| INCR_PTR of int
	| INCR_DATA of int * int
	| ASSIGN of int * int
	| ADD_MULT of int * int
	| PUT_CHR
	| GET_CHR
	| LOOP of stmt list