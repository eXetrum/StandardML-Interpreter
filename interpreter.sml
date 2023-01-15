datatype type_name     			= TypeName of string;
datatype type_func_body     	= TypeFuncBody of (string)list;
datatype type_func_type     	= TYPE_FUN | TYPE_IN_OUT
datatype type_key_value_list	= TypeKeyValueList of (string*string)list;
(* Function name; Function argument name; Function body; Function variables copy *)
datatype function     		= Function of type_func_type*type_name*type_name*type_func_body*type_key_value_list;

datatype cmditem =
	(* Part #1 *)
	NUMBER of int
	| NAME of string
	| STR of string
	| BOOL of bool
	| PUSH of string
	| ERROR
	| POP
	| ADD
	| SUB
	| MUL
	| DIV
	| REM
	| NEG
	| SWAP
	| QUIT
	(* Part #2 *)
	| AND
	| OR
	| NOT
	| EQUAL
	| LESS_THAN
	| BIND
	| UNIT
	| IF
	| LET
	| END
	(* Part #3 *)
	| FUN of string
	| IN_OUT_FUN of string
	| FUN_END
	| CALL
	| RETURN
	| FUNCTION of function
	;

(* key, value pair datatype *)
datatype key_value_pair = KeyValue of cmditem*cmditem*(key_value_pair)list;
datatype variables = Variables of (key_value_pair)list;

(* Join list of strings into single string separated by newline *)
fun Concat (nil, glue) = "" |
	Concat (xs, glue) = 
		foldl (fn (x,acc) => acc  ^ glue ^ x) (hd xs) (tl xs);
(* Return true if string contains all characters of numbers *)
fun IsNumeric(s) = let
	val s = if #"-"=hd(String.explode(s)) andalso String.size s > 1 then String.implode(tl(String.explode(s)))
		else s
in (List.all Char.isDigit o String.explode) ( s )
end;

(* Return true if string can be used as variable name *)
fun IsName(s) = (List.all Char.isAlphaNum o String.explode)(s) andalso Char.isAlpha (hd(String.explode s));
(* Return true if string contains string literal (string in quotes " ") *)
fun IsStringLiteral(s) = ((String.isPrefix "\"" s) andalso (String.isSuffix "\"" s));
(* Return true if string contains boolean string *)
fun IsBool(s) = (s = ":true:" orelse s= ":false:");
	
(* Convert string to command datatype *)
fun StrToCmd(s : string) = 
	if String.isPrefix "fun " s then FUN(Concat(tl(String.tokens Char.isSpace s), " "))
	else if String.isPrefix "inOutFun " s then IN_OUT_FUN(Concat(tl(String.tokens Char.isSpace s), " "))
	else if s="funEnd" then FUN_END
	else if s="call" then CALL
	else if String.isPrefix "push" s then PUSH(Concat(tl(String.tokens Char.isSpace s), " "))
	else if s="pop" then POP
	else if s=":true:" then BOOL(true)
	else if s=":false:" then BOOL(false)
	else if s=":error:" then ERROR
	else if s="add" then ADD
	else if s="sub" then SUB
	else if s="mul" then MUL
	else if s="div" then DIV
	else if s="rem" then REM
	else if s="neg" then NEG
	else if s="swap" then SWAP
	else if s="quit" then QUIT	
	else if s="and" then AND
	else if s="or" then OR
	else if s="not" then NOT
	else if s="equal" then EQUAL
	else if s="lessThan" then LESS_THAN
	else if s="bind" then BIND
	else if s=":unit:" then UNIT
	else if s="if" then IF
	else if s="let" then LET
	else if s="end" then END
	else if s="call" then CALL
	else if s="return" then RETURN
	else if IsNumeric(s) then NUMBER(valOf( Int.fromString s)) 
	else if IsStringLiteral(s) then STR(s)
	else if IsName(s) then NAME(s)
	else ERROR
	;
	
(* Convert command datatype to string *)
fun CmdToStr(i) = 
	case i of 
		NUMBER(i) => if (i >= 0) then Int.toString(i) else "-"^Int.toString(~i)
		| STR(i) => let 
			val s = tl(String.explode i)
			in String.implode(List.take(s, length s - 1))
		end 
		| NAME(i) => i
		| BOOL(i) => if i then ":true:" else ":false:"
		| ERROR => ":error:"
		| PUSH(i) => "push" ^ " " ^ i
		| POP => "pop"
		| ADD => "add"
		| SUB => "sub"
		| MUL => "mul"
		| DIV => "div"
		| REM => "rem"
		| NEG => "neg"
		| SWAP => "swap"
		| QUIT => "quit"
		| AND => "and"
		| OR => "or"
		| NOT => "not"
		| EQUAL => "equal"
		| LESS_THAN => "lessThan"
		| BIND => "bind"
		| UNIT => ":unit:"
		| IF => "if"
		| LET => "let"
		| END => "end"
		| FUN(i) => i
		| FUN_END => "funEnd"
		| CALL => "call"
		| FUNCTION(f) => let
			val Function(ftype, TypeName fname, TypeName farg, fbody, fclone) = f
		in fname
		end
		| RETURN => "return"
	;
		

fun Read(input : string) = let
	(* Read all strings from input file and convert them to "stack item" *)
	val fdInput = TextIO.openIn input
    val cmd = map StrToCmd (String.tokens (fn c => c = #"\n") (TextIO.inputAll fdInput))
    val _ = TextIO.closeIn fdInput
in cmd
end;

fun Write(output : string, st : (cmditem)list ) = let
	(* Gather all items on the stack and print to the output file *)
	val fdOutput = TextIO.openOut output
	val _ = TextIO.output (fdOutput, Concat( map CmdToStr st, "\n") )
	val _ = TextIO.closeOut fdOutput
in ()
end;

fun update([], NAME name, NUMBER num) = [(NAME name, NUMBER num)] @ []
| update([], NAME name, STR s) = [(NAME name, STR s)] @ []
| update([], NAME name, BOOL b) = [(NAME name, BOOL b)] @ []
| update([], NAME name, UNIT) = [(NAME name, UNIT)] @ []
| update([], NAME name, FUNCTION f) = [(NAME name, FUNCTION f)] @ []
| update( ( NAME key, value )::xs, NAME name, data) = (if key=name then [(NAME name, data)] @ xs else [(NAME key, value)] @ update(xs, NAME name, data) )
;

fun lookup([], value) = value
| lookup((NAME(key), value)::xs, NAME name) = (if key=name then value else lookup(xs, NAME name))
| lookup(x::xs, NUMBER num) = NUMBER num
| lookup(x::xs, STR s) = STR s
| lookup(x::xs, BOOL b) = BOOL b
| lookup(x::xs, UNIT) = UNIT
| lookup(x::xs, _) = ERROR;


fun dumpVals([]) = ""
| dumpVals( ( NAME key, value )::xs ) = let
	val _ = print("Key: " ^ key ^ " | value: " ^ CmdToStr(value) ^ "\n")
	in dumpVals(xs)
end

fun converToKeyValue([]) = []
| converToKeyValue(x::xs) = let
	val (key, value) = x
in [(CmdToStr key, CmdToStr value)]@ converToKeyValue(xs)
end

fun convertToVars([]) = []
| convertToVars(x::xs) = let
	val (key, value) = x
in [(NAME key, StrToCmd value)] @ convertToVars(xs)
end


fun mergeVars([], vars) = []
| mergeVars(x::xs, vars) = let
	val (key, value) = x
	val ret = (fn(FUNCTION(value)) => FUNCTION(value)
		| (value) => value) ( lookup(vars, value) )	
in [(key, ret)] @ xs
end

fun execute(commands, cmd, st, vars) = 
let
in
	case cmd of
		PUSH(i) => let
			val value = if IsNumeric(i) then StrToCmd(i)
				else if IsName(i) then NAME(i) 
				else if IsStringLiteral(i) then STR(i)
				else ERROR
		in (commands, [value] @ st, vars)
		end
		| POP => (commands, (if length st <> 0 then tl(st) else [ERROR] @ st), vars)
		| BOOL(true) => (commands, [BOOL(true)] @ st, vars)
		| BOOL(false) => (commands, [BOOL(false)] @ st, vars)
		| ERROR => (commands, [ERROR] @ st, vars)
		| ADD => let
			val st = if length st < 2 then [ERROR] @ st else let 
				val y = hd(st)
				val x = hd(tl(st))
				val st = tl(tl(st))
				val (x_val, y_val) = ( lookup(vars, x), lookup(vars, y) )
				val z = (fn (NUMBER(x), NUMBER(y)) => NUMBER(y + x) | (_, _) => ERROR) (y_val, x_val)				
			in  (fn (NUMBER(n)) => [NUMBER(n)] @ st | (_) => [ERROR] @ [y] @ [x] @ st) (z)
			end
		in (commands, st, vars)
		end
		| SUB => let
			val st = if length st < 2 then [ERROR] @ st else let 
				val y = hd(st)
				val x = hd(tl(st))
				val st = tl(tl(st))
				val (x_val, y_val) = ( lookup(vars, x), lookup(vars, y) )
				val z = (fn (NUMBER(x), NUMBER(y)) => NUMBER(y - x) | (_, _) => ERROR) (y_val, x_val)				
			in  (fn (NUMBER(n)) => [NUMBER(n)] @ st | (_) => [ERROR] @ [y] @ [x] @ st) (z)
			end
		in (commands, st, vars)
		end
		| MUL => let
			val st = if length st < 2 then [ERROR] @ st else let 
				val y = hd(st)
				val x = hd(tl(st))
				val st = tl(tl(st))
				val (x_val, y_val) = ( lookup(vars, x), lookup(vars, y) )
				val z = (fn (NUMBER(x), NUMBER(y)) => NUMBER(y * x) | (_, _) => ERROR) (y_val, x_val)
			in  (fn (NUMBER(n)) => [NUMBER(n)] @ st | (_) => [ERROR] @ [y] @ [x] @ st) (z)
			end
		in (commands, st, vars)
		end
		| DIV => let
			val st = if length st < 2 then [ERROR] @ st else let 
				val y = hd(st)
				val x = hd(tl(st))
				val st = tl(tl(st))
				val (x_val, y_val) = ( lookup(vars, x), lookup(vars, y) )
				val z = (fn (NUMBER(x), NUMBER(y)) => (if 0=x then ERROR else NUMBER( y div x )) | (_, _) => ERROR) (y_val, x_val)
			in  (fn (NUMBER(n)) => [NUMBER(n)] @ st | (_) => [ERROR] @ [y] @ [x] @ st) (z)
			end
		in (commands, st, vars)
		end
		| REM => let
			val st = if length st < 2 then [ERROR] @ st else let 
				val y = hd(st)
				val x = hd(tl(st))
				val st = tl(tl(st))
				val (x_val, y_val) = ( lookup(vars, x), lookup(vars, y) )
				val z = (fn (NUMBER(x), NUMBER(y)) => (if 0=x then ERROR else NUMBER( y mod x )) | (_, _) => ERROR) (y_val, x_val)
			in  (fn (NUMBER(n)) => [NUMBER(n)] @ st | (_) => [ERROR] @ [y] @ [x] @ st) (z)
			end
		in (commands, st, vars)
		end
		| NEG => let
			val st = if length st < 1 then [ERROR] @ st else let 
				val x = hd(st)
				val st = tl(st)
				val x_val = lookup(vars, x)
				val z = (fn (NUMBER(x)) => NUMBER(~x) | (_) => ERROR) (x_val)
			in  (fn (NUMBER(n)) => [NUMBER(n)] @ st | (_) => [ERROR] @ [x] @ st) (z)
			end
		in (commands, st, vars)
		end
		| SWAP => (commands, if length st < 2 then [ERROR] @ st else let
			val x = hd(st)
			val y = hd(tl(st))
			in [y] @ [x] @ tl(tl(st))
		end, vars )		
		| QUIT => ([], st, vars)
		| AND => let
			val st = if length st < 2 then [ERROR] @ st else let 
				val y = hd(st)
				val x = hd(tl(st))
				val st = tl(tl(st))	
				val (x_val, y_val) = ( lookup(vars, x), lookup(vars, y) )				
				val z = (fn (BOOL(x), BOOL(y)) => BOOL( y andalso x ) | (_, _) => ERROR) (y_val, x_val)
			in  (fn (BOOL(b)) => [BOOL(b)] @ st | (_) => [ERROR] @ [y] @ [x] @ st) (z)
			end
		in (commands, st, vars)
		end
		| OR => let
			val st = if length st < 2 then [ERROR] @ st else let 
				val y = hd(st)
				val x = hd(tl(st))
				val st = tl(tl(st))	
				val (x_val, y_val) = ( lookup(vars, x), lookup(vars, y) )				
				val z = (fn (BOOL(x), BOOL(y)) => BOOL( y orelse x ) | (_, _) => ERROR) (y_val, x_val)
			in  (fn (BOOL(b)) => [BOOL(b)] @ st | (_) => [ERROR] @ [y] @ [x] @ st) (z)
			end
		in (commands, st, vars)
		end
		| NOT => let
			val st = if length st < 1 then [ERROR] @ st else let 
				val x = hd(st)
				val st = tl(st)		
				val x_val = lookup(vars, x)
				val z = (fn (BOOL(x)) => BOOL( not x ) | (_) => ERROR) (x_val)
			in  (fn (BOOL(b)) => [BOOL(b)] @ st | (_) => [ERROR] @ [x] @ st) (z)
			end
		in (commands, st, vars)
		end
		| EQUAL => let
			val st = if length st < 2 then [ERROR] @ st else let 
				val y = hd(st)
				val x = hd(tl(st))
				val st = tl(tl(st))	
				val (x_val, y_val) = ( lookup(vars, x), lookup(vars, y) )				
				val z = (fn (NUMBER(x), NUMBER(y)) => BOOL( y = x ) | (_, _) => ERROR) (y_val, x_val)
			in  (fn (BOOL(b)) => [BOOL(b)] @ st | (_) => [ERROR] @ [y] @ [x] @ st) (z)
			end
		in (commands, st, vars)
		end		
		| LESS_THAN => let
			val st = if length st < 2 then [ERROR] @ st else let 
				val y = hd(st)
				val x = hd(tl(st))
				val st = tl(tl(st))		
				val (x_val, y_val) = ( lookup(vars, x), lookup(vars, y) )
				val z = (fn (NUMBER(x), NUMBER(y)) => BOOL( y < x ) | (_, _) => ERROR) (y_val, x_val)
			in  (fn (BOOL(b)) => [BOOL(b)] @ st | (_) => [ERROR] @ [y] @ [x] @ st) (z)
			end
		in (commands, st, vars)
		end
		| BIND => let
			val (st, vars) = if length st < 2 then ([ERROR] @ st, vars) else let 
				(* Value *)
				val y = hd(st)
				(* Name *)
				val x = hd(tl(st))
				(* Remove from stack first two items *)
				val st = tl(tl(st))				
				
				val (z, vars) = (fn (NAME(x), NUMBER(y)) => (UNIT, update(vars, NAME x, NUMBER y) ) 
					| (NAME(x), STR(y)) => (UNIT, update(vars, NAME x, STR y) )
					| (NAME(x), BOOL(y)) => (UNIT, update(vars, NAME x, BOOL y) )
					| (NAME(x), UNIT) => (UNIT, update(vars, NAME x, UNIT) )
					| (NAME(x), NAME(y)) =>  
						( fn (NAME i) => (ERROR , vars)
							| (NUMBER i) => (UNIT, update(vars, NAME x, NUMBER i) )
							| (STR i) => (UNIT, update(vars, NAME x, STR i) )
							| (BOOL i) => (UNIT, update(vars, NAME x, BOOL i) )
							| (UNIT) => (UNIT, update(vars, NAME x, UNIT) ) 
							| (_) => (ERROR, vars) 
						)  ( lookup(vars, NAME y) )
					| (_, _) => (ERROR, vars)) (x, y)				
					
			in  ( (fn (UNIT) => [UNIT] @ st | (_) => [ERROR] @ [y] @ [x] @ st) (z) , vars )
			end
		in (commands, st, vars)
		end 
		| IF => let
			val st = if length st < 3 then [ERROR] @ st else let 
				val x = hd(st)
				val y = hd(tl(st))
				val z = hd( tl(tl(st)) )
				val st = tl(tl(tl(st)))		
				val z_val = lookup(vars, z)
				
			in  (fn (BOOL(z), x, y) => [(if z=true then x else y )] @ st 
					| (_, x, y) => [ERROR] @ [z] @ [y] @ [x]) (z_val, x, y)
			end
		in (commands, st, vars)
		end
		| LET => let
			(* Copy current stack items into inner stack *)
			val st_let = st
			val vars_let = vars
			val (commands, st_let, vars_let) = let 
				val rec execLet = ( fn (x, st_let, vars_let, END) => (x, st_let, vars_let)
					| ((x::xs), st_let, vars_let, _) => let
						(* Execute next command *)
						val (cmd, st_let, vars_let) = if x<>END then execute(xs, x, st_let, vars_let) else (xs, st_let, vars_let)
					in execLet(cmd, st_let, vars_let, x)
					end
				)				
			in execLet(commands, st_let, vars_let, hd(commands) )
			end	
		
		in (commands, [if length st_let <> 0 then lookup(vars_let, hd(st_let)) else ERROR] @ st, vars)
		end
		| FUN(declar) => let			
			val declaration = (String.tokens Char.isSpace) (declar)
			val name = hd declaration						(* Function name *)	
			val arg  = hd(tl(declaration))					(* Function argument name *)

			(* Function body *)
			val rec reader = (fn (cmd, x, FUN_END) => cmd
				| (cmd, (x::xs), _) => ( if x <> FUN_END then [CmdToStr(x)] @ reader(cmd, xs, x) else [CmdToStr(x)] @ cmd)
			)
			val body = reader([], commands, hd(commands))
			(* Drop function body from command list *)
			val commands = List.drop(commands, length body) 
			val clone = TypeKeyValueList (converToKeyValue vars)
			val f = Function(TYPE_FUN, TypeName name, TypeName arg, TypeFuncBody body, clone)

			val vars = update(vars, NAME name, FUNCTION f)
			val value = UNIT
			
		in (commands, [value] @ st, vars)
		end	
		| IN_OUT_FUN(declar) => let
			val declaration = (String.tokens Char.isSpace) (declar)
			val name = hd declaration						(* Function name *)	
			val arg  = hd(tl(declaration))					(* Function argument name *)

			(* Function body *)
			val rec reader = (fn (cmd, x, FUN_END) => cmd
				| (cmd, (x::xs), _) => ( if x <> FUN_END then [CmdToStr(x)] @ reader(cmd, xs, x) else [CmdToStr(x)] @ cmd)
			)
			val body = reader([], commands, hd(commands))
			(* Drop function body from command list *)
			val commands = List.drop(commands, length body) 			
			val clone = TypeKeyValueList (converToKeyValue vars)			
			val f = Function(TYPE_IN_OUT, TypeName name, TypeName arg, TypeFuncBody body, clone)			
			
			val vars = update(vars, NAME name, FUNCTION f)
			val value = UNIT
			
		in (commands, [value] @ st, vars)
		end
		| CALL => let
			val (st, vars) = if length st < 2 then ([ERROR] @ st, vars) else let 
				val y = hd(st)
				val x = hd(tl(st))
				val st = tl(tl(st))		
				val (x_val, y_val) = ( lookup(vars, x), lookup(vars, y) )
				
				val y_val = (fn (FUNCTION(x)) => FUNCTION(x) | (_) => ERROR) (y_val)
				val x_val = (fn (NAME(x)) => ERROR | (_) => x_val ) (x_val)
				
				val (st, vars) = if y_val=ERROR orelse x_val=ERROR then let
					in ([ERROR] @ [y] @ [x] @ st, vars) 
				end
				else let
					val FUNCTION(Function(ftype, TypeName fname, TypeName farg, TypeFuncBody fbody, fvars)) = y_val
					val TypeKeyValueList(temp) = fvars					
					(* Refresh functions from parent env *)
					val func_vars = mergeVars(convertToVars(temp), vars)					
					
					val func_body = map StrToCmd fbody
							
					val func_vars = update(func_vars, NAME farg, x_val)
					val func_vars = update(func_vars, NAME fname, y_val)
	
	
					val (ret, func_st, func_vars) = let 
						val ret = false
						val rec execFunc = ( fn (x, st_fun, vars_fun, FUN_END) => (false, st_fun, vars_fun)
						| (x, st_fun, vars_fun, RETURN) => (true, st_fun, vars_fun)
						| ((x::xs), st_fun, vars_fun, _) => let
							(* Next command*)
							val (cmd, st_fun, vars_fun) = if x<>RETURN then execute(xs, x, st_fun, vars_fun) else (xs, st_fun, vars_fun)
							in execFunc(cmd, st_fun, vars_fun, x)
						end
						)
						
					in execFunc(func_body, [], func_vars, hd(func_body))
					end (* Execute function body *)
					
					val (st, vars) = let
						(* Top frame *)
						val st = if ret then [ if length func_st <> 0 then lookup(func_vars, hd(func_st)) else ERROR  ] @ st else st						
						(* Update variables if type of function is IN_OUT *)
						val vars = let 
							val ret_value = lookup(func_vars, NAME farg)
							val vars = if ftype=TYPE_IN_OUT then update(vars, x, ret_value) else vars
						in vars
						end
					in (st, vars)
					end (* Return from function *)
					
				in (st, vars)
				end		
				
			in  (st, vars)
			end
		in (commands, st, vars)
		end		
		| _ => (commands, [ERROR] @ st, vars)
end;


fun process(nil, st, vars) = (st, vars)
| process(x::xs, st, vars) = let
	val (xs, st, vars) = execute(xs, x, st, vars)
	in process(xs, st, vars)
end;

fun interpreter(input : string, output : string) = let
	val cmd = Read(input)
	(* Execute commands starting with empty stack *)	
	val (st, vars) = process(cmd, [], [])
	(* Save result *)
	in Write(output, st)
end;