
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Stack;
import java.util.regex.Pattern;





public class interpreter {
	
	public static class Environment {
		
		public Environment() {
			stack = new Stack<>();
			variables = new HashMap();
		}
		public Stack<String> stack;
		public HashMap variables;
	}
	
	public static enum FunctionType
	{
		FUN,
		IN_OUT
	}
	
	static class FunctionObject {
		
		private FunctionType type;
		private String name;
		private String arg;		
		private Stack<String> cmd;
		private HashMap<String, Object> variables;
		
		private Stack<String> currentCommandStack;
		
		public FunctionObject(FunctionType type, String name, String arg, Stack<String> cmd) {
			this.type = type;
			this.name = name;
			this.arg = arg;
			this.cmd = cmd;
			this.variables = new HashMap<>();
		}
		
		public void addVariable(String name, String value) { variables.put(name, value); }
		public Object getVariable(String name) { return variables.get(name);}
		public void removeVariable(String name) { variables.remove(name); }
		
		public String getName() { return name; }
		public String getArgument() { return arg; }
		public Stack<String> getCommands() { return (Stack<String>)cmd.clone(); }
		public FunctionType getFuncType() { return type; }
		
		public void startExecution() {
			currentCommandStack = (Stack<String>)cmd.clone();
		}
		
		public boolean hasNextCommand() { return !currentCommandStack.isEmpty(); }
		
		public String getNextCommand() { return currentCommandStack.pop(); }
	}
	
	static boolean quitOccured = false;
	
	static FunctionObject parseFunction(Stack<String> cmd) {
		String declaration = cmd.pop();
		String[] chunks = declaration.split(" ");
		FunctionType ftype = chunks[0].equals("fun") ? FunctionType.FUN : FunctionType.IN_OUT;
		String name = chunks[1];
		String arg = chunks[2];
		
		
		Stack<String> fbody = new Stack<>();
		while(!cmd.isEmpty() ) {
			String line = cmd.pop();
			if(line.equals("funEnd")) break;
			fbody.push(line);
		}
		Collections.reverse(fbody);
		
		return new FunctionObject(ftype, name, arg, fbody);
	}
	static boolean IsBool(String s) { return s.equals(":true:") || s.equals(":false:"); }
	static boolean StrToBool(String s) { return s.equals(":true:"); }
	static String BoolToStr(boolean b) { return b ? ":true:" : ":false:"; }
	
	public static Environment execCommands(Stack<String> cmd, Environment env, PrintStream outputStream) {
		while(!cmd.isEmpty()) {

			String line = cmd.pop();
		
			if (line.equals("quit")) Quit(env, outputStream);
			else if(IsBool(line) || line.equals(":error:")) {
				env = booleanOrError(line, env);
			} 
			else if(line.equals("let")) {
				env = Let(env, cmd, outputStream);
			}
			else if(line.equals("end")) {				
				break;
			}
			else if(line.startsWith("fun") || line.startsWith("inOutFun")) {
				cmd.push(line);
				env = Func(env, cmd);
			}
			else if(line.equals("call")) {
				env = Call(env, outputStream);
			} 
			else if(line.equals("return")) {
				cmd.push("return");
				break;
			}
			else {
				env = simpleCommand(line, env, outputStream);
			}
		}
		return env;
	}
	
	public static Environment Func(Environment env, Stack<String> cmd) {
		FunctionObject f = parseFunction(cmd);
		env.variables.put(f.getName(), f);
		f.variables = (HashMap) env.variables.clone();
		env.stack.push(":unit:");
		return env;
	}
	
	public static Environment Call(Environment env, PrintStream outputStream) {
		
		if(env.stack.size() < 2) {
			env.stack.push(":error:");
			return env;
		}
		
		String name = env.stack.pop();
		String arg  = env.stack.pop();
		
		if(name.equals(":error:") || arg.equals(":error:")) {
			env.stack.push(arg);
			env.stack.push(name);
			env.stack.push(":error:");
			return env;
		}
		
		try {
			Object fobj = env.variables.get(name);
			
			if(fobj == null || !(fobj instanceof FunctionObject)) {
				env.stack.push(arg);
				env.stack.push(name);
				env.stack.push(":error:");
				return env;
			}
			
			FunctionObject f = (FunctionObject)fobj;
			
			Environment env_func = new Environment();
			env_func.variables = (HashMap<String, Object>) f.variables.clone();			
			
			Object argValue = arg;
			// Argument is variable name
			if(arg.matches("^[a-zA-Z]+[0-9]?")) {
				Object arg_var_val = env.variables.get(arg);
				if(arg_var_val == null || (arg_var_val instanceof String && ((String)arg_var_val).matches("^[a-zA-Z]+[0-9]?")) ) {
					env.stack.push(arg);
					env.stack.push(name);
					env.stack.push(":error:");
					return env;
				}
				argValue = arg_var_val;
			}
			
	
			env_func.variables.put((String)f.getArgument(), argValue);
			
			// Execute			
			boolean ret = false;
			Stack<String> cmd = (Stack<String>)f.getCommands();
			while(!cmd.isEmpty()) {
				String line = cmd.pop();
				if(line .equals("return")) {
					ret = true;
					break;
				} else {
					cmd.push(line);
					env_func = execCommands(cmd, env_func, outputStream);
				}
			}
			if(!env_func.stack.empty()) {
				
				String frame = env_func.stack.pop();
				Object frame_value = frame;
				if(frame.matches("^[a-zA-Z]+[0-9]?")) {
					frame_value = env_func.variables.get(frame);
					if(frame_value == null) frame_value = frame;
				}

				if(ret) {
					if(frame_value instanceof String) {
						env.stack.push((String)frame_value);
					} else {
						env.stack.push( ((FunctionObject)frame_value).getName());
					}
				}
				
				if(f.getFuncType() == FunctionType.IN_OUT) {
					env.variables.put((String)arg, env_func.variables.get(f.getArgument()));
				}				
				
			}
			
			
			
		} catch(Exception e) {
			env.stack.push(arg);
			env.stack.push(name);
			env.stack.push(":error:");
		}
		
		
		return env;
	}
	
	public static void interpreter(String input, String output) throws IOException{
		
		PrintStream outputStream=new PrintStream(new File(output));
		PrintStream console = System.out;
		System.setOut(outputStream);
		
		BufferedReader in = new BufferedReader(new FileReader(input));
		Environment env = new Environment();
		
		String line = in.readLine();
		Stack<String> cmd = new Stack<>();
		while(line!=null){
			
			cmd.add(0, line.trim());
			line = in.readLine();
		} 
		
		env = execCommands(cmd, env, outputStream);
		
		if(quitOccured == false) {
			Quit(env, outputStream);
		}
		in.close();
		System.setOut(console);
	}
	
	public static Environment simpleCommand(String line, Environment env, PrintStream myconsole){
		if (line.equals("add"))       		env = Add(env);
		else if (line.equals("sub"))  		env = Sub(env);
		else if (line.equals("mul"))  		env = Mul(env);
		else if (line.equals("div"))  		env = Div(env);
		else if (line.equals("rem"))  		env = Rem(env);
		else if (line.equals("pop"))  		env = Pop(env);
		else if (line.startsWith("push")) 	env = Push(env, line);
		else if (line.equals("swap")) 		env = Swap(env);
		else if (line.equals("neg"))  		env = Neg(env);
		else if (line.equals("and"))  		env = And(env);
		else if (line.equals("or"))  		env = Or(env);
		else if (line.equals("not"))  		env = Not(env);
		else if (line.equals("equal"))  	env = Equal(env);
		else if (line.equals("lessThan"))  	env = LessThan(env);
		else if (line.equals("bind"))  		env = Bind(env);
		else if (line.equals("if"))  		env = If(env);		
		else 								env.stack.push(":error:");
		
		return env;
	}
	
	
	
	public static Environment booleanOrError(String line, Environment env) {
		if (line.equals(":error:"))         env.stack.push(":error:");
		else if (line.equals(":true:"))     env.stack.push(":true:");
		else if (line.equals(":false:")) 	env.stack.push(":false:");
		else 								env.stack.push(":error:");
		
		return env;
	}
	
	public static Environment Push(Environment env, String line) {

		String arg = line.substring(5);
		// Number
		if (arg.matches("[+-]?[0-9]+")){
			env.stack.push(arg);
		}
		// Variable name
		else if (arg.matches("^[a-zA-Z]+[0-9]?")){
			env.stack.push(arg);
		}
		// String literal
		else if (arg.matches("^\".+\"$")){
			env.stack.push(arg);
		}
		else{
			env.stack.push(":error:");
		}
		return env;
	}

	public static Environment Pop(Environment env) {
		if (env.stack.size() < 1){
			env.stack.push(":error:");
		}
		else{
			env.stack.pop();
		}
		return env;
	}
	
	public static Environment Add(Environment env) {
		if (env.stack.size() < 2){
			env.stack.push(":error:");
			return env;
		}
		
		String y = env.stack.pop();
		String x = env.stack.pop();
		
		int x_val, y_val;
		if(y.matches("[+-]?[0-9]+")) x_val = Integer.parseInt(y);
		else {
			
			String y_var_val = (String) env.variables.get(y);
			if(y_var_val == null || !y_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			x_val = Integer.parseInt(y_var_val);
		}
		if(x.matches("[+-]?[0-9]+")) y_val = Integer.parseInt(x);
		else {
			String x_var_val = (String) env.variables.get(x);
			if(x_var_val == null || !x_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			y_val = Integer.parseInt(x_var_val);
		}
		
		
		Integer z = y_val + x_val;
		env.stack.push(z.toString());
		
		
		return env;
	}
	
	public static Environment Sub(Environment env) {
		if (env.stack.size() < 2){
			env.stack.push(":error:");
			return env;
		}
		
		String y = env.stack.pop();
		String x = env.stack.pop();
		
		int x_val, y_val;
		if(y.matches("[+-]?[0-9]+")) x_val = Integer.parseInt(y);
		else {
			
			String y_var_val = (String) env.variables.get(y);
			if(y_var_val == null || !y_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			x_val = Integer.parseInt(y_var_val);
		}
		if(x.matches("[+-]?[0-9]+")) y_val = Integer.parseInt(x);
		else {
			String x_var_val = (String) env.variables.get(x);
			if(x_var_val == null || !x_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			y_val = Integer.parseInt(x_var_val);
		}
		
		
		Integer z = y_val - x_val;
		env.stack.push(z.toString());
		
		
		return env;
	}

	public static Environment Mul(Environment env) {
		if (env.stack.size() < 2){
			env.stack.push(":error:");
			return env;
		}
		
		String y = env.stack.pop();
		String x = env.stack.pop();
		
		int x_val, y_val;
		if(y.matches("[+-]?[0-9]+")) x_val = Integer.parseInt(y);
		else {
			
			String y_var_val = (String) env.variables.get(y);
			if(y_var_val == null || !y_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			x_val = Integer.parseInt(y_var_val);
		}
		if(x.matches("[+-]?[0-9]+")) y_val = Integer.parseInt(x);
		else {
			String x_var_val = (String) env.variables.get(x);
			if(x_var_val == null || !x_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			y_val = Integer.parseInt(x_var_val);
		}
		
		
		Integer z = y_val * x_val;
		env.stack.push(z.toString());
		
		
		return env;
	}

	
	public static Environment Div(Environment env) {
		if (env.stack.size() < 2){
			env.stack.push(":error:");
			return env;
		}
		
		String y = env.stack.pop();
		String x = env.stack.pop();
		
		int x_val, y_val;
		if(y.matches("[+-]?[0-9]+")) x_val = Integer.parseInt(y);
		else {
			
			String y_var_val = (String) env.variables.get(y);
			if(y_var_val == null || !y_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			x_val = Integer.parseInt(y_var_val);
		}
		if(x.matches("[+-]?[0-9]+")) y_val = Integer.parseInt(x);
		else {
			String x_var_val = (String) env.variables.get(x);
			if(x_var_val == null || !x_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			y_val = Integer.parseInt(x_var_val);
		}
		
		if(y_val == 0) {
			env.stack.push(x);
			env.stack.push(y);
			env.stack.push(":error:");
		} else {
			Integer z = y_val / x_val;
			env.stack.push(z.toString());
		}
		
		return env;
	}
	
	public static Environment Rem(Environment env) {
		if (env.stack.size() < 2){
			env.stack.push(":error:");
			return env;
		}
		
		String y = env.stack.pop();
		String x = env.stack.pop();
		
		int x_val, y_val;
		if(y.matches("[+-]?[0-9]+")) x_val = Integer.parseInt(y);
		else {
			
			String y_var_val = (String) env.variables.get(y);
			if(y_var_val == null || !y_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			x_val = Integer.parseInt(y_var_val);
		}
		if(x.matches("[+-]?[0-9]+")) y_val = Integer.parseInt(x);
		else {
			String x_var_val = (String) env.variables.get(x);
			if(x_var_val == null || !x_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			y_val = Integer.parseInt(x_var_val);
		}
		
		if(y_val == 0) {
			env.stack.push(x);
			env.stack.push(y);
			env.stack.push(":error:");
		} else {
			Integer z = y_val % x_val;
			env.stack.push(z.toString());
		}
		
		return env;
	}

	

	public static Environment Neg(Environment env) {
		if (env.stack.isEmpty()){
			env.stack.push(":error:");
			return env;
		}		
		
		String y = env.stack.pop();
		
        int y_val;
        if(y.matches("[+-]?[0-9]+")) y_val = Integer.parseInt(y);
		else {
			String y_var_val = (String) env.variables.get(y);
			if(y_var_val == null || !y_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			y_val = Integer.parseInt(y_var_val);
        }
		Integer z = y_val * (-1);
		env.stack.push( z.toString() );
		
		return env;
	}

	private static Environment Swap(Environment env) {
		if (env.stack.size() < 2){
			env.stack.push(":error:");
		}
		else{
			String x = env.stack.pop();
			String y = env.stack.pop();
			env.stack.push(x);
			env.stack.push(y);
		}
		return env;
	}
	

	public static void Quit(Environment env, PrintStream outputStream) {
		quitOccured = true;
		while(!env.stack.isEmpty()) {
			String item = env.stack.pop().replace("\"", "");
			outputStream.println(item);
		}
		outputStream.close();
	}
	
	
	
	
	public static Environment And(Environment env) {
		if (env.stack.size() < 2){
			env.stack.push(":error:");
			return env;
		}
		
		String y = (String) env.stack.pop();
		String x = (String) env.stack.pop();
		
		boolean x_val, y_val;
		if(IsBool(y)) {
			y_val = StrToBool(y); 
		} else {
			String y_var_val = (String) env.variables.get(y);
			if(y_var_val == null || !IsBool(y_var_val)) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			y_val = StrToBool(y_var_val);
		}
		
		if(IsBool(x)) {
			x_val = StrToBool(x); 
		} else {
			String x_var_val = (String) env.variables.get(x);
			if(x_var_val == null || !IsBool(x_var_val)) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			x_val = StrToBool(x_var_val);
		}

		Boolean z = x_val && y_val;
		env.stack.push( BoolToStr(z) );
		return env;
	}
	
	public static Environment Or(Environment env) {
		if (env.stack.size() < 2){
			env.stack.push(":error:");
			return env;
		}
		
		String y = (String) env.stack.pop();
		String x = (String) env.stack.pop();
		
		boolean x_val, y_val;
		if(IsBool(y)) {
			y_val = StrToBool(y); 
		} else {
			String y_var_val = (String) env.variables.get(y);
			if(y_var_val == null || !IsBool(y_var_val)) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			y_val = StrToBool(y_var_val);
		}
		
		if(IsBool(x)) {
			x_val = StrToBool(x); 
		} else {
			String x_var_val = (String) env.variables.get(x);
			if(x_var_val == null || !IsBool(x_var_val)) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			x_val = StrToBool(x_var_val);
		}

		Boolean z = x_val || y_val;
		env.stack.push( BoolToStr(z) );
		return env;
	}
	
	public static Environment Not(Environment env) {
		if (env.stack.size() < 1){
			env.stack.push(":error:");
			return env;
		}
		
		String y = (String) env.stack.pop();
		
		boolean y_val;
		if(IsBool(y)) {
			y_val = StrToBool(y); 
		} else {
			String y_var_val = (String) env.variables.get(y);
			if(y_var_val == null || !IsBool(y_var_val)) {
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			y_val = StrToBool(y_var_val);
		}
		
		Boolean z = ! y_val;
		env.stack.push( BoolToStr(z) );
		return env;
	}
	
	public static Environment Equal(Environment env) {
		if (env.stack.size() < 2){
			env.stack.push(":error:");
			return env;
		}
		
		String y = env.stack.pop();
		String x = env.stack.pop();
		
		int x_val, y_val;
		if(y.matches("[+-]?[0-9]+")) y_val = Integer.parseInt(y);
		else {
			
			String y_var_val = (String) env.variables.get(y);
			if(y_var_val == null || !y_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			y_val = Integer.parseInt(y_var_val);
		}
		if(x.matches("[+-]?[0-9]+")) x_val = Integer.parseInt(x);
		else {
			String x_var_val = (String) env.variables.get(x);
			if(x_var_val == null || !x_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			x_val = Integer.parseInt(x_var_val);
		}
		
		
		Boolean z = y_val == x_val;
		env.stack.push(BoolToStr(z));
		
		
		return env;
	}
	
	
	public static Environment LessThan(Environment env) {
		if (env.stack.size() < 2){
			env.stack.push(":error:");
			return env;
		}
		
		String y = env.stack.pop();
		String x = env.stack.pop();
		
		int x_val, y_val;
		if(y.matches("[+-]?[0-9]+")) y_val = Integer.parseInt(y);
		else {
			
			String y_var_val = (String) env.variables.get(y);
			if(y_var_val == null || !y_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			y_val = Integer.parseInt(y_var_val);
		}
		if(x.matches("[+-]?[0-9]+")) x_val = Integer.parseInt(x);
		else {
			String x_var_val = (String) env.variables.get(x);
			if(x_var_val == null || !x_var_val.matches("[+-]?[0-9]+")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			x_val = Integer.parseInt(x_var_val);
		}
		
		
		Boolean z = y_val > x_val;
		env.stack.push(BoolToStr(z));
		
		
		return env;
	}
	
	public static Environment Bind(Environment env) {
		if (env.stack.size() < 2){
			env.stack.push(":error:");
			return env;
		}

		// Value
		String y = (String) env.stack.pop();
		// Name
		String x = (String) env.stack.pop();
		
		if(x.equals(":error:") || y.equals(":error:")) {
			env.stack.push(x);
			env.stack.push(y);
			env.stack.push(":error:");
			return env;
		}
		
		if(x.matches("^[a-zA-Z]+[0-9]?")) {
			String y_var_val = y;
			if(y.matches("^[a-zA-Z]+[0-9]?")) {
				y_var_val = (String)env.variables.get(y);
			}
			// Still not value but name
			if(y_var_val == null || y_var_val.matches("^[a-zA-Z]+[0-9]?")) {
				env.stack.push(x);
				env.stack.push(y);
				env.stack.push(":error:");
				return env;
			}
			
			env.variables.put(x, y_var_val);
			env.stack.push(":unit:");
		}
		else {
			env.stack.push(x);
			env.stack.push(y);
			env.stack.push(":error:");
		}

		return env;
	}
	
	public static Environment If(Environment env) {
		if (env.stack.size() < 3){
			env.stack.push(":error:");
			return env;
		}
		
		String x = env.stack.pop();
		String y = env.stack.pop();
		String z = env.stack.pop();
		
		boolean z_val;
		if(IsBool(z)) z_val = StrToBool(z);
		else {			
			String z_var_val = (String) env.variables.get(z);
			if(z_var_val == null || !IsBool(z_var_val)) {
				env.stack.push(z);
				env.stack.push(y);
				env.stack.push(x);				
				env.stack.push(":error:");
				return env;
			}
			z_val = StrToBool(z_var_val);
		}
	
		env.stack.push(z_val ? x : y);
		
		
		return env;
	}
	
	
	public static Environment Let(Environment env, Stack<String> cmd, PrintStream outputStream) {
		Environment env_let = new Environment();
		env_let.variables = (HashMap)env.variables.clone();
		env_let = execCommands(cmd, env_let, outputStream);
				
		if(!env_let.stack.isEmpty()) {
			String frame = env_let.stack.pop();
			String frame_value = frame;
			if(frame.matches("^[a-zA-Z]+[0-9]?")) {
				frame_value = (String)env_let.variables.get(frame);
			}
			if(frame_value == null)
				env.stack.push(frame);
			else
				env.stack.push(frame_value);
		}
		return env;
	}
	
	
	

}
