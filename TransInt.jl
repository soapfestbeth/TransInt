#
# Class Interpreter 3
# With numbers, plus, minus, if0, with, variable references,
# user-defined functions (lambda), and function calls.
#

module TransInt

push!(LOAD_PATH, pwd())

using Error
using Lexer
using Printf

export parse, calc, interp, analyze

#
# ==================================================
#

abstract type AE
end

# <AE> ::= <number>
struct NumNode <: AE
    n::Real
end

# <AE> ::= (+ <AE> <AE> ...)
struct PlusNode <: AE
	addends::Array{AE}
end

# <AE> ::= (- <AE> <AE>)
struct MinusNode <: AE
    lhs::AE
    rhs::AE
end

struct AndNode <: AE
	expressions::Array{AE}
end

struct UnaryNode <: AE
	op::Function
	param::AE
end

struct BinopNode <: AE
	op::Function
	lhs::AE
	rhs::AE
end

# <AE> ::= (if0 <AE> <AE> <AE>)
struct If0Node <: AE
    cond::AE
    zerobranch::AE
    nzerobranch::AE
end

# <AE> ::= (with <id> <AE> <AE>)
struct WithNode <: AE
    symbols::Array{Symbol,1}
    binding_expressions::Array{AE,1}
    body::AE
end

# <AE> ::= <id>
struct VarRefNode <: AE
    sym::Symbol
end

# <AE> ::= (lambda <id> <AE>)
struct FuncDefNode <: AE
    formal::Array{Symbol}
    body::AE
end

# <AE> ::= (<AE> <AE>)
struct FuncAppNode <: AE
    fun_expr::AE
    arg_exprs::Array{AE}
end

#
# ==================================================
#

function collatz( n::Real )
	if n <= 0
		throw(LispError("Error: collatz(n) not permitted for n < 1"))
	end
 	return collatz_helper( n, 0 )
end

function collatz_helper( n::Real, num_iters::Int )
  if n == 1
    return num_iters
  end
  if mod(n,2)==0
    return collatz_helper( n/2, num_iters+1 )
  else
    return collatz_helper( 3*n+1, num_iters+1 )
  end
end

function divide(a::Number, b::Number)
	if b == 0
		throw(LispError("Error: Divide by zero not allowed"))
	end
	return a / b
end

#
# ====================
#

abstract type RetVal
end

abstract type Environment
end

struct NumVal <: RetVal
    n::Real
end

struct ClosureVal <: RetVal
    formal::Array{Symbol}
    body::AE
    env::Environment
end

#
# ==================================================
#

struct EmptyEnv <: Environment
end

struct ExtendedEnv <: Environment
    symbols::Array{Symbol,1}
    values::Array{RetVal,1}
    parent::Environment
end



#
# ==================================================
#

function isProtectedSymbol(symbol::Symbol)
	protected_symbols = [:+, :-, :*, :/, :mod, :collatz, :lambda, :with, :if0]
	if length(findall(protected_symbols .== symbol)) != 0
		return true
	end
	return false
end

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Symbol )
	if isProtectedSymbol(expr)
		throw(LispError("ERROR: Protected symbols cannot be used as variable names"))
	end
    return VarRefNode( expr )
end

function parseWith( expr )
	if length(expr) != 3
		throw(LispError("ERROR: Invalid format for with"))
	end
	if typeof(expr[2]) == Array{Any,1} && length(expr[2]) == 2 && typeof(expr[2][1]) == Symbol
		if isProtectedSymbol(expr[2][1])
			throw(LispError("ERROR: Protected symbols cannot be used as variable names"))
		end
		return WithNode( [expr[2][1]], [parse(expr[2][2])], parse(expr[3]) )
	else
		symbols = Symbol[]
		values = AE[]
		if typeof(expr[2]) != Array{Any,1}
			throw(LispError("ERROR: Expected array of arrays, format: (with ((x 1) (y 2) (z 3) ...) (expr) )"))
		end
		for element in expr[2]
			if typeof(element) != Array{Any, 1} || length(element) != 2
				throw(LispError("ERROR: Expected array of arrays, format: (with ((x 1) (y 2) (z 3) ...) (expr) )"))
			end
			if length(findall(symbols .== element[1])) != 0
				throw(LispError("ERROR: Duplicate variable names not allowed."))
			end
			if isProtectedSymbol(element[1])
				throw(LispError("ERROR: Protected symbols cannot be used as variable names"))
			end
			push!(symbols, element[1])
			push!(values, parse(element[2]))
		end
		return WithNode( symbols, values, parse(expr[3]))
	end
end

function parseLambda( expr )
	if length(expr) != 3
		throw(LispError("Invalid format for lambda"))
	end
	if typeof(expr[2]) == Symbol
		if isProtectedSymbol(expr[2])
			throw(LispError("ERROR: Protected symbols cannot be used as variable names"))
		end
		return FuncDefNode( [expr[2]], parse(expr[3]) )
	else
		symbols = Symbol[]
		for element in expr[2]
			if typeof(element) != Symbol
				throw(LispError("ERROR: Expected array of variable names, format: (lambda (x y z ...) (expr) )"))
			elseif isProtectedSymbol(element)
				throw(LispError("ERROR: Protected symbols cannot be used as variable names"))
			elseif length(findall(symbols .== element)) != 0
				throw(LispError("ERROR: Duplicate variable names not allowed."))
			end
			push!(symbols, element)
		end
		return FuncDefNode( symbols, parse(expr[3]))
	end
end

function parseUnaryOps(expr)
	unaryOps = Dict(:collatz => collatz, :- => -)
	if haskey(unaryOps, expr[1])
		unaryNode = UnaryNode(unaryOps[expr[1]], parse(expr[2]))
		return unaryNode
	else
		throw(LispError("Error: expected a unary operator"))
	end
end

function parseBinaryOps(expr)
	binOps = Dict(:+ => +, :- => -, :* => *, :/ => divide, :mod => mod)
	if haskey(binOps, expr[1])
		binNode = BinopNode(binOps[expr[1]], parse(expr[2]), parse(expr[3]))
		return binNode
	else
		throw(LispError("Error: expected a binary operator"))
	end
end

function parseFunAppNode(expr)
	arg_expressions = []
	for i=2:length(expr)
		push!(arg_expressions, expr[i])
	end
	arg_ae_expressions = map(parse, arg_expressions)
	return FuncAppNode( parse(expr[1]), arg_ae_expressions)
end

function parsePlus(expr)
	addends = map(parse, getindex(expr, 2:length(expr)))
	return PlusNode(addends)
end

function parseAnd(expr)
	expressions = map(parse, getindex(expr, 2:length(expr)))
	return AndNode(expressions)
end

function parse( expr::Array{Any} )
	unaryOps = Dict(:collatz => collatz, :- => -)
	binOps = Dict(:+ => +, :- => -, :* => *, :/ => divide, :mod => mod)
	operator = expr[1]

    if expr[1] == :if0 && length(expr) == 4
        return If0Node( parse(expr[2]), parse(expr[3]) , parse(expr[4]) )

    elseif expr[1] == :with
		return parseWith(expr)

    elseif expr[1] == :lambda
		return parseLambda(expr)

	elseif expr[1] == :and
		return parseAnd(expr)

	elseif length(expr) == 2 && haskey(unaryOps, expr[1])
		return parseUnaryOps(expr)

	elseif expr[1] == :+
		return parsePlus(expr)

	elseif length(expr) == 3 && haskey(binOps, expr[1])
		return parseBinaryOps(expr)

    else
        return parseFunAppNode(expr)
	end
    throw(LispError("ERROR: Unknown operator $operator"))
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

#
# ==================================================
#

function analyze(ast::NumNode)
	return ast
end

function analyze(ast::VarRefNode)
	return ast
end

function analyze(ast::PlusNode)
	if length(ast.addends) == 2
		return BinopNode(+, ast.addends[1], ast.addends[2])
	else
		return BinopNode(+, ast.addends[1], analyze(PlusNode(getindex(ast.addends, 2:length(ast.addends)))))
	end
end

function analyze(ast::AndNode)
	if length(ast.expressions) == 1
		return If0Node(ast.expressions[1], NumNode(0), NumNode(1))
	else
		return If0Node(ast.expressions[1], NumNode(0), analyze(AndNode(getindex(ast.expressions, 2:length(ast.expressions)))))
	end
end

function analyze(ast::MinusNode)
	lhs = analyze(ast.lhs)
	rhs = analyze(ast.rhs)

	#=
	if typeof(lhs) == NumNode && typeof(rhs) == NumNode
		return NumNode(lhs.n - rhs.n)
	end
	=#
	return MinusNode(lhs, rhs)
end

function analyze(ast::UnaryNode)
	return UnaryNode(ast.op, analyze(ast.param))
end

function analyze(ast::BinopNode)
	lhs = analyze(ast.lhs)
	rhs = analyze(ast.rhs)
	#=
	if typeof(lhs) == NumNode && typeof(rhs) == NumNode
		return NumNode(map(ast.op, [lhs.n], [rhs.n]))
	end
	=#
	return BinopNode(ast.op, lhs, rhs)
end

function analyze(ast::If0Node)
	cond = analyze(ast.cond)
    #=
	if typeof(cond) == NumNode
		if cond.n == 0
			return analyze(ast.zerobranch)
		else
			return analyze(ast.nonzerobrach)
		end
	end
	=#
	return If0Node(cond, analyze(ast.zerobranch), analyze(ast.nonzerobranch))
end

function analyze(ast::WithNode)
	fdn = FuncDefNode(ast.symbols, analyze(ast.body))
	return FuncAppNode(fdn, map(analyze, ast.binding_expressions))
end

function analyze(ast::FuncAppNode)
	return ast
end

function analyze(ast::FuncDefNode)
	return ast
end

#
# ====================================================
#

function calc( ast::NumNode, env::Environment )
    return NumVal( ast.n )
end

function calc(ast::UnaryNode, env::Environment)
	param = calc(ast.param, env)
	if typeof(param) == ClosureVal
		throw(LispError("ERROR: Expected numerical input, received function"))
	end
	return NumVal(map(ast.op, [param.n])[1])
end

function calc(ast::BinopNode, env::Environment)
	lhs = calc(ast.lhs, env)
	rhs = calc(ast.rhs, env)
	if typeof(lhs) == ClosureVal || typeof(rhs) == ClosureVal
		throw(LispError("ERROR: Expected numerical input, received function"))
	end
	return NumVal(map(ast.op, [lhs.n], [rhs.n])[1])
end

function calc( ast::If0Node, env::Environment )
    cond = calc( ast.cond, env )
    if cond.n == 0
        return calc( ast.zerobranch, env )
    else
        return calc( ast.nzerobranch, env )
    end
end

#=
function calc( ast::WithNode, env::Environment )
    #binding_values = calc( ast.binding_expr, env )
	binding_values = map(x -> calc(x, env), ast.binding_expressions)
    ext_env = ExtendedEnv( ast.symbols, binding_values, env )
    return calc( ast.body, ext_env )
end
=#

function calc( ast::VarRefNode, env::EmptyEnv )
    throw( Error.LispError("Undefined variable " * string( ast.sym )) )
end

function calc( ast::VarRefNode, env::ExtendedEnv )
    if length(findall(env.symbols .== ast.sym)) != 0
        return env.values[findall(env.symbols .== ast.sym)[1]]
    else
        return calc( ast, env.parent )
    end
end

function calc( ast::FuncDefNode, env::Environment )
    return ClosureVal( ast.formal, ast.body , env )
end

function calc( ast::FuncAppNode, env::Environment )
    closure_val = calc( ast.fun_expr, env )

	if typeof(closure_val) != ClosureVal
		throw(LispError("ERROR: Expected function call"))
	end

	if length(ast.arg_exprs) != length(closure_val.formal)
		throw(LispError("ERROR: Arity of supplied arguments does not match arity of function"))
	end
    #actual_parameter = calc( ast.arg_expr, env )
	actual_parameters = map(x -> calc(x, env), ast.arg_exprs)
    ext_env = ExtendedEnv( closure_val.formal,
                           actual_parameters,
                           closure_val.env )
    return calc( closure_val.body, ext_env )
end

function calc( ast::AE )
    return calc( ast, EmptyEnv() )
end

#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
	ast = analyze(ast)
	#return ast
    return calc( ast, EmptyEnv() )
end

# evaluate a series of tests in a file
function interpf( fn::AbstractString )
  f = open( fn )

  cur_prog = ""
  for ln in eachline(f)
      ln = chomp( ln )
      if length(ln) == 0 && length(cur_prog) > 0
          ( "" )
          println( "--------- Evaluating ----------" )
          println( cur_prog )
          println( "---------- Returned -----------" )
          try
              println( interp( cur_prog ) )
          catch errobj
              println( ">> ERROR: lxd" )
              lxd = Lexer.lex( cur_prog )
              println( lxd )
              println( ">> ERROR: ast" )
              ast = parse( lxd )
              println( ast )
              println( ">> ERROR: rethrowing error" )
              throw( errobj )
          end
          println( "------------ done -------------" )
          println( "" )
          cur_prog = ""
      else
          cur_prog *= ln
      end
  end

  close( f )
end

end #module
