@eval macro $:immutable(e::Expr) deftype(e, false) end
@eval macro $:type(e::Expr) deftype(e, true) end

deftype(e::Expr, mutable) = begin
  call,super = e.head ≡ :<: ? e.args : [e, Any]
  name, args = (call.args[1], call.args[2:end])
  def = Expr(:type, mutable,
             :($(esc(name)) <: $(esc(super))),
             quote $(map(tofield, args)...) end)
  out = quote Base.@__doc__($def) end
  for (i, arg) in enumerate(args)
    isoptional(arg) || continue
    params = map(tofield, take(args, i - 1))
    values = map(tovalue, take(args, i))
    push!(out.args, esc(:($name($(params...)) = $name($(values...)))))
  end
  out
end

isoptional(param::Expr) = param.head ≡ :kw
isoptional(::Any) = false

tofield(e::Expr) =
  if e.head ≡ :kw
    if isa(e.args[1], Expr)
      e.args[1]
    else
      Expr(:(::), e.args[1], extract_type(e.args[2]))
    end
  else
    @assert e.head ≡ :(::)
    e
  end
tofield(s::Symbol) = Expr(:(::), s, Any)

extract_type(x::Any) = typeof(x)
extract_type(e::Expr) = begin
  e.head ≡ :curly && return e
  e.head ≡ :call && return e.args[1]
  error("unknown paramter format $e")
end

tovalue(s::Symbol) = s
tovalue(e::Expr) = e.head ≡ :kw ? e.args[2] : e.args[1]

export @immutable, @type
