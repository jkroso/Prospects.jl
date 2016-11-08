@eval macro $:immutable(e::Expr) deftype(e, false) end
@eval macro $:type(e::Expr) deftype(e, true) end

deftype(e::Expr, mutable) = begin
  call,super = e.head ≡ :<: ? e.args : [e, Any]
  name, args = (call.args[1], call.args[2:end])
  def = Expr(:type, mutable, :($name <: $super), quote $(map(tofield, args)...) end)
  out = quote Base.@__doc__($(esc(def))) end
  for (i, arg) in enumerate(args)
    Meta.isexpr(arg, :kw) || continue
    params = map(tofield, take(args, i - 1))
    values = [map(tosymbol, params)..., tovalue(args[i])]
    push!(out.args, esc(:($name($(params...)) = $name($(values...)))))
  end
  if !mutable
    fields = map(x->x|>tofield|>tosymbol, args)
    push!(out.args, esc(defhash(sans_curly(name), fields)),
                    esc(defequals(sans_curly(name), fields)))
  end
  out
end

sans_curly(e::Expr) = e.args[1]
sans_curly(e) = e

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
tosymbol(e::Expr) = e.args[1]

extract_type(x::Any) = typeof(x)
extract_type(e::Expr) = begin
  e.head ≡ :curly && return e
  e.head ≡ :call && return e.args[1]
  error("unknown parameter format $e")
end

tovalue(s::Symbol) = s
tovalue(e::Expr) = e.head ≡ :kw ? e.args[2] : e.args[1]

"""
Define a basic stable `Base.hash` which just combines the hash of an
instance's `DataType` with the hash of its values
"""
defhash(T, fields) =
  :(Base.hash(a::$T, h::UInt) = $(foldr((f,e)->:(hash(a.$f, $e)), :(hash($T, h)), fields)))

"""
Define a basic `Base.==` which just recurs on each field of the type
"""
defequals(T, fields) = begin
  isempty(fields) && return nothing # already works
  exprs = map(f->:(a.$f == b.$f), fields)
  :(Base.:(==)(a::$T, b::$T) = $(foldr((a,b)->:($a && $b), exprs)))
end

export @immutable, @type
