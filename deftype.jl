@eval macro $:struct(e::Expr) deftype(e, false) end
@eval macro $:mutable(e::Expr) deftype(e, true) end

deftype(e::Expr, mutable) = begin
  call,super = e.head ≡ :<: ? e.args : [e, Any]
  name, args = (call.args[1], call.args[2:end])
  def = Expr(:type, mutable, :($name <: $super), quote $(map(tofield, args)...) end)
  out = quote Base.@__doc__($(esc(def))) end
  for i in length(args):-1:1
    arg = args[i]
    isoptional(arg) || break
    params = map(nullable_param, map(tofield, take(args, i - 1)))
    values = [map(tosymbol, params)..., tovalue(args[i])]
    push!(out.args, esc(:($name($(params...)) = $name($(values...)))))
  end
  if !mutable
    fields = map(x->x|>tofield|>tosymbol, args)
    push!(out.args, esc(defhash(name, fields)),
                    esc(defequals(name, fields)))
  end
  push!(out.args, nothing)
  out
end

isoptional(e::Symbol) = false
isoptional(e::Expr) = e.head ≡ :kw || e.head ≡ :(::) && isnullable(e.args[2])

isnullable(e) = false
isnullable(e::Symbol) = e ≡ :Nullable
isnullable(e::Expr) = e.head ≡ :curly && e.args[1] ≡ :Nullable

# a::Nullable → a::Union{Nullable,Any}
nullable_param(e::Expr) =
  if isnullable(e.args[2])
    Expr(:(::), e.args[1], :(Union{$(e.args[2]),$(get_type_param(e.args[2]))}))
  else
    e
  end

# Nullable{Int} → Int
get_type_param(e::Symbol) = Any
get_type_param(e::Expr) = e.args[2]

# a=1 → a::Int
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
tovalue(e::Expr) =
  if     e.head ≡ :kw e.args[2]
  elseif isnullable(e.args[2]) Expr(:call, e.args[2])
  else   e.args[1]
  end

"""
Define a basic stable `Base.hash` which just combines the hash of an
instance's `DataType` with the hash of its values
"""
defhash(T, fields) = begin
  body = foldr((f,e)->:(hash(a.$f, $e)), :(hash($T, h)), fields)
  :(Base.hash{$(curlies(T)...)}(a::$T, h::UInt) = $body)
end

"""
Define a basic `Base.==` which just recurs on each field of the type
"""
defequals(T, fields) = begin
  isempty(fields) && return nothing # already works
  exprs = map(f->:($isequal(a.$f, b.$f)), fields)
  body = foldr((a,b)->:($a && $b), exprs)
  :(Base.:(==){$(curlies(T)...)}(a::$T, b::$T) = $body)
end

curlies(e) = Meta.isexpr(e, :curly) ? e.args[2:end] : []

export @struct, @mutable
