@use MacroTools: MacroTools, @capture, @match, rmlines, prewalk
@use Distributed: Future
using Base.Iterators

"""
Define partial application methods for `fn` for when its called with too few arguments
"""
macro curry(fn::Expr)
  @capture(MacroTools.longdef(fn), function name_(params__) body_ end)
  out = :(begin Base.@__doc__($(esc(fn))) end)
  name = esc(name)
  for (i, param) in enumerate(params)
    i ≡ lastindex(params) && break
    isoptional(params[i+1]) && break
    args = map(esc, params[1:i])
    push!(out.args, quote
      if !ismethod($name, [$(map(arg_type, params[1:i])...)])
        $name($(args...)) = partial($name, $(args...))
      end
    end)
  end
  out|>MacroTools.flatten
end

arg_type(e::Expr) = (@assert(e.head ≡ :(::)); esc(e.args[2]))
arg_type(s::Symbol) = :Any
isoptional(param) = @capture(param, (_=_)|(_::_=_))

"""
Check if `f` has a specific method. It's basically a more specific
version of `hasmethod`

```julia
ismethod(map, (Function, String)) # => false
hasmethod(map, Tuple{Function, String}) # => true
```
"""
ismethod(f::Function, types::Any) = begin
  sig = Tuple{typeof(f), types...}
  any(m-> m.sig <: sig, methods(f, types))
end

"""
Recursively lift nested arrays/rows within an `array`
"""
flat(a::Union{Vector,Tuple}) = vcat(map(flat, a)...)
flat(a::Array) = reshape(a, prod(size(a)))
flat(a) = a

"""
Lift nested arrays one level
"""
flatten(a::Vector) = vcat(a...)

"""
Insert `x` between each value of `itr`
"""
interleave(itr, x) = Interleave(itr, x)

struct Interleave
  itr
  x
end
Base.length(i::Interleave) = max(2length(i.itr) - 1, 0)
Base.eltype(i::Interleave) = Union{eltype(i.itr), typeof(i.x)}
Base.iterate(i::Interleave) = iterate(i, (iterate(i.itr), false))
Base.iterate(i::Interleave, (state, middle)) = begin
  state == nothing && return nothing
  if middle
    (i.x, (state, false))
  else
    (state[1], (iterate(i.itr, state[2]), true))
  end
end

"""
An unsafe get
"""
Base.get(a, key) = begin
  a = get(a, key, Base.secret_table_token)
  a ≡ Base.secret_table_token && throw(KeyError(key))
  return a
end

"""
Default to `getproperty`
"""
Base.get(x, key, default) = hasproperty(x, key::Symbol) ? getproperty(x, key) : default
# base has a weird definition of get(::IO,::Any,::Any) which we can't shadow because other
# parts of base rely on it for terminal colors ???. But we can do this:
Base.get(io::IO, key::Symbol) = getproperty(io, key)
Base.get(t::Tuple, i, default) = isdefined(t, i) ? getindex(t, i) : default
Base.get(m::Module, name::Symbol, default) = isdefined(m, name) ? getfield(m, name) : default
# Dict's are really just a special type of function
(dict::Dict)(key) = get(dict, key)
(nt::NamedTuple)(key) = get(nt, key)

"""
Get a value deeply nested within an AbstractDict object
If no value is defined it will return default
"""
get_in(a, path, default) = begin
  for key in path
    a = get(a, key, Base.secret_table_token)
    a ≡ Base.secret_table_token && return default
  end
  return a
end

"""
Like the 3 argument version except it throws if it
the `path` is not defined
"""
get_in(a, path) = foldl(get, path, init=a)

# define keys/values for structs
Base.keys(x) = propertynames(x)
# this actually overrides a core definition so is sketchy
# https://github.com/JuliaLang/julia/blob/71518a370213db58b8875e5939666ad6d2bb8e7d/base/essentials.jl#L791
Base.values(t::T) where T = (get(t, k) for k in keys(t))

Base.values(s::Set) = s
Base.keys(s::Set) = throw("keys(::Set) is undefined")

"""
Map `f` over `itr` and flatten the result one level
"""
mapcat(f::Function, itr) =
  foldl(itr, init=[]) do result, value
    foldl(push!, f(value), init=result)
  end

"""
Compose a series of functions into one which takes an input and runs it
sequentially through all the composed functions and returns the result
"""
compose(fns::Any...) = input -> foldl((x, f) -> f(x), fns, init=input)

"""
Create a copy of a collection with some elements added
"""
push(collection, first, second, rest...) = push(push(push(collection, first), second), rest...)
push(collection) = collection
push(a::AbstractArray, item) = push!(copy(a), item)
push(d::Base.ImmutableDict{K,V}, p::Pair) where {K,V} = Base.ImmutableDict(d, p)
push(dict::AbstractDict, item::Pair) = push!(copy(dict), item)
push(object, pair::Pair) = assoc(object, pair[1], pair[2])
push(t::Tuple, x) = tuple(t..., x)

"""
Create a copy of a sequence with some elements added at the start
"""
unshift(collection, items...) = reduce(unshift, items, init=collection)
unshift(a::AbstractArray, item) = vcat(item, a)
unshift(a::AbstractArray, items...) = vcat(reverse(items)..., a)

"""
Create a copy of an `AbstractDict` like structure with one key=>value pair altered
"""
assoc(x, key, value, rest...) = assoc(assoc(x, key, value), rest...)
assoc(dict::AbstractDict{K,V}, key::K, value::V) where {K,V} = push!(copy(dict), key=>value)
assoc(dict::AbstractDict{K,V}, key::X, value::Y) where {K,V,X,Y} = Dict(dict..., key=>value)
assoc(d::Base.ImmutableDict{K,V}, key::K, value::V) where {K,V} = Base.ImmutableDict{K,V}(d, key, value)
assoc(arr::AbstractArray, i, value) = (arr = copy(arr); arr[i] = value; arr)
assoc(t::Tuple, i, value) = begin
  0 < i <= length(t) || throw(BoundsError(t, i))
  tuple(t[1:i-1]..., value, t[i+1:end]...)
end
assoc(o::NamedTuple, key, value) = begin
  names = propertynames(o)
  t = map(f -> f ≡ key ? value : getproperty(o, f), names)
  NamedTuple{names, typeof(t)}(t)
end
assoc(o::T, key, value) where T = T(map(f -> f ≡ key ? value : getproperty(o, f), propertynames(o))...)

"""
Add an association deep in the structure
"""
assoc_in(a::Any, kvs::Pair...) = reduce(assoc_in, kvs, init=a)
assoc_in(a::Any, kv::Pair) = begin
  (keys, value) = kv
  l = length(keys)
  l == 0 && return value
  (head,) = keys
  if l == 1
    assoc(a, head, value)
  else
    assoc(a, head, assoc_in(get(a, head), drop(keys, 1)=>value))
  end
end

"""
Create a copy of a collection with `keys` removed
"""
dissoc(dict::AbstractDict, key) = delete!(copy(dict), key)
dissoc(dict::AbstractDict, keys...) = foldl(delete!, keys, init=copy(dict))
dissoc(d::Base.ImmutableDict, key) = filter((k,v)-> k != key, d)
dissoc(a::AbstractArray, i) = deleteat!(copy(a), i)

"""
Remove an association deep in the structure
"""
dissoc_in(a, paths...) = reduce(dissoc_in, paths, init=a)
dissoc_in(a, path) = begin
  isempty(path) && return a
  key = first(path)
  rest = drop(path, 1)
  isempty(rest) && return dissoc(a, key)
  assoc(a, key, dissoc_in(get(a, key), rest))
end

"""
Split a sequence of values into two vectors according to the return value of `f`
"""
group(f, itr) = begin
  T = eltype(itr)
  yes,no = (Vector{T}(), Vector{T}())
  for value in itr
    push!(f(value) ? yes : no, value)
  end
  yes,no
end

"""
Create a new `Function` with some parameters already defined.
i.e a less abstract `Function`

```julia
isone = partial(==, 1)
map(isone, [1,2,3]) # => [true, false, false]
```
"""
partial(fn::Function, a...) = (b...) -> fn(a..., b...)

"""
Run a value through a series of transducers
"""
@curry transduce(fns::Vector, combine::Function) = foldr(partial, fns, init=combine)
@curry transduce(fns::Vector, combine::Function, accum, value) =
  foldr(partial, fns, init=combine)(accum, value)

# Define some basic transducers
@curry Base.map(f::Function, combine::Function, result, value) = combine(result, f(value))
@curry Base.filter(f::Function, combine::Function, result, value) =
  f(value) ? combine(result, value) : result
@curry mapcat(f::Function, combine::Function, result, value) = reduce(combine, f(value), init=result)

struct Field{name} end
(::Field{f})(object::Any) where f = getfield(object, f)
macro field_str(s) Field{Symbol(s)}() end
Base.get(o, ::Field{f}, default) where f = isdefined(o, f) ? getfield(o, f) : default
Base.get(o, ::Field{f}) where f = getfield(o, f)
Base.getproperty(o, ::Field{f}) where f = getfield(o, f)
Base.setproperty!(o, ::Field{f}, x) where f = setfield!(o, f, x)

"""
Unpack a boxed value. If necessary it can wait for the value to become available
"""
need(x::Any) = x
need(f::Future) = begin
  result = fetch(f)
  if isa(result, RemoteException)
    rethrow(result.captured.ex)
  else
    result
  end
end

need(t::Task) = begin
  istaskdone(t) && return t.result
  istaskfailed(t) && throw(t.result)
  try
    wait(t)
    t.result
  catch e
    throw(e.task.result)
  end
end

"""
Instead of throwing an Error if no value is a available it will just return a
`default` value
"""
need(x::Any, default::Any) = try need(x) catch; default end
need(f::Future, default::Any) = (r = fetch(f); isa(r, RemoteException) ? default : r)

# this was left out of base for some reason
Base.filter(f::Function, d::Base.ImmutableDict) =
  reduce((d, pair) -> f(pair...) ? Base.ImmutableDict(d, pair) : d, d, init=typeof(d)())

"""
Wait for one of several conditions to trigger. Returns a `Condition`
which will trigger with a `Tuple` of the first input to trigger along
with the value that it triggered with
"""
waitany(conditions...) = begin
  out = Condition()
  triggered = false
  for c in conditions
    @async try
      value = wait(c)
      if !triggered
        triggered = true
        notify(out, (value, c))
      end
    catch e
      if !triggered
        triggered = true
        notify(out, (e, c), error=true)
      end
    end
  end
  wait(out)
end

waitall(conditions...) = asyncmap(wait, conditions)

@eval macro $:struct(e::Expr) deftype(parse_type(e), false, __module__) end
@eval macro $:mutable(e::Expr) deftype(parse_type(e), true, __module__) end

parse_type(e::Expr) =
  @match e begin
    (call_ <: super_) => assoc(parse_call(call), :super, super)
    _(__) => parse_call(e)
    _ => parse_struct(e)
  end

parse_struct(e::Expr) = begin
  @capture e struct ((T_<:super_)|T_) fields__ end
  @capture T (name_{curlies__}|name_)
  (fields=[parse_field(f) for f in fields],
   curlies=curlies==nothing ? [] : curlies,
   name=name,
   super=super==nothing ? :Any : super)
end

parse_call(e::Symbol) = (fields=[], curlies=[], name=e, super=Any)
parse_call(e::Expr) = begin
  if @capture e (name_{curlies__}|name_)(args__)
    (fields=[parse_field(a) for a in args], curlies=curlies == nothing ? [] : curlies, name=name, super=Any)
  elseif @capture e (name_{curlies__}|name_)
    (fields=Any[], curlies=curlies == nothing ? [] : curlies, name=name, super=Any)
  else
    error("unable to parse name of your datatype")
  end
end

@Base.kwdef struct FieldDef
  name::Symbol
  type::Any=:Any
  default::Any=missing
  isoptional::Bool=false
end

parse_field(e) =
  @match e begin
    (s_Symbol::t_=default_) => FieldDef(s, t, default, true)
    (s_Symbol=default_) => FieldDef(s, :(typeof($default)), default, true)
    (s_Symbol::t_) => FieldDef(s, t, missing, @capture(t, (Union{Missing,_}|Union{_,Missing})))
    (s_Symbol) => FieldDef(name=s)
    _ => error("unknown field definition $e")
  end

tofield(f::FieldDef) = :($(f.name)::$(f.type))

deftype((fields, curlies, name, super)::NamedTuple, mutable, __module__) = begin
  T = isempty(curlies) ? name : :($name{$(curlies...)})
  s = eval(__module__, super)
  while s != Any
    # mixin inherited fields
    haskey(field_map, s) && push!(fields, field_map[s]...)
    s = supertype(s)
  end
  def = Expr(:struct, mutable, :($T <: $super), quote $(map(tofield, fields)...) end)
  out = quote Base.@__doc__($(esc(def))) end
  for i in length(fields):-1:2
    field = fields[i]
    field.isoptional || break
    names = map(field"name", fields[1:i-1])
    values = [names..., field.default]
    push!(out.args, esc(:($T($(names...)) where {$(curlies...)} = $T($(values...)))))
  end
  if !mutable
    push!(out.args, esc(defhash(T, curlies, map(field"name", fields))),
                    esc(defequals(T, curlies, map(field"name", fields))))
  end
  push!(out.args, esc(kwdef(T, curlies, fields)))
  push!(out.args, nothing)
  out
end

kwdef(T, curlies, fields) = begin
  isempty(fields) && return nothing
  :($T(;$(map(to_kwarg, fields)...)) where {$(curlies...)} = $T($(map(field"name", fields)...)))
end

to_kwarg(f::FieldDef) = f.isoptional ? Expr(:kw, f.name, f.default) : :($(f.name))

"""
Define a basic stable `Base.hash` which just combines the hash of an
instance's `DataType` with the hash of its values
"""
defhash(T, curlies, fields) = begin
  body = foldr((f,e)->:(hash(getfield(a, $(QuoteNode(f))), $e)), fields, init=:(hash($T, h)))
  :(Base.hash(a::$T, h::UInt) where {$(curlies...)} = $body)
end

"""
Define a basic `Base.==` which just recurs on each field of the type
"""
defequals(T, curlies, fields) = begin
  isempty(fields) && return nothing # already works
  exprs = map(f->:(Base.isequal(getfield(a, $f), getfield(b, $f))), map(QuoteNode, fields))
  body = foldr((a,b)->:($a && $b), exprs)
  :(Base.:(==)(a::$T, b::$T) where {$(curlies...)} = $body)
end

const field_map = IdDict()

"Defines an abstract type with sudo fields"
macro abstract(expr)
  mutable, def, body = expr.args
  fields = map(parse_field, rmlines(body).args)
  fields = map(fields) do field
    assoc(field, :default, prewalk(field.default) do e
      Meta.isexpr(e, :$) ? Base.eval(__module__, e.args[1]) : e
    end)
  end
  @capture def (name_ <: _)|name_
  quote
    Base.@__doc__ abstract type $(esc(def)) end
    field_map[$(esc(name))] = $fields
    nothing
  end
end

Base.convert(::Type{NamedTuple}, x::T) where T =
  NamedTuple{fieldnames(T), Tuple{fieldtypes(T)...}}(tuple(values(x)...))

export group, assoc, dissoc, compose, mapcat, flat,
       flatten, get_in, partial, @curry,
       transduce, ismethod, Field, @field_str,
       need, push, assoc_in, dissoc_in, unshift,
       waitany, waitall, @struct, @mutable, interleave,
       @abstract
