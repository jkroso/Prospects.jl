@use MacroTools: MacroTools, @capture, @match, rmlines, prewalk
@use Distributed: Future
using Base.Iterators

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
Iterators.flatten(a::Vector) = vcat(a...)

"""
Insert `x` between each value of `itr`
"""
interleave(itr, x) = take(flatten(zip(itr, repeated(x))), max(0,2length(itr)-1))

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
# Base.values(t::T) where T = (get(t, k) for k in keys(t))

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
Create a copy of a collection with the last element removed
"""
pop(v::AbstractVector) = pop!(copy(v))
pop(t::Tuple) = t[begin:end-1]
pop(t::AbstractString) = t[begin:end-1]

"""
Create a copy of a collection with some elements added
"""
append(collection, first, second, rest...) = append(append(append(collection, first), second), rest...)
append(collection) = collection
append(a::AbstractArray, item) = push!(copy(a), item)
append(d::Base.ImmutableDict{K,V}, p::Pair) where {K,V} = Base.ImmutableDict(d, p)
append(dict::AbstractDict, item::Pair) = push!(copy(dict), item)
append(t::Tuple, x) = tuple(t..., x)

"""
Create a copy of a sequence with some elements added at the start
"""
prepend(collection, items...) = reduce(prepend, items, init=collection)
prepend(a::AbstractArray, item) = vcat(item, a)
prepend(a::AbstractArray, items...) = vcat(reverse(items)..., a)

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
assoc(o::NamedTuple, key::Symbol, value) = begin
  names = propertynames(o)
  t = map(f -> f ≡ key ? value : getproperty(o, f), names)
  NamedTuple{names, typeof(t)}(t)
end
assoc(o::NamedTuple, key::Integer, value) = typeof(o)((i == key ? value : v for (i,v) in enumerate(o)))
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

struct Field{name} end
(f::Field)(object::Any) = get(object, f)
macro field_str(s) Field{Symbol(s)}() end
Base.get(o, ::Field{f}, default) where f = isdefined(o, f) ? getfield(o, f) : default
Base.get(o, f::Field) = getproperty(o, f)
Base.getproperty(o, ::Field{f}) where f = getfield(o, f)
Base.setproperty!(o, ::Field{f}, x) where f = setfield!(o, f, x)
assoc(o, key::Field{f}, value) where f = assoc(o, f, value)

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

macro def(e::Expr)
  if e.head == :abstract
    def = e.args[1]
    @capture def (name_ <: _)|name_
    return quote
      Base.@__doc__ abstract type $(esc(namespacedef(def, __module__))) end
      field_map[$(esc(name))] = []
      store_field_info(@__MODULE__, $(esc(name)), [])
      nothing
    end
  end
  s = parse_type(e)
  deftype(s, s.mutable, __module__)
end

macro def(abstract_kw::Symbol, e::Expr)
  abstract_kw == :abstract || error("unexpected keyword: $abstract_kw")
  _, def, body = e.args
  fields = map(parse_field, rmlines(body).args)
  fields = map(fields) do field
    assoc(field, :default, namespace(field.default, __module__),
                 :type, namespace(field.type, __module__))
  end
  @capture def (name_ <: _)|name_
  quote
    Base.@__doc__ abstract type $(esc(namespacedef(def, __module__))) end
    field_map[$(esc(name))] = $fields
    store_field_info(@__MODULE__, $(esc(name)), $fields)
    nothing
  end
end

parse_type(e::Expr) = begin
  @match e begin
    (call_ <: super_) => assoc(parse_call(call), :super, super)
    _(__) => parse_call(e)
    _ => parse_struct(e)
  end
end

parse_struct(e::Expr) = begin
  @capture e (mutable struct ((T_<:super_)|T_) fields__ end)|(struct ((T_<:super_)|T_) fields__ end)
  @capture T (name_{curlies__}|name_)

  # Separate fields and constructor definitions
  parsed_fields = []
  constructors = []

  for field in fields
    if is_constructor_def(field)
      push!(constructors, field)
    else
      push!(parsed_fields, parse_field(field))
    end
  end

  (fields=parsed_fields,
   constructors=constructors,
   curlies=curlies==nothing ? [] : curlies,
   name=name,
   mutable=@capture(e, mutable struct _ __ end),
   super=super==nothing ? :Any : super)
end

parse_call(e::Symbol) = (fields=[], constructors=[], curlies=[], name=e, mutable=false, super=Any)
parse_call(e::Expr) = begin
  if @capture e ((name_{curlies__}|name_)(args__))|(name_{curlies__}|name_)
    (fields=isnothing(args) ? [] : [parse_field(a) for a in args],
     constructors=[],
     curlies=curlies == nothing ? [] : curlies,
     name=name,
     mutable=false,
     super=Any)
  else
    error("unable to parse name of your datatype")
  end
end

@kwdef struct FieldDef
  name::Symbol
  type::Any=:Any
  default::Any=missing
  isoptional::Bool=false
end

parse_field(e) = begin
  @match e begin
    (s_Symbol::t_=default_) => FieldDef(s, t, default, true)
    (s_Symbol=default_) => FieldDef(s, :(typeof($default)), default, true)
    (s_Symbol::t_) => FieldDef(s, t, missing, @capture(t, (Union{Missing,_}|Union{_,Missing})))
    (s_Symbol) => FieldDef(name=s)
    _ => error("unknown field definition $e")
  end
end

tofield(f::FieldDef) = :($(f.name)::$(f.type))

# Helper function to identify constructor definitions
is_constructor_def(expr) = begin
  @capture(expr, (name_(args__) = body_) | (name_(args__) where {params__} = body_) |
                 (function name_(args__) body_ end) | (function name_(args__) where {params__} body_ end))
end

deftype((;fields, constructors, curlies, name, super)::NamedTuple, mutable, __module__, defoptionals=true) = begin
  T = isempty(curlies) ? name : :($name{$(curlies...)})
  s = Base.eval(__module__, super)
  while s != Any
    # mixin inherited fields, skipping ones the subtype already defines
    # Try field_map first, then fall back to module-local __field_info__
    # (field_map entries are lost when loading from compiled cache)
    if !haskey(field_map, s)
      recover_field_info!(s)
    end
    if haskey(field_map, s)
      existing = Set(f.name for f in fields)
      for f in field_map[s]
        f.name in existing || push!(fields, f)
      end
    end
    s = supertype(s)
  end
  # Build struct body with fields and constructors
  struct_body = quote $(map(tofield, fields)...) end

  # Add inner constructors to struct body
  for constructor in constructors
    push!(struct_body.args, constructor)
  end

  def = Expr(:struct, mutable, :($T <: $super), struct_body)
  out = quote Base.@__doc__($(esc(def))) end
  if defoptionals
    for i in length(fields):-1:2
      field = fields[i]
      field.isoptional || break
      names = map(field"name", fields[1:i-1])
      values = [names..., field.default]
      push!(out.args, esc(:($T($(names...)) where {$(curlies...)} = $T($(values...)))))
    end
  end
  if !mutable
    push!(out.args, esc(defhash(T, curlies, map(field"name", fields))),
                    esc(defequals(T, curlies, map(field"name", fields))))
  end
  push!(out.args, esc(kwdef(T, curlies, fields)))

  push!(out.args, :(Base.getproperty(t::$(esc(name)), k::Symbol) = getproperty(t, Field{k}())))
  push!(out.args, :(Base.setproperty!(t::$(esc(name)), k::Symbol, x) = setproperty!(t, Field{k}(), x)))
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

"""
Store field info in a module-local const dict so it survives compilation cache serialization.
The global `field_map` IdDict is populated as a runtime side-effect that gets lost when loading
from `.ji` cache. This function stores the same info as a module `const` which IS serialized.
"""
function store_field_info(mod::Module, T::DataType, fields::Vector)
  if !isdefined(mod, :__field_info__)
    Base.invokelatest(Core.eval, mod, :(const __field_info__ = $(Dict{Symbol,Vector}())))
  end
  Base.invokelatest(getfield, mod, :__field_info__)[nameof(T)] = fields
end

"""
Recover field info from a module's serialized `__field_info__` dict when `field_map` is empty
(typically after loading from compiled cache).
"""
function recover_field_info!(T::DataType)
  mod = parentmodule(T)
  if isdefined(mod, :__field_info__)
    fi = getfield(mod, :__field_info__)
    name = nameof(T)
    if haskey(fi, name)
      field_map[T] = fi[name]
      return true
    end
  end
  false
end

"Defines an abstract type with sudo fields"
macro abstract(expr)
  _, def, body = expr.args
  fields = map(parse_field, rmlines(body).args)
  fields = map(fields) do field
    assoc(field, :default, namespace(field.default, __module__),
                 :type, namespace(field.type, __module__))
  end
  @capture def (name_ <: _)|name_
  quote
    Base.@__doc__ abstract type $(esc(namespacedef(def, __module__))) end
    field_map[$(esc(name))] = $fields
    store_field_info(@__MODULE__, $(esc(name)), $fields)
    nothing
  end
end

namespace(expr::Any, mod) = expr
namespace(expr::Symbol, mod) = Expr(:(.), mod, QuoteNode(expr))
namespace(expr::Expr, mod) = begin
  if expr.head == :(.)
    Expr(:(.), namespace(expr.args[1], mod), expr.args[2])
  elseif expr.head == :(=)
    Expr(:(=), expr.args[1], namespace(expr.args[2], mod))
  elseif expr.head == :abstract
    Expr(:abstract, namespacedef(expr.args[1], mod), map(x->namespacedef(x,mod), expr.args[2:end])...)
  else
    Expr(expr.head, map(x->namespace(x, mod), expr.args)...)
  end
end

namespacedef(expr::Any, mod) = expr
namespacedef(expr::Expr, mod) = begin
  if expr.head == :(<:)
    Expr(:(<:), expr.args[1], namespace(expr.args[2], mod))
  elseif expr.head == :(::)
    Expr(:(::), expr.args[1], namespace(expr.args[2], mod))
  else
    namespace(expr, mod)
  end
end

Base.convert(::Type{NamedTuple}, x::T) where T = begin
  NamedTuple{fieldnames(T), Tuple{fieldtypes(T)...}}(tuple((getfield(x, f) for f in fieldnames(T))...))
end

"""
Defines a getproperty method on a type

```julia
@property AbstractDict.keys = keys(self)
```

The code block can access the instance using `self`
"""
macro property(e)
  @assert @capture e T_Symbol.p_ = def_
  quote
    if !ismethod($Base.getproperty, ($(esc(T)), Symbol))
      $Base.getproperty(self::$(esc(T)), s::Symbol) = $Base.getproperty(self, Field{s}())
    end
    $Base.getproperty(($(esc(:self)))::$(esc(T)), ::Field{$(QuoteNode(p))}) = $(esc(def))
  end
end

"""
Works just like `@property` except the generated value will be stored in a field of the same name.
So the type must be mutable
"""
macro lazyprop(expr)
  @assert @capture expr T_Symbol.p_ = def_
  quote
    @assert ismutabletype($(esc(T)))
    if !ismethod($Base.getproperty, ($(esc(T)), Symbol))
      $Base.getproperty(self::$(esc(T)), s::Symbol) = $Base.getproperty(self, Field{s}())
    end
    $Base.getproperty(($(esc(:self)))::$(esc(T)), ::Field{$(QuoteNode(p))}) = begin
      if isdefined($(esc(:self)), $(QuoteNode(p)))
        getfield($(esc(:self)), $(QuoteNode(p)))
      else
        setfield!($(esc(:self)), $(QuoteNode(p)), $(esc(def)))
      end
    end
  end
end

export group, assoc, dissoc, compose, mapcat, flat,
       flatten, get_in, pop, ismethod, Field, @field_str,
       need, append, assoc_in, dissoc_in, prepend,
       waitany, waitall, @struct, @mutable, interleave,
       @abstract, @property, @lazyprop, @def
