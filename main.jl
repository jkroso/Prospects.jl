@require "github.com/MikeInnes/MacroTools.jl" => MacroTools @capture
@require "./deftype" exports...
using Base.Iterators

"""
Define partial application methods for `fn` for when its called with too few arguments
"""
macro curry(fn::Expr)
  @capture(MacroTools.longdef(fn), function name_(params__) body_ end)
  out = :(begin Base.@__doc__($(esc(fn))) end)
  name = esc(name)
  for (i, param) in enumerate(params)
    i ≡ endof(params) && break
    isoptional(params[i+1]) && break
    args = map(esc, params[1:i])
    push!(out.args, quote
      if !method_defined($name, [$(map(arg_type, params[1:i])...)])
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
Check if any methods are defined on `f` that would be ambiguous with `types`.
`method_exists` is similar but tests for applicability which is a larger set
of methods

```julia
method_defined(map, [Function, String]) # => false
method_exists(map, [Function, String]) # => true
```
"""
method_defined(f::Function, types::Any) = begin
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
An unsafe get
"""
Base.get(a, key) = begin
  a = get(a, key, Base.secret_table_token)
  a ≡ Base.secret_table_token && throw(KeyError(key))
  return a
end

"""
Default to `getfield`
"""
Base.get(object, key, default) = isdefined(object, key::Symbol) ? getfield(object, key) : default
Base.get(t::Tuple, i, default) = isdefined(t, i) ? getindex(t, i) : default

"""
Get a value deeply nested within an associative object
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
get_in(a, path) = foldl(get, a, path)

"""
Map `f` over `itr` and flatten the result one level
"""
mapcat(f::Function, itr) = begin
  foldl([], itr) do result, value
    foldl(push!, result, f(value))
  end
end

"""
Compose a series of functions into one which takes an input and runs it
sequentially through all the composed functions and returns the result
"""
compose(fns::Any...) = input -> foldl((x, f) -> f(x), input, fns)

"""
Create a copy of a collection with some elements added
"""
push(collection, items...) = reduce(push, collection, items)
push(a::AbstractArray, item) = push!(copy(a), item)
push{K,V}(d::Base.ImmutableDict{K,V}, p::Pair) = Base.ImmutableDict{K,V}(d, p[1], p[2])
push(dict::Associative, item::Pair) = push!(copy(dict), item)
push(object, pair::Pair) = assoc(object, pair[1], pair[2])

"""
Create a copy of a sequence with some elements added at the start
"""
unshift(collection, items...) = reduce(unshift, collection, items)
unshift(a::AbstractArray, item) = vcat(item, a)
unshift(a::AbstractArray, items...) = vcat(reverse(items)..., a)

"""
Create a copy of an `Associative` like structure with one key=>value pair altered
"""
assoc{K,V}(dict::Associative{K,V}, key::K, value::V) = push!(copy(dict), key=>value)
assoc{K,V,X,Y}(dict::Associative{K,V}, key::X, value::Y) = Dict(dict..., key=>value)
assoc{K,V}(d::Base.ImmutableDict{K,V}, key::K, value::V) = Base.ImmutableDict{K,V}(d, key, value)
assoc(arr::AbstractArray, i, value) = (arr = copy(arr); arr[i] = value; arr)
assoc(t::Tuple, i, value) = begin
  0 < i <= length(t) || throw(BoundsError(t, i))
  tuple(t[1:i-1]..., value, t[i+1:end]...)
end
assoc{T}(o::T, key, value) =
  T(map(f -> f ≡ key ? value : getfield(o, f), fieldnames(T))...)

"""
Add an association deep in the structure
"""
assoc_in(a::Any, kvs::Pair...) = reduce(assoc_in, a, kvs)
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
dissoc(dict::Associative, key) = delete!(copy(dict), key)
dissoc(dict::Associative, keys...) = foldl(delete!, copy(dict), keys)
dissoc(d::Base.ImmutableDict, key) = filter((k,v)-> k != key, d)
dissoc(a::AbstractArray, i) = deleteat!(copy(a), i)

"""
Remove an association deep in the structure
"""
dissoc_in(a, paths...) = reduce(dissoc_in, a, paths)
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
@curry transduce(fns::Vector, combine::Function) = foldr(partial, combine, fns)
@curry transduce(fns::Vector, combine::Function, accum, value) =
  foldr(partial, combine, fns)(accum, value)

##
# Define some basic transducers
#
@curry Base.map(f::Function, combine::Function, result, value) = combine(result, f(value))
@curry Base.filter(f::Function, combine::Function, result, value) =
  f(value) ? combine(result, value) : result
@curry mapcat(f::Function, combine::Function, result, value) = reduce(combine, result, f(value))

struct Field{name} end
(f::Field{name}){name}(object::Any) = getfield(object, name)
macro field_str(s) Field{Symbol(s)}() end
Base.get{name}(o::Any, f::Field{name}, default::Any) = isdefined(o, name) ? getfield(o, name) : default
Base.get{name}(o::Any, f::Field{name}) = getfield(o, name)

"""
Unpack a boxed value. If necessary it can wait for the value to become available
"""
need(x::Any) = x
need(n::Nullable) = get(n)
need(f::Future) = begin
  result = fetch(f)
  if isa(result, RemoteException)
    rethrow(result.captured.ex)
  else
    result
  end
end

"""
Instead of throwing an Error if no value is a available it will just return a
`default` value
"""
need(x::Any, default::Any) = try need(x) catch; default end
need(n::Nullable, default::Any) = get(n, default)
need(f::Future, default::Any) = (r = fetch(f); isa(r, RemoteException) ? default : r)

# this was left out of base for some reason
Base.filter(f::Function, d::Base.ImmutableDict) =
  reduce((d, pair) -> f(pair...) ? Base.ImmutableDict(d, pair) : d, typeof(d)(), d)

"""
Wait for one of several conditions to trigger. Returns a `Condition`
which will trigger with a `Tuple` of the first input to trigger along
with the value that it triggered with
"""
waitany(conditions...) = begin
  out = Condition()
  triggered = false
  for c in conditions
    @schedule try
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

export group, assoc, dissoc, compose, mapcat, flat,
       flatten, get_in, partial, @curry,
       transduce, method_defined, Field, @field_str,
       need, push, assoc_in, dissoc_in, unshift, @mutable,
       @struct, waitany, waitall
