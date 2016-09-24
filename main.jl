"""
Define partial application methods for `fn` for when its called with too few arguments
"""
macro curry(fn::Expr)
  params = fn.args[1].args
  name = esc(params[1])
  # escape Type annotations
  params = map(params[2:end]) do p
    !isa(p, Expr) && return p
    p.head != :(::) && return p
    Expr(p.head, p.args[1], esc(p.args[2]))
  end
  out = :(begin $(esc(fn)) end)
  for (i, param) in enumerate(params)
    i == endof(params) && break
    isoptional(params[i+1]) && break
    args = params[1:i]
    push!(out.args, :($name($(args...)) = partial($name, $(args...))))
  end
  out
end

isoptional(param) = isa(param, Expr) && param.head == :kw

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

# A private unique value
const undefined = gensym()

"""
An unsafe get
"""
Base.get(a, key) = begin
  a = get(a, key, undefined)
  a ≡ undefined && error("can't get property: $key")
  return a
end

"""
Get a value deeply nested within an associative object
If no value is defined it will return default
"""
get_in(a, path, default) = begin
  for key in path
    a = get(a, key, default)
    a ≡ default && break
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

Base.:(|>)(from::IO,to::IO) = (write(to, from); to)

"""
Provide a way of limiting the length of a stream
"""
type TruncatedIO <: IO
  io::IO
  nb::Int
end

Base.truncate(io::IO, n::Integer) = TruncatedIO(io, n)
Base.eof(io::TruncatedIO) = io.nb == 0
Base.read(io::TruncatedIO, ::Type{UInt8}) = begin
  io.nb -= 1
  read(io.io, UInt8)
end

"""
Create a mutated copy of some Associative like object
"""
assoc(dict::Associative, pairs::Pair...) = push!(copy(dict), pairs...)
assoc(object, pairs::Pair...) = begin
  typ = typeof(object)
  dict = Dict(pairs...)
  vals = map(name -> get(dict, name, getfield(object, name)), fieldnames(typ))
  typ(vals...)
end

dissoc(dict::Associative, key) = delete!(copy(dict), key)
dissoc(dict::Associative, keys...) = foldl(delete!, copy(dict), keys)

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

export group, assoc, dissoc, compose, mapcat, flat,
       flatten, get_in, TruncatedIO, partial, @curry
