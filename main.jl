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
function Base.get(a, key)
  a = get(a, key, undefined)
  a ≡ undefined && error("can't get property: $key")
  return a
end

"""
Get a value deeply nested within an associative object
If no value is defined it will return default
"""
function get_in(a, path, default)
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
function mapcat(f::Function, itr)
  foldl([], itr) do result, value
    foldl(push!, result, f(value))
  end
end

"""
Compose a series of functions into one which takes an input and runs it
sequentially through all the composed functions and returns the result
"""
compose(fns::Function...) = input -> foldl((x, f) -> f(x), input, fns)
compose(fns...) = compose(flat(fns)...)

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

export group, assoc, dissoc, compose, mapcat, flat, flatten, get_in, TruncatedIO
