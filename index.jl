##
# Recursively lift nested arrays/rows within an `array`
#
flat(a::Union(Vector,Tuple)) = vcat(map(flat, a)...)
flat(a::Array) = reshape(a, prod(size(a)))
flat(a) = a

##
# Lift nested arrays one level
#
flatten(a::Vector) = vcat(a...)

# A private unique value
const undefined = Dict()

##
# An unsafe get
#
function Base.get(a, key)
  a = get(a, key, undefined)
  a === undefined && error("can't get property '$key'")
  return a
end

##
# Get a value deeply nested within an associative object
# If no value is defined it will return default
#
function get_in(a, path, default)
  for key in path
    a = get(a, key, default)
    a === default && break
  end
  return a
end

##
# Like the 3 argument version except it throws if it
# the `path` is not defined
#
get_in(a, path) = foldl(get, a, path)

##
# Map `f` over `itr` and flatten the result one level
#
function mapcat(f::Function, itr::Union(AbstractArray,Tuple))
  foldl({}, itr) do result, value
    foldl(push!, result, f(value))
  end
end

##
# Compose a series of functions into one which takes an
# input and runs it sequentially through all the composed
# functions and returns the result
#
compose(fns::Function...) = input -> foldl((x, f) -> f(x), input, fns)
compose(fns...) = compose(flat(fns)...)

##
# Copy one stream to another
#
function Base.write(a::IO, b::IO)
  total = 0
  while !eof(b)
    total += write(a, read(b, Uint8))
  end
  total
end

##
# AsyncStream's in Julia don't do a very good job of being API
# compatible with other types of IO. TruncatedStream is an attempt
# to rectify this
#
type TruncatedStream <: IO
  stream::Base.AsyncStream
  nb::Int
  buff::Vector{Uint8}
  cursor::Int
end

Base.truncate(io::Base.AsyncStream, n::Integer) = TruncatedStream(io, n, Uint8[], 0)
Base.eof(io::TruncatedStream) = io.nb == 0
Base.read(io::TruncatedStream, ::Type{Uint8}) = begin
  io.nb -= 1
  if io.cursor == length(io.buff)
    io.buff = readavailable(io.stream).data
    io.cursor = 0
  end
  io.buff[io.cursor += 1]
end
# TODO: implement a decent `skip(::TruncatedStream, ::Int)`
