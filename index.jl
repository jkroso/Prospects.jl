##
# Recursively lift nested arrays/rows within an `array`
#
flat(a::Union(Vector,Tuple)) = vcat(map(flat, a)...)
flat(a::Array) = reshape(a, prod(size(a)))
flat(a) = a

test("flat") do
  @test flat(map(ones, [1,2,3])) == ones(6)
  @test flat({[1], [2,3]}) == [1,2,3]
  @test flat({[1], {2,[3]}}) == [1,2,3]
  @test flat([1 3; 2 4]) == [1,2,3,4]
  @test flat({[1 3; 2 4], {5,[6]}}) == [1,2,3,4,5,6]
  @test flat({[1], (2,3)}) == [1,2,3]
  @test flat({[1]}) == [1]
  @test flat([]) == []
end

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
  a ≡ undefined && error("can't get property: $key")
  return a
end

test("get(object, key)") do
  @test get({1=>2}, 1) == 2
  @test get([2], 1) == 2
  @test @catch(get({}, 1)).msg == "can't get property: 1"
end

##
# Get a value deeply nested within an associative object
# If no value is defined it will return default
#
function get_in(a, path, default)
  for key in path
    a = get(a, key, default)
    a ≡ default && break
  end
  return a
end

##
# Like the 3 argument version except it throws if it
# the `path` is not defined
#
get_in(a, path) = foldl(get, a, path)

test("get_in") do
  @test get_in({1=>2}, ()) == {1=>2}
  @test get_in({1=>2}, (1,)) == 2
  @test get_in({1=>{2=>3}}, (1,2)) == 3
  @test get_in({1=>{2=>[1,2,3]}}, (1,2,3)) == 3
  @test isa(@catch(get_in({}, (1,))), Exception)
end

##
# Map `f` over `itr` and flatten the result one level
#
function mapcat(f::Function, itr)
  foldl({}, itr) do result, value
    foldl(push!, result, f(value))
  end
end

test("mapcat") do
  @test mapcat(ones, []) == []
  @test mapcat(ones, [3]) == ones(3)
  @test mapcat(ones, [2,3]) == ones(5)
end

##
# Compose a series of functions into one which takes an input and runs it
# sequentially through all the composed functions and returns the result
#
compose(fns::Function...) = input -> foldl((x, f) -> f(x), input, fns)
compose(fns...) = compose(flat(fns)...)

test("compose") do
  @test compose(vcat)(1) == [1]
  @test compose(iseven, vcat)(1) == [false]
  @test compose(ones, prod, vcat)(3) == [1]
  @test compose(((ones,),), prod, vcat)(3) == [1]
end

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

Base.|>(from::IO,to::IO) = (write(to, from); to)

test("stream piping") do
  open(tempname(), "w+") do file
    @test write(file, IOBuffer("abc")) == 3
    @test IOBuffer("def") |> file === file
    seekstart(file)
    @test readall(file) == "abcdef"
  end
end

##
# Provide a way of limiting the length of a stream
#
type TruncatedIO <: IO
  io::IO
  nb::Int
end

Base.truncate(io::IO, n::Integer) = TruncatedIO(io, n)
Base.eof(io::TruncatedIO) = io.nb == 0
Base.read(io::TruncatedIO, ::Type{Uint8}) = begin
  io.nb -= 1
  read(io.io, Uint8)
end

test("TruncatedIO") do
  io = open("index.jl")
  head = TruncatedIO(io, 100)
  readall(head) == readall("index.jl")[1:100]
  close(io)
end

##
# AsyncStream's in Julia don't do a very good job of being API
# compatible with other types of IO. AsyncIO is an attempt
# to rectify this
#
type AsyncIO <: IO
  stream::Base.AsyncStream
  nb::Int
  buff::Vector{Uint8}
  cursor::Int
end

Base.truncate(io::Base.AsyncStream, n::Integer) = AsyncIO(io, n, Uint8[], 0)
Base.eof(io::AsyncIO) = io.nb == 0
Base.read(io::AsyncIO, ::Type{Uint8}) = begin
  io.nb -= 1
  if io.cursor == length(io.buff)
    io.buff = readavailable(io.stream).data
    io.cursor = 0
  end
  io.buff[io.cursor += 1]
end

# TODO: implement a decent `skip(::AsyncIO, ::Int)`
