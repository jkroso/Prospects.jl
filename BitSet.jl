@use "./Enum.jl" ScopedEnum enums setinstances!

"""
Takes the concept of BitFlags and adds the interface of an Enum and a Set
"""
abstract type BitSet{T} <: ScopedEnum{T} end

bestfit(max, T=UInt8) = (while typemax(T) < max; T = widen(T) end; T)

"""
Define a new BitSet subtype `@BitSet <Name>[::InternalType] [instance names...]`

```julia
@BitSet Keys cmd ctrl shft opt
# With optional internal type specifier which is useful if you want to generate the instance names seperatly
@BitSet Keys::UInt8
```
"""
macro BitSet(name, instances...)
  n = length(instances)
  @assert n <= 128 "Bitset only supports 128 unique values"
  if Meta.isexpr(name, :(::))
    NT = eval(@__MODULE__, name.args[2])
    name = name.args[1]
  else
    NT = bestfit(big"2"^n)
  end
  T = esc(name)
  quote
    Core.@__doc__(struct $T <: BitSet{$NT} value::$NT end)
    setinstances!($T, Symbol[$(map(QuoteNode, instances)...)])
    $T
  end
end

Base.getproperty(::Type{T}, sym::Symbol) where {N,T<:BitSet{N}} = try
  T(N(1) << getfield(enums[T], sym))
catch
  getfield(T, sym)
end

setinstances!(::Type{T}, names::Vector{Symbol}, values=0:length(names)-1) where T<:BitSet = begin
  enums[T] = NamedTuple{tuple(names...), NTuple{length(names),bestfit(length(names))}}(tuple(values...))
end

Base.:|(a::T, b::T) where T<:BitSet = T(a.value|b.value)
Base.:&(a::T, b::T) where T<:BitSet = T(a.value&b.value)
Base.:⊻(a::T, b::T) where T<:BitSet = T(a.value⊻b.value)
Base.in(a::T, b::T) where T<:BitSet = a.value&b.value == a.value
Base.string(e::T) where {N,T<:BitSet{N}} = sprint(print, e)
Base.print(io::IO, b::BitSet) = show(io, MIME("text/plain"), b)

Base.show(io::IO, ::MIME"text/plain", e::T) where T<:BitSet = begin
  names = typeof(enums[T]).parameters[1][one_positions(e.value)]
  write(io, string(nameof(T)), '.', length(names) == 1 ? names[1] : "($(join(names, ',')))")
end

Base.show(io::IO, ::MIME"text/plain", ::Type{T}) where T<:BitSet = begin
  names = typeof(enums[T]).parameters[1]
  write(io, string(nameof(T)), "::", length(names) == 1 ? names[1] : "($(join(names, ',')))")
end

one_positions(n::T) where T<:Integer = [i+1 for i in 0:ndigits(n, base=2) if !iszero(n&(T(1)<<i))]

Base.length(::Type{T}) where T<:BitSet = length(enums[T])
Base.eltype(::Type{T}) where T<:BitSet = T
Base.iterate(::Type{T}, (i, values)=(1, enums[T])) where T<:BitSet = begin
  i > length(values) ? nothing : (values[i], (i+1, values))
end

Base.length(b::T) where T<:BitSet = count_ones(b.value)
Base.eltype(::T) where T<:BitSet = T
Base.iterate(b::T, state=b.value) where T<:BitSet = begin
  state == 0 && return nothing
  v = 1<<trailing_zeros(state)
  T(v), state ⊻ v
end

Base.union(a::T, b::T) where T<:BitSet = T(a.value|b.value)
Base.intersect(a::T, b::T) where T<:BitSet = T(a.value&b.value)
Base.symdiff(a::T, b::T) where T<:BitSet = T(a.value⊻b.value)
Base.setdiff(a::T, b::T) where T<:BitSet = T(a.value&(~b.value))
Base.issubset(a::T, b::T) where T<:BitSet = a in b
Base.filter(f, b::T) where T<:BitSet = begin
  n = b.value
  for x in b
    f(x) || (b = b ⊻ x)
  end
  b
end

Base.:(:)(a::T, b::T) where T<:BitSet = begin
  start = trailing_zeros(a.value)+1
  stop = trailing_zeros(b.value)+1
  @assert start <= stop
  mask=((1<<(stop-start+1))-1)<<(start-1)
  T(a.value|mask)
end

Base.nameof(b::T) where T<:BitSet = begin
  m = log2(b.value)
  @assert isinteger(m) "A BitSet composition has no single name"
  typeof(enums[T]).parameters[1][Int(m)+1]
end
