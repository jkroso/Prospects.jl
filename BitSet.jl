@use "./Enum.jl" ScopedEnum

"""
Takes the concept of BitFlags and adds the interface of an Enum and a Set
"""
abstract type BitSet{T} <: ScopedEnum{T} end

bestfit(max, T=UInt8) = (while typemax(T) < max; T = widen(T) end; T)

"""
Define a new BitSet subtype

```julia
@BitSet Keys cmd ctrl shft opt
@BitSet Keys "cmd ctrl shft opt 0:9"
```

When the second argument is a string, words are split on spaces and
ranges like `0:9` are expanded into individual names.
"""
macro BitSet(name, instances...)
  if length(instances) == 1 && instances[1] isa String
    instances = Symbol.(mapreduce(expand_word, vcat, split(instances[1]); init=String[]))
  end
  n = length(instances)
  @assert n <= 128 "Bitset only supports 128 unique values"
  NT = bestfit(big"2"^n)
  T = esc(name)
  storage = esc(gensym(:instances))
  values = Expr(:tuple, (0:n-1)...)
  quote
    Core.@__doc__(struct $T <: BitSet{$NT} value::$NT end)
    const $storage = NamedTuple{$(Expr(:tuple, map(QuoteNode, instances)...)), NTuple{$n, $NT}}($values)
    Base.instances(::Type{$T}) = $storage
    $T
  end
end

# Dispatch on `Type{<:BitSet}` directly rather than `::Type{T} where {N,T<:BitSet{N}}`.
# Julia 1.12 tightened `where`-clause resolution: the broader signature installs a
# method on `Base.getproperty(::Type, ::Symbol)` (one of the hottest functions in
# Base) and when dispatch considers it against an abstract `DataType` that isn't
# a concrete BitSet subtype, `N` ends up unbound and using it raises
# `UndefVarError(:T, scope=:static_parameter)`.
#
# `isconcretetype(T)` also calls `getproperty` internally on 1.12, which
# recurses back into this method. Avoid it — use `getfield` throughout and let
# construction failures fall through to the plain field lookup.
function Base.getproperty(T::Type{<:BitSet}, sym::Symbol)
  try
    N = getfield(T, :types)[1]
    T(N(1) << getfield(instances(T), sym))
  catch
    getfield(T, sym)
  end
end

Base.:|(a::T, b::T) where T<:BitSet = T(a.value|b.value)
Base.:&(a::T, b::T) where T<:BitSet = T(a.value&b.value)
Base.:⊻(a::T, b::T) where T<:BitSet = T(a.value⊻b.value)
Base.in(a::T, b::T) where T<:BitSet = a.value&b.value == a.value
Base.string(e::T) where {N,T<:BitSet{N}} = sprint(print, e)
Base.show(io::IO, b::BitSet) = show(io, MIME("text/plain"), b)
Base.show(io::IO, b::Type{<:BitSet}) = show(io, MIME("text/plain"), b)

Base.show(io::IO, ::MIME"text/plain", e::T) where T<:BitSet = begin
  names = collect(keys(instances(T)))[one_positions(e.value)]
  write(io, string(nameof(T)), '.', length(names) == 1 ? names[1] : "($(join(names, ',')))")
  nothing
end

Base.show(io::IO, ::MIME"text/plain", ::Type{T}) where T<:BitSet = begin
  names = collect(keys(instances(T)))
  write(io, string(nameof(T)), "::", length(names) == 1 ? first(names) : "($(join(names, ',')))")
  nothing
end

one_positions(n::T) where T<:Integer = [i+1 for i in 0:ndigits(n, base=2) if !iszero(n&(T(1)<<i))]

Base.length(::Type{T}) where T<:BitSet = length(instances(T))
Base.eltype(::Type{T}) where T<:BitSet = T
Base.iterate(::Type{T}, (i, values)=(1, instances(T))) where T<:BitSet = begin
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
  @assert b.value != 0 "empty bitset has no name"
  m = log2(b.value)+1
  @assert isinteger(m) "A BitSet composition has no single name"
  typeof(instances(T)).parameters[1][Int(m)]
end

expand_word(word) = begin
  m = match(r"^([a-zA-Z_]*)\(?([^)]+:[^)]+)\)?$", word)
  isnothing(m) && return [word]
  prefix = m.captures[1]
  parts = split(m.captures[2], ':')
  range = if all(s -> length(s) == 1 && isletter(s[1]), parts)
    parts[1][1]:parts[end][1]
  else
    nums = map(s -> parse(Int, s), parts)
    length(nums) == 2 ? (nums[1]:nums[2]) : (nums[1]:nums[2]:nums[3])
  end
  [string(prefix, i) for i in range]
end


