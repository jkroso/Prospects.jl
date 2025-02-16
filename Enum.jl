abstract type ScopedEnum{T} <: Enum{T} end

const enums = Dict{DataType,NamedTuple}()

Base.getproperty(T::Type{<:ScopedEnum}, sym::Symbol) = try getfield(enums[T], sym) catch; getfield(T, sym) end
Base.propertynames(T::Type{<:ScopedEnum}) = keys(enums[T])
Base.iterate(T::Type{<:ScopedEnum}) = iterate(values(enums[T]))
Base.iterate(T::Type{<:ScopedEnum}, state) = iterate(values(enums[T]), state)
Base.instances(T::Type{<:ScopedEnum}) = enums[T]
Base.convert(T::Type{<:ScopedEnum{N}}, n::Integer) where N = T(N(n))
Base.convert(T::Type{<:Integer}, n::ScopedEnum) = convert(T, n.value)

Base.nameof(e::ScopedEnum) = begin
  for (k,v) in pairs(instances(typeof(e)))
    v.value == e.value && return k
  end
end

Base.string(e::ScopedEnum) = string(nameof(e))
Base.print(io::IO, e::ScopedEnum) = show(io, e)
Base.print(io::IO, T::Type{<:ScopedEnum}) = show(io, T)

Base.show(io::IO, e::ScopedEnum) = begin
  write(io, nameof(typeof(e)), '.', nameof(e))
  nothing
end

Base.show(io::IO, T::Type{<:ScopedEnum}) = begin
  write(io, nameof(T), "::($(join(map(nameof, instances(T)), ',')))")
  nothing
end

setinstances!(::Type{T}, names::Vector{Symbol}, values=1:length(names)) where T<:ScopedEnum = begin
  enums[T] = NamedTuple{tuple(names...), NTuple{length(names), T}}(values)
end

macro Enum(name, instances...)
  T = esc(name)
  quote
    Core.@__doc__(struct $T <: ScopedEnum{UInt8} value::UInt8 end)
    setinstances!($T, [$(map(QuoteNode, instances)...)])
    $T
  end
end
