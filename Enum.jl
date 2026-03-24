abstract type ScopedEnum{T} <: Enum{T} end

Base.getproperty(T::Type{<:ScopedEnum}, sym::Symbol) = try getfield(instances(T), sym) catch; getfield(T, sym) end
Base.propertynames(T::Type{<:ScopedEnum}) = keys(instances(T))
Base.iterate(T::Type{<:ScopedEnum}) = iterate(values(instances(T)))
Base.iterate(T::Type{<:ScopedEnum}, state) = iterate(values(instances(T)), state)
Base.Enums.namemap(T::Type{<:ScopedEnum}) = Dict(Int(v.value) => k for (k,v) in pairs(instances(T)))
Base.typemin(T::Type{<:ScopedEnum}) = first(values(instances(T)))
Base.typemax(T::Type{<:ScopedEnum}) = last(values(instances(T)))
Base.convert(T::Type{<:ScopedEnum{N}}, n::Integer) where N = T(N(n))
Base.convert(T::Type{<:Integer}, n::ScopedEnum) = convert(T, n.value)
Base.Integer(e::ScopedEnum) = Int(e.value)

Base.nameof(e::ScopedEnum) = begin
  for (k,v) in pairs(instances(typeof(e)))
    v.value == e.value && return k
  end
end

Base.Symbol(e::ScopedEnum) = nameof(e)
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

macro Enum(name, instances...)
  T = esc(name)
  names_tuple = Expr(:tuple, map(QuoteNode, instances)...)
  n = length(instances)
  storage = esc(gensym(:instances))
  quote
    Core.@__doc__(struct $T <: ScopedEnum{UInt8} value::UInt8 end)
    const $storage = NamedTuple{$names_tuple, NTuple{$n, $T}}($(1:n))
    Base.instances(::Type{$T}) = $storage
    $T
  end
end
