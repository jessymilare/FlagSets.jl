# This file is part of FlagSets.jl project, which is licensed under BDS-3-Clause.
# See file LICENSE.md for more information.

function flagset_flags end
function flag_bit_map end
function flags end
function flagkeys end

"""
    BitMask(bitmask::Integer)

Structure used internally by `FlagSets` for default constructor of subtypes of `FlagSet`.
"""
struct BitMask{T<:Integer}
    bitmask::T
end

basetype(::Type{<:FlagSet{F,B}}) where {F,B<:Integer} = B

Base.isvalid(::Type{T}, x::Integer) where {T<:FlagSet} = (x & basetype(T)(typemax(T)) == x)
Base.isvalid(::Type{T}, x::BitMask) where {T<:FlagSet} = (x.bitmask & basetype(T)(typemax(T)) == x.bitmask)
Base.isvalid(::Type{T}, x::T) where {T<:FlagSet} = true
Base.isvalid(x::T) where {T<:FlagSet} = true

function Base.isvalid(::Type{T}, x::S) where {S,T<:FlagSet}
    Base.isiterable(S) && all(elt ∈ flags(T) for elt ∈ x)
end

function Base.isvalid(::Type{T}, x::Symbol) where {T<:FlagSet{Symbol}}
    x ∈ flags(T)
end

(::Type{I})(x::FlagSet) where {I<:Integer} = I(x.bitflags)::I
Base.cconvert(::Type{I}, x::FlagSet) where {I<:Integer} = I(x.bitflags)
Base.write(io::IO, x::T) where {T<:FlagSet} = write(io, x.bitflags)
Base.read(io::IO, ::Type{T}) where {T<:FlagSet} = T(BitMask(read(io, basetype(T))))
#Base.isless(x::FlagSet{T}, y::FlagSet{T}) where {T<:Integer} = isless(T(x), T(y))
Base.:|(x::T, y::T) where {T<:FlagSet} = T(BitMask(x.bitflags | y.bitflags))
Base.:&(x::T, y::T) where {T<:FlagSet} = T(BitMask(x.bitflags & y.bitflags))
Base.:⊻(x::T, y::T) where {T<:FlagSet} = T(BitMask(x.bitflags ⊻ y.bitflags))
Base.:~(x::T) where {T<:FlagSet} = setdiff(typemax(T), x)
Base.iszero(x::FlagSet) = isempty(x)

# Iterator interface

function Base.iterate(x::T, xi::Integer = x.bitflags) where {T<:FlagSet}
    iszero(xi) && (return nothing)
    fs = flagset_flags(T)
    nbit = trailing_zeros(xi)
    xi ⊻= typeof(xi)(1) << nbit
    return (fs[nbit+1], xi)
end

Base.length(x::FlagSet) = count_ones(x.bitflags)
Base.isempty(x::FlagSet) = iszero(x.bitflags)

"""
    flags(x::FlagSet)

Return the set of flags in `x` as a `Tuple`.

# Examples
```julia
julia> @symbol_flagset FontFlags bold=1 italic=2 large=4

julia> flags(FontFlags(3))
(:bold, :italic)
```
"""
flags(x::T) where {T<:FlagSet} = NTuple{length(x),eltype(T)}(x)

"""
    flags(T)

Return the set of flags of type `T` as a `Tuple`.

# Examples
```julia
julia> @flagset RoundingFlags RoundDown RoundUp RoundNearest

julia> flags(RoundingFlags)
(RoundingMode{:Down}(), RoundingMode{:Up}(), RoundingMode{:Nearest}())
```
"""
flags(::Type{<:FlagSet})

# Set manipulation
Base.union(x::T, y::T) where {T<:FlagSet} = T(BitMask(x.bitflags | y.bitflags))
Base.intersect(x::T, y::T) where {T<:FlagSet} = T(BitMask(x.bitflags & y.bitflags))
Base.setdiff(x::T, y::T) where {T<:FlagSet} = T(BitMask(x.bitflags & ~y.bitflags))
Base.issubset(x::T, y::T) where {T<:FlagSet} = (x.bitflags & y.bitflags) == x.bitflags
Base.in(elt, x::T) where {T<:FlagSet} = !iszero(get_flag_bit(T, elt, zero(basetype(T))) & x.bitflags)
Base.:⊊(x::T, y::T) where {T<:FlagSet} = x != y && (x.bitflags & y.bitflags) == x.bitflags

Base.empty(s::FlagSet{B,F}, ::Type{F}) where {B,F} = typeof(s)()
Base.empty(s::FlagSet) = typeof(s)()

# Printing
function Base.show(io::IO, x::T) where {T<:FlagSet}
    compact = get(io, :compact, false)::Bool
    type_p = (get(io, :typeinfo, false) == T)::Bool
    malformed = !isvalid(x)
    if compact || malformed
        xi = Integer(x)
        type_p &= malformed
        type_p || show(io, T)
        type_p || print(io, "(")
        show(io, xi)
        type_p || print(io, malformed ? " #= Invalid code =#)" : ")")
    else
        show(io, T)
        print(io, "([")
        join(io, repr.(x), ", ")
        print(io, "])")
    end
end

function Base.show(io::IO, mime::MIME"text/plain", x::FlagSet)
    if get(io, :compact, false)
        show(io, x)
    else
        invoke(show, Tuple{typeof(io),typeof(mime),AbstractSet}, io, mime,  x)
    end
end

# Printing FlagSet type
function Base.show(io::IO, mime::MIME"text/plain", type::Type{<:FlagSet})
    if isconcretetype(type) && !(get(io, :compact, false))
        print(io, "FlagSet ")
        Base.show_datatype(io, type)
        print(io, ":")
        keys = map(key -> isnothing(key) ? "" : String(key), flagkeys(type))
        if !all(isempty, keys)
            klen = maximum(length, keys)
            keys = map(k -> isempty(k) ? ' '^(klen+2) : k * ' '^(klen-length(k)) * " = ", keys)
        end
        for (flag, key) ∈ zip(flags(type), keys)
            bit = get_flag_bit(type, flag)
            print(io, "\n ", key, repr(bit), " --> ", repr(flag))
        end
    else
        invoke(show, Tuple{typeof(io),typeof(mime),Type}, io, mime, type)
    end
end

function flagset_argument_error(typename, x)
    throw(ArgumentError(string("invalid value for FlagSet $(typename): ", repr(x))))
end

function get_flag_bit(::Type{T}, flag, default) where {T<:FlagSet}
    get(flag_bit_map(T), flag, default)
end

function get_flag_bit(::Type{T}, flag) where {T<:FlagSet}
    not_found = basetype(T)(0x7f)
    val = get(flag_bit_map(T), flag, not_found)
    val == not_found && flagset_argument_error(T, flag)
    val
end

# Default constructors

function (::Type{T})(flag::Symbol, flags::Symbol...) where {T<:FlagSet{Symbol}}
    T((flag, flags...))
end

function (::Type{T})(itr) where {T<:FlagSet}
    Base.isiterable(typeof(itr)) || flagset_argument_error(T, itr)
    bitmask::basetype(T) = 0
    for flag ∈ itr
        bitmask |= get_flag_bit(T, flag)
    end
    T(BitMask(bitmask))
end

function (::Type{T})(bitmask::Integer) where {T<:FlagSet}
    T(BitMask(bitmask))
end
