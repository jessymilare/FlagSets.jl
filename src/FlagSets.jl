# This file is part of FlagSets.jl project, which is licensed under BDS-3-Clause.
# See file LICENSE.md for more information.

module FlagSets

import Core.Intrinsics.bitcast
export FlagSet, @flagset, flags

function flagnames end
function flags end
function namemap end

abstract type FlagSet{T<:Integer} <: AbstractSet{Symbol} end

basetype(::Type{<:FlagSet{T}}) where {T<:Integer} = T

# Bits manipulation
(::Type{I})(x::FlagSet{T}) where {I<:Integer,T<:Integer} = I(bitcast(T, x))::I
Base.cconvert(::Type{I}, x::FlagSet{T}) where {I<:Integer,T<:Integer} = I(x)
Base.write(io::IO, x::FlagSet{T}) where {T<:Integer} = write(io, T(x))
Base.read(io::IO, ::Type{T}) where {T<:FlagSet} = T(read(io, basetype(T)))
#Base.isless(x::FlagSet{T}, y::FlagSet{T}) where {T<:Integer} = isless(T(x), T(y))
Base.:|(x::T, y::T) where {T<:FlagSet} = T(basetype(T)(x) | basetype(T)(y))
Base.:&(x::T, y::T) where {T<:FlagSet} = T(basetype(T)(x) & basetype(T)(y))
Base.:⊻(x::T, y::T) where {T<:FlagSet} = T(basetype(T)(x) ⊻ basetype(T)(y))
Base.:~(x::T) where {T<:FlagSet} = T(~basetype(T)(x))

# Iterator interface
function Base.iterate(x::FlagSet{T}) where {T<:Integer}
    xi = T(x)
    iterate(x, xi)
end

function Base.iterate(x::T, xi::Integer) where {T<:FlagSet}
    iszero(xi) && (return nothing)
    fs = flagnames(T)
    nbit = trailing_zeros(xi)
    xi ⊻= 1 << nbit
    return (fs[nbit+1], xi)
end

function Base.first(x::T) where {T<:FlagSet}
    xi = basetype(T)(x)
    iszero(xi) && throw(ArgumentError("collection must be non-empty"))
    fs = flagnames(T)
    nbit = trailing_zeros(xi)
    fs[nbit+1]
end

Base.length(x::FlagSet{T}) where {T<:Integer} = count_ones(T(x))
Base.isempty(x::FlagSet{T}) where {T<:Integer} = iszero(T(x))

"""
    flags(x::FlagSet)

Return the set of flags in `x` as a `Tuple`.

# Examples
```jldoctest fontflags
julia> @flagset FontFlags bold=1 italic=2 large=4

julia> flags(FontFlags(3))
(:bold, :italic)
```

"""
flags(x::FlagSet) = NTuple{length(x),Symbol}(x)

# Set manipulation
Base.union(x::T, y::T) where {T<:FlagSet} = T(basetype(T)(x) | basetype(T)(y))
Base.intersect(x::T, y::T) where {T<:FlagSet} = T(basetype(T)(x) & basetype(T)(y))
Base.setdiff(x::T, y::T) where {T<:FlagSet} = T(basetype(T)(x) & ~basetype(T)(y))
Base.issubset(x::FlagSet{T}, y::FlagSet{T}) where {T<:Integer} = (T(x) & T(y)) == T(x)
Base.in(elt::Symbol, x::T) where {T<:FlagSet} =
    !iszero(get(namemap(T), elt, 0) & basetype(T)(x))
Base.:⊊(x::FlagSet{T}, y::FlagSet{T}) where {T<:Integer} = x != y && (T(x) & T(y)) == T(x)

Base.empty(s::T, ::Type{Symbol} = Symbol) where {T<:FlagSet} = T()

# Printing
function Base.print(io::IO, x::FlagSet)
    compact = get(io, :compact, false)::Bool
    xi = Integer(x)
    print(io, typeof(x))
    if compact
        print(io, "(", xi, ")")
    else
        len = length(x)
        xt = flags(x)
        if len == 1
            print(io, "(")
            show(io, xt[1])
            print(io, ")")
        else
            print(io, xt)
        end
    end
end

function Base.show(io::IO, x::FlagSet)
    compact = get(io, :compact, false)::Bool
    xi = Integer(x)
    Base.show_datatype(io, typeof(x))
    if compact
        print(io, "(")
        show(io, xi)
        print(io, ")")
    else
        len = length(x)
        xt = flags(x)
        if len == 1
            print(io, "(")
            show(io, xt[1])
            print(io, ")")
        else
            print(io, xt)
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", x::FlagSet)
    show(io, x)
    if !get(io, :compact, false)
        print(io, " = ")
        show(io, Integer(x))
    end
end

# Printing FlagSet type
function Base.show(io::IO, m::MIME"text/plain", t::Type{<:FlagSet})
    if isconcretetype(t)
        print(io, "FlagSet ")
        Base.show_datatype(io, t)
        print(io, ":")
        xi = one(basetype(t))
        for sym ∈ flagnames(t)
            if sym != Symbol()
                print(io, "\n ", sym, " = ")
                show(io, xi)
            end
            xi <<= 1
        end
    else
        invoke(show, Tuple{IO,typeof(m),Type}, io, m, t)
    end
end

@noinline flagset_argument_error(typename, x) =
    throw(ArgumentError(string("invalid value for FlagSet $(typename): $x")))

"""
    @flagset FlagSetName[::BaseType] flag1[=x] flag2[=y]

Create a `FlagSet{BaseType}` subtype with name `FlagSetName` and flags `flag1` and `flag2`
with optional associated values of `x` and `y`, respectively, which must be positive
powers of 2.
Flag sets are implemented similarly to [`BitSet`](@ref), but its members are `Symbol`s.
Eash instance of `FlagSetName` represents a subset of `Set([:flag1, :flag2])`, where
- `FlagSetName(0) == FlagSetName()` represents the empty set `Set()`;
- `FlagSetName(x) == FlagSetName(:flag1)` represents `Set([:flag1])`;
- `FlagSetName(y) == FlagSetName(:flag2)` represents `Set([:flag2])`; and
- `FlagSetName(x | y) == FlagSetName(:flag1, :flag2)` represents `Set([:flag1, :flag2])`.

# Examples
```jldoctest fontflags
julia> @flagset FontFlags bold=1 italic=2 large=4

julia> FontFlags(1)
FontFlags(:bold) = 0x00000001

julia> FontFlags(:bold, :italic)
FontFlags(:bold, :italic) = 0x00000003

julia> for flag in FontFlags(3); println(flag) end
bold
italic

julia> FontFlags(3) | FontFlags(4)
FontFlags(:bold, :italic, :large) = 0x00000007
```

Values can also be specified inside a `begin` block, e.g.

```julia
@flagset FlagSetName begin
    flag1
    flag2
end
```

`BaseType`, which defaults to [`UInt32`](@ref), must be a primitive subtype of `Integer`.
Eash instance of `FlagSetName` can be converted to `BaseType`, where each bit set in the
converted `BaseType` corresponds to a flag in the flag set. `read` and `write`
perform these conversions automatically. In case the flagset is created with a non-default
`BaseType`, `Integer(flagset)` will return the integer `flagset` with the type `BaseType`.

To list flags of a flag set type or instance, use `flags`, e.g.

```jldoctest fontflags
julia> flags(FontFlags)
(:bold, :italic, :large)

julia> flags(FontFlags(3))
(:bold, :italic)
```
"""
macro flagset(T::Union{Symbol,Expr}, syms...)
    if isempty(syms)
        throw(ArgumentError("no arguments given for FlagSet $T"))
    end
    basetype = UInt32
    if isa(T, Expr) && T.head === :(::) && length(T.args) == 2 && isa(T.args[1], Symbol)
        typename = T.args[1]
        basetype = Core.eval(__module__, T.args[2])
        if !isa(basetype, DataType) || !(basetype <: Integer) || !isbitstype(basetype)
            throw(ArgumentError(
                "invalid base type for FlagSet $typename, ::$basetype; " *
                "base type must be an unsigned integer primitive type",
            ))
        end
    elseif isa(T, Symbol)
        typename = T
    else
        throw(ArgumentError("invalid type expression for bit flag $T"))
    end
    len = sizeof(basetype) * 8
    values = fill(Symbol(), len)
    values_end = 0
    nm = Dict{Symbol,basetype}()
    i = one(basetype)
    mask = zero(basetype)

    if length(syms) == 1 && syms[1] isa Expr && syms[1].head === :block
        syms = syms[1].args
    end
    for s in syms
        s isa LineNumberNode && continue
        if isa(s, Expr) &&
           (s.head === :(=) || s.head === :kw) &&
           length(s.args) == 2 &&
           isa(s.args[1], Symbol)
            i = Core.eval(__module__, s.args[2]) # allow exprs, e.g. uint128"1"
            if !isa(i, Integer)
                throw(ArgumentError(
                    "invalid value for FlagSet $typename, $s; " *
                    "values must be unsigned integers",
                ))
            end
            if !ispow2(i)
                throw(ArgumentError(
                    "invalid value for FlagSet $typename, $s; " *
                    "each value must be a positive power of 2",
                ))
            end
            i = convert(basetype, i)
            s = s.args[1]
        elseif !(s isa Symbol) || s == Symbol()
            throw(ArgumentError(string("invalid argument for FlagSet ", typename, ": ", s)))
        end
        if i <= zero(basetype)
            throw(ArgumentError("overflow in value \"$s\" of FlagSet $typename"))
        end
        s = s::Symbol
        if (i & mask) != 0
            throw(ArgumentError("values for FlagSet $typename are not unique"))
        end
        if haskey(nm, s)
            throw(ArgumentError("name \"$s\" in FlagSet $typename is not unique"))
        end
        nm[s] = i
        nbit = trailing_zeros(i)
        values[nbit+1] = s
        mask |= i
        values_end = max(values_end, nbit + 1)
        i <<= 1
    end
    fnames = Tuple(values[1:values_end])
    fnames_filtered = filter(!isequal(Symbol()), fnames)
    blk = quote
        # flagset definition
        Base.@__doc__(
            primitive type $(esc(typename)) <: FlagSet{$(basetype)} $(sizeof(basetype) * 8) end
        )
        function $(esc(typename))(x::Integer)
            x & $(mask) == x || flagset_argument_error($(Expr(:quote, typename)), x)
            return bitcast($(esc(typename)), convert($(basetype), x))
        end
        function $(esc(typename))(sym::Symbol, syms::Symbol...)
            xi::$(basetype) = get(FlagSets.namemap($(esc(typename))), sym) do
                flagset_argument_error($(Expr(:quote, typename)), sym)
            end
            for sym ∈ syms
                xi |= get(FlagSets.namemap($(esc(typename))), sym) do
                    flagset_argument_error($(Expr(:quote, typename)), sym)
                end
            end
            $(esc(typename))(xi)
        end
        $(esc(typename))() = $(esc(typename))($(zero(basetype)))
        function $(esc(typename))(itr)
            Base.isiterable(itr) || flagset_argument_error($(Expr(:quote, typename)), sym)
            $(esc(typename))(itr...)
        end

        FlagSets.flagnames(::Type{$(esc(typename))}) = $(esc(fnames))
        FlagSets.flags(::Type{$(esc(typename))}) = $(esc(fnames_filtered))
        FlagSets.namemap(::Type{$(esc(typename))}) = $(esc(nm))
        Base.typemin(x::Type{$(esc(typename))}) = $(esc(typename))($(zero(basetype)))
        Base.typemax(x::Type{$(esc(typename))}) = $(esc(typename))($mask)
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end

end # module
