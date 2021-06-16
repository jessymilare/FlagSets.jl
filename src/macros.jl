# This file is part of FlagSets.jl project, which is licensed under BDS-3-Clause.
# See file LICENSE.md for more information.

"""
    @symbol_flagset T[::BaseType] flag_1[=bit_1] flag_2[=bit_2] ...

Create a `FlagSet{Symbol,BaseType}` subtype with name `T` and the flags supplied.
Each instance of `T` represents a subset of `{:flag1, :flag2, ...}`.
The associated `bit_1`,`bit_2`, etc, if provided, must be distinct positive powers of 2.

The following constructors are provided:
    - `T(itr)`: construct a set of the symbols from the given iterable object. Each symbol
    must be one of :`flag_1`, :`flag_2`, etc;
    - `T()`: construct an empty set;
    - `T(sym1::Symbol, syms::Symbol...)`: equivalent to `T([sym1, syms...])`
    - `T(bitmask::Integer)`: construct a set of flags associated with each bit in `mask`;
    - `T(; kwargs...)`: each `flag_i` can be supplied as a boolean value
    (`flag_i = true` includes `:flag_i`, `flag_i = false` excludes it).

To construct sets of non-`Symbol` constants, use the macro [`@flagset`](@ref).

# Examples
```julia
julia> @symbol_flagset FontFlags bold=1 italic=2 large=4

julia> FontFlags(1)
FontFlags with 1 element:
  :bold

julia> FontFlags(:bold, :italic)
FontFlags with 2 elements:
  :bold
  :italic

julia> FontFlags(bold=true, italic=false)
FontFlags with 1 element:
  :bold

julia> for flag in FontFlags(3); println(flag) end
bold
italic

julia> FontFlags(3) | FontFlags(4)
FontFlags with 3 elements:
  :bold
  :italic
  :large

julia> eltype(FontFlags)
Symbol

julia> typemax(FontFlags)
FontFlags with 3 elements:
  :bold
  :italic
  :large

julia> typemin(FontFlags)
FontFlags([])
```

Values can also be specified inside a `begin` block, e.g.

```julia
@symbol_flagset T begin
    flag_1 [= x]
    flag_2 [= y]
end
```

If `BaseType` is provided, it must be an `Integer` type big enough to represent each
`bit_i`. Otherwise, some convenient integer type is inferred
(`UInt32` is used if possible, to guarantee compatibility with C code).
Eash instance of `T` can be converted to `BaseType`, where each bit set in the
converted `BaseType` corresponds to a flag in the flag set. `read` and `write`
perform these conversions automatically. In case the flagset is created with a non-default
`BaseType`, `Integer(flagset)` will return the integer `flagset` with the type `BaseType`.

```julia
julia> convert(Int, FontFlags(bold=true, italic=false))
1

julia> convert(Integer, FontFlags(bold=true, italic=false))
0x00000001
```

To list flags of a flag set type or instance, use `flags`, e.g.

```julia
julia> flags(FontFlags)
(:bold, :italic, :large)

julia> flags(FontFlags(3))
(:bold, :italic)
```
"""
macro symbol_flagset(typespec::Union{Symbol,Expr}, flagspecs...)
    expand_flagset(typespec, flagspecs, true, __module__)
end

"""
    @flagset T [key_1 =] [bit_1 -->] flag_1 [key_2 =] [bit_2 -->] flag_2...

Create a [`FlagSet`](@ref) subtype with name `T` and the flags supplied (evaluated).
Each instance of the created type represents a subset of `{flag_1, flag_2, ...}`.
The associated `bit_1`,`bit_2`, etc, if provided, must be distinct positive powers of 2.

The following constructors are created:
    - `T(itr)`: construct a set of the flags from the given iterable object. Each flag
    must be one of `flag_1`, `flag_2`, etc;
    - `T()`: construct an empty set;
    - `T(bitmask::Integer)`: construct a set of flags associated with each bit in `mask`;
    - `T(; kwargs...)`: each `key_i` can be given as a keyword with boolean value
    (`key_i = true` includes `flag_i`, `key_i = false` excludes it).

If all flags are `Symbol`s, the simpler macro [`@symbol_flagset`](@ref) can be used.
Flags can also be specified inside a `begin` block.

# Examples
```julia
julia> @flagset RoundingFlags begin
    down = RoundDown
    up = RoundUp
    near = 16 --> RoundNearest
    64 --> RoundNearestTiesUp  # No key
end

julia> RoundingFlags([RoundDown,RoundUp])
RoundingFlags with 2 elements:
  RoundingMode{:Down}()
  RoundingMode{:Up}()

julia> RoundingFlags(near = true, up = false)
RoundingFlags with 1 element:
  RoundingMode{:Nearest}()

julia> RoundingFlags(1)
RoundingFlags with 1 element:
  RoundingMode{:Down}()

julia> for flag in RoundingFlags(3); println(flag) end
RoundingMode{:Down}()
RoundingMode{:Up}()

julia> RoundingFlags(3) | RoundingFlags(16)
RoundingFlags with 3 elements:
  RoundingMode{:Down}()
  RoundingMode{:Up}()
  RoundingMode{:Nearest}()

julia> eltype(RoundingFlags)
RoundingMode

julia> typemax(RoundingFlags)
RoundingFlags with 4 elements:
  RoundingMode{:Down}()
  RoundingMode{:Up}()
  RoundingMode{:Nearest}()
  RoundingMode{:NearestTiesUp}()

julia> typemin(RoundingFlags)
RoundingFlags([])
```

The following syntaxes can be used to provide `FlagType` and/or `BaseType`:

```julia
@flagset T {FlagType,BaseType} ... # Use supplied types
@flagset T {FlagType} ...          # Detect BaseType
@flagset T {nothing,BaseType} ...  # Detect FlagType
@flagset T {} ...                  # Braces are ignored
```

If `FlagType` is not provided or is nothing, it is inferred from the flag values
(like array constructors do).
Otherwise, `flag_1`, `flag_2`, etc, must be of type `FlagType`.

If `BaseType` is provided, it must be an `Integer` type big enough to represent each
`bit_i`. Otherwise, some convenient integer type is inferred
(`UInt32` is used if possible, for maximum compatibility with C code).

Eash instance of `T` can be converted to `BaseType`, where each bit set in the
converted `BaseType` corresponds to a flag in the flag set. `read` and `write`
perform these conversions automatically. In case the flagset is created with a non-default
`BaseType`, `Integer(flagset)` will return the integer `flagset` with the type `BaseType`.

```julia
julia> convert(Int, RoundingFlags(up = true, near = true))
6

julia> convert(Integer, RoundingFlags(up = true, near = true))
0x00000006
```

To list flags of a flag set type or instance, use `flags`, e.g.

```julia
julia> flags(RoundingFlags)
(RoundingMode{:Down}(), RoundingMode{:Up}(), RoundingMode{:Nearest}())

julia> flags(RoundingFlags(3))
(RoundingMode{:Down}(), RoundingMode{:Up}())
```

!!! note

    For backward compatibility, the syntax `@flagset T::BaseType ...` is equivalent to
    `@symbol_flagset T::BaseType ...`, but is deprecated
    (use [`symbol_flagset`](@ref) instead).
"""
macro flagset(typespec::Union{Symbol,Expr}, flagspecs...)
    try
        return expand_flagset(typespec, flagspecs, false, __module__)
    catch ex
        if ex isa UndefVarError
            error(
                "An $ex has been caught during macroexpansion of @flagset. If you are " *
                "using the old syntax (before version 0.3), use @symbol_flagset instead.",
            )
        else
            throw(ex)
        end
    end
end

## Helper functions

function check_base_type(BaseType, typename)
    msg = "invalid base type for FlagSet $typename: $BaseType; should be an integer type"
    (isnothing(BaseType) || BaseType isa Type && BaseType <: Integer) || throw(ArgumentError(msg))
end

function check_flag_type(FlagType, typename)
    msg = "invalid flag type for FlagSet $typename: $FlagType"
    (isnothing(FlagType) || FlagType isa Type) || throw(ArgumentError(msg))
end

function parse_flag_set_type(typespec, of_type, symflags::Bool, __module__)
    BaseType = nothing
    FlagType = symflags ? Symbol : nothing
    typename = nothing
    if !isnothing(of_type)
        if typespec isa Symbol && 0 ≤ length(of_type.args) ≤ 2
            typename = typespec
            length(of_type.args) ≥ 1 && (FlagType = Core.eval(__module__, of_type.args[1]))
            length(of_type.args) ≥ 2 && (BaseType = Core.eval(__module__, of_type.args[2]))
            # else typename = nothing # indicate error
        end
    elseif (
        isa(typespec, Expr) &&
        typespec.head === :(::) &&
        length(typespec.args) == 2 &&
        isa(typespec.args[1], Symbol)
    )
        if !symflags
            @warn(
                "Deprecated syntax for macro @flagset. Use @symbol_flagset or use the syntax: " *
                "@flagset $(typespec.args[1]) {Symbol,$(typespec.args[2])}",
                maxlog = 1,
            )
        end
        FlagType = Symbol
        typename = typespec.args[1]
        BaseType = Core.eval(__module__, typespec.args[2])
    elseif isa(typespec, Symbol)
        typename = typespec
        # else typename = nothing
    end
    if isnothing(typename)
        throw(ArgumentError("invalid type expression for FlagSet: $typespec"))
    end
    check_flag_type(FlagType, typename)
    check_base_type(BaseType, typename)
    (typename, FlagType, BaseType, symflags)
end

function parse_flag_spec(typename, FlagType, flagspec, symflags::Bool, __module__)
    key, flag, bit = nothing, nothing, nothing
    if (
        isa(flagspec, Expr) &&
        (flagspec.head === :(=) || flagspec.head === :kw) &&
        length(flagspec.args) == 2
    )
        key = flagspec.args[1]
        flag_value = flagspec.args[2]
    elseif FlagType == Symbol && flagspec isa Symbol
        key = flag = flagspec
        return (key, flag, nothing)
    elseif symflags
        flag_value = nothing # indicates an error
    else
        flag_value = flagspec
    end
    if (isa(flag_value, Expr) && (flag_value.head === :-->) && length(flag_value.args) == 2)
        bit = Core.eval(__module__, flag_value.args[1]) # allow consts and exprs, e.g. uint128"1"
        flag = Core.eval(__module__, flag_value.args[2])
    else
        val = Core.eval(__module__, flag_value)
        flag, bit = FlagType == Symbol && !(val isa Symbol) ? (key, val) : (val, nothing)
    end
    if isnothing(flag_value) || !(key isa Union{Symbol,Nothing})
        throw(ArgumentError("invalid argument for FlagSet $typename: $flagspec"))
    end
    if !isnothing(bit)
        if !(bit isa Real && bit >= 1 && ispow2(bit))
            msg =
                "invalid bit for FlagSet $typename: $flagspec; " *
                "should be an integer positive power of 2"
            throw(ArgumentError(msg))
        end
    end
    (key, flag, bit)
end

function parse_flag_specs(typename, FlagType, BaseType, flagspecs, symflags::Bool, __module__)
    if length(flagspecs) == 1 && flagspecs[1] isa Expr && flagspecs[1].head === :block
        flagspecs = flagspecs[1].args
    end
    result = Any[]
    bit = one(BaseType)
    mask = zero(BaseType)
    flags = Set([])
    for flagspec in flagspecs
        flagspec isa LineNumberNode && continue
        (key, flag, next_bit) = parse_flag_spec(typename, FlagType, flagspec, symflags, __module__)
        bit::BaseType = something(next_bit, bit)
        if bit <= zero(BaseType)
            throw(ArgumentError("overflow in bit \"$flagspec\" of FlagSet $typename"))
        end
        flag::FlagType = flag
        if (bit & mask) != 0
            throw(ArgumentError("bits for FlagSet $typename are not unique"))
        end
        if flag in flags
            throw(ArgumentError("flag \"$flag\" in FlagSet $typename is not unique"))
        end
        push!(flags, flag)
        mask |= bit
        index = trailing_zeros(bit) + 1
        push!(result, (key, flag, bit, index))
        bit <<= 1
    end
    (sort!(result, by = t -> t[4]), mask)
end

function infer_types(FlagType, BaseType, parsed, mask)
    if isnothing(FlagType)
        # Construct an array from flags and let Julia tell us what is the best flag type
        FlagType = eltype(collect(p[2] for p ∈ parsed))
    end
    if isnothing(BaseType)
        BaseType = if mask ≤ typemax(UInt32)
            UInt32
        elseif mask ≤ typemax(UInt64)
            UInt64
        elseif mask ≤ typemax(UInt128)
            UInt128
        else
            BigInt
        end
    end
    (FlagType, BaseType)
end

function expand_flagset(typespec, flagspecs, symflags::Bool, __module__)
    of_type = nothing
    if !isempty(flagspecs) && first(flagspecs) isa Expr && first(flagspecs).head == :braces
        of_type = first(flagspecs)
        flagspecs = flagspecs[2:end]
    end
    if isempty(flagspecs)
        throw(ArgumentError("no arguments given for FlagSet $typespec"))
    end
    (typename, FlagType, BaseType, symflags) =
        parse_flag_set_type(typespec, of_type, symflags, __module__)

    FT = something(FlagType, Any)
    BT = something(BaseType, BigInt)
    parsed, mask = parse_flag_specs(typename, FT, BT, flagspecs, symflags, __module__)
    (FlagType, BaseType) = infer_types(FlagType, BaseType, parsed, mask)

    len = last(parsed)[4] # last index
    key_vector = Vector{Union{Symbol,Nothing}}(undef, len)
    fill!(key_vector, nothing) # Avoid undefined reference errors
    flag_vector = Vector{Union{FlagType,Nothing}}(undef, len)
    fill!(flag_vector, nothing) # Avoid undefined reference errors
    indices = BaseType[]
    flag_bit_map = Dict{FlagType,BaseType}()

    for (key, flag, bit, index) ∈ parsed
        key_vector[index] = key
        flag_vector[index] = flag
        flag_bit_map[flag] = bit
        push!(indices, index)
    end

    all_flags = Tuple(flag_vector)
    flags = Tuple(flag_vector[idx] for idx in indices)
    keys = Tuple(key_vector[idx] for idx in indices)
    keys_flags = ((key, flag) for (key, flag) ∈ zip(keys, flags) if !isnothing(key))

    blk = quote
        # flagset definition
        Base.@__doc__(struct $(esc(typename)) <: FlagSet{$FlagType,$BaseType}
                bitflags::$BaseType
                function $(esc(typename))(x::Integer)
                    x & $(mask) == x || flagset_argument_error($(Expr(:quote, typename)), x)
                    return new(convert($BaseType, x))
                end
            end)
        function $(esc(typename))(; $((Expr(:kw, :($key::Bool), false) for (key, _) ∈ keys_flags)...))
            xi::$BaseType = zero($BaseType)
            $((:($key && (xi |= $(flag_bit_map[flag]))) for (key, flag) ∈ keys_flags)...)
            $(esc(typename))(xi)
        end
        # Only create raw_constructor for symbols
        $(FlagType == Symbol && :(function $(esc(typename))(flag::$FlagType, flags::$FlagType...)
                $(esc(typename))((flag, flags...))
            end))
        function $(esc(typename))(itr::T) where {T}
            Base.isiterable(T) || flagset_argument_error($(Expr(:quote, typename)), itr)
            xi = zero($BaseType)
            for flag ∈ itr
                xi |= FlagSets.getflag($(esc(typename)), flag)
            end
            $(esc(typename))(xi)
        end

        FlagSets.all_flags(::Type{$(esc(typename))}) = $(esc(all_flags))
        FlagSets.flags(::Type{$(esc(typename))}) = $(esc(flags))
        FlagSets.flag_bit_map(::Type{$(esc(typename))}) = $(esc(flag_bit_map))
        Base.typemin(x::Type{$(esc(typename))}) = $(esc(typename))($(zero(BaseType)))
        Base.typemax(x::Type{$(esc(typename))}) = $(esc(typename))($mask)
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end
