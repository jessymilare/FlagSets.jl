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
Each instance of `T` can be converted to `BaseType`, where each bit set in the
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

julia> RoundingFlags(near = true, up = false, RoundNearestTiesUp = true)
RoundingFlags with 2 elements:
  RoundingMode{:Nearest}()
  RoundingMode{:NearestTiesUp}()

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

If some `flag_i` is a `Symbol` and `key_i` is not explicitly given, `key_i` is set to
`flag_i`. Therefore, both macros below are equivalent:
```
@flagset FontFlags :bold :italic 8 --> :large
@flagset FontFlags begin
    bold = :bold
    italic = :italic
    large = 8 --> :large
end
```

The following syntaxes can be used to provide `FlagType` and/or `BaseType`:

```julia
@flagset T {FlagType,BaseType} ... # Use supplied types
@flagset T {FlagType} ...          # Detect BaseType
@flagset T {FlagType,_} ...        # Detect BaseType too
@flagset T {_,BaseType} ...        # Detect FlagType
@flagset T {} ...                  # Braces are ignored
```

If `FlagType` is not provided or is `_`, it is inferred from the flag values
(like array constructors do).
Otherwise, `flag_1`, `flag_2`, etc, must be of type `FlagType`.

If `BaseType` is not provided or is `_`, some convenient integer type is inferred
(`UInt32` is used if possible, for maximum compatibility with C code).
Otherwise, it must be an `Integer` type big enough to represent each `bit_i`.

Each instance of `T` can be converted to `BaseType`, where each bit set in the
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
    `@symbol_flagset T::BaseType ...`, but the former syntax deprecated
    (use [`symbol_flagset`](@ref) instead).
"""
macro flagset(typespec::Union{Symbol,Expr}, flagspecs...)
    try
        return expand_flagset(typespec, flagspecs, false, __module__)
    catch ex
        throw(ex isa UndefVarError ? undef_var_error_hint(ex) : ex)
    end
end

## Error creation

function undef_var_error_hint(ex)
    ErrorException(
        "An $ex has been caught during macroexpansion of @flagset. If you are using " *
        "the old syntax (before version 0.3), use @symbol_flagset or the following syntax:" *
        "\n @flagset T [bit_1 -->] :flag_1 [bit_2 -->] :flag_2 ..."
    )
end

function deprecated_syntax_message(typename, BaseType)
    ArgumentError(
        "Deprecated syntax for macro @flagset. Use @symbol_flagset or use the syntax:" *
        "\n @flagset $typename {Symbol,$BaseType)} [bit_1 -->] :flag_1 [bit_2 -->] :flag_2 ..."
    )
end

function invalid_bit_error(typename, flagspec)
    ArgumentError("invalid bit for FlagSet $typename: $flagspec; should be an integer positive power of 2")
end

function invalid_flagspec_error(typename, flagspec)
    ArgumentError("invalid flag argument for FlagSet $typename: $flagspec")
end

function overflow_error(typename, flagspec)
    ArgumentError("overflow in bit of flag $flagspec for FlagSet $typename")
end

function not_unique_error(field::Symbol, typename, value)
    ArgumentError("$field for FlagSet $typename is not unique: $value")
end

function invalid_type_error(kind::Symbol, typename, type)
    ArgumentError("invalid $kind type for FlagSet $typename: $type")
end

function check_flag_type(typename, FlagType)
    (isnothing(FlagType) || FlagType isa Type) ||
        throw(invalid_type_error(:flag, typename, FlagType))
end

function check_base_type(typename, BaseType)
    (isnothing(BaseType) || BaseType isa Type && BaseType <: Integer) ||
        throw(invalid_type_error(:base, typename, BaseType))
end

## Helper functions

function _to_nothing(expr, __module__)
    expr == :_ ? nothing : Core.eval(__module__, expr)
end

function parse_flag_set_type(typespec, of_type, symflags::Bool, __module__)
    BaseType = nothing
    FlagType = symflags ? Symbol : nothing
    typename = nothing
    if !isnothing(of_type)
        if typespec isa Symbol && 0 ≤ length(of_type.args) ≤ 2
            typename = typespec
            length(of_type.args) ≥ 1 && (FlagType = _to_nothing(of_type.args[1], __module__))
            length(of_type.args) ≥ 2 && (BaseType = _to_nothing(of_type.args[2], __module__))
            # else typename = nothing # indicate error
        end
    elseif (
        isa(typespec, Expr) &&
        typespec.head === :(::) &&
        length(typespec.args) == 2 &&
        isa(typespec.args[1], Symbol)
    )
        symflags || @warn(deprecated_syntax_message(typespec.args[1], typespec.args[2]), maxlog = 1)
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
    check_flag_type(typename, FlagType)
    check_base_type(typename, BaseType)
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
        flag_expr = flag_value.args[2]
        flag = Core.eval(__module__, flag_expr)
    else
        flag_expr = flag_value
        val = Core.eval(__module__, flag_value)
        flag, bit = FlagType == Symbol && !(val isa Symbol) ? (key, val) : (val, nothing)
    end
    if isnothing(key)
        key = flag isa Symbol && Base.isidentifier(flag) && flag != :missing ? flag : nothing
    end
    if isnothing(flag_value) || !(key isa Union{Symbol,Nothing})
        throw(invalid_flagspec_error(typename, flagspec))
    end
    if !isnothing(bit)
        (bit isa Real && bit >= 1 && ispow2(bit)) || throw(invalid_bit_error(typename, flagspec))
    end
    (key, flag, bit)
end

function parse_flag_specs(typename, FlagType, BaseType, flagspecs, symflags::Bool, __module__)
    if length(flagspecs) == 1 && flagspecs[1] isa Expr && flagspecs[1].head === :block
        flagspecs = flagspecs[1].args
    end
    flagspecs = filter(fs -> !(fs isa LineNumberNode), flagspecs)
    result = Any[]
    bit = one(BaseType)
    mask = zero(BaseType)
    flags = Set([])
    keys = Set([])
    for flagspec in flagspecs
        (key, flag, next_bit) = parse_flag_spec(typename, FlagType, flagspec, symflags, __module__)
        bit::BaseType = something(next_bit, bit)
        bit <= zero(BaseType) && throw(overflow_error(typename, flagspec))
        flag::FlagType = flag
        (bit & mask) != 0 && throw(not_unique_error(:bit, typename, bit))
        flag in flags && throw(not_unique_error(:flag, typename, flag))
        key in keys && throw(not_unique_error(:key, typename, key))
        push!(flags, flag)
        isnothing(key) || push!(keys, key)
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

    flagset_flags = Tuple(flag_vector)
    flags = Tuple(flag_vector[idx] for idx in indices)
    keys = Tuple(key_vector[idx] for idx in indices)
    keys_flags = ((key, flag) for (key, flag) ∈ zip(keys, flags) if !isnothing(key))

    @show flags keys (keys_flags...,)

    blk = quote
        # flagset definition
        Base.@__doc__(struct $(esc(typename)) <: FlagSet{$FlagType,$BaseType}
                bitflags::$BaseType
                function $(esc(typename))(bitmask::Integer)
                    bitmask & $(mask) == bitmask || flagset_argument_error($(esc(typename)), bitmask)
                    return new(convert($BaseType, bitmask))
                end
            end)
        function $(esc(typename))(; $((Expr(:kw, :($key::Bool), false) for (key, _) ∈ keys_flags)...))
            bitmask::$BaseType = zero($BaseType)
            $((:($key && (bitmask |= $(flag_bit_map[flag]))) for (key, flag) ∈ keys_flags)...)
            $(esc(typename))(bitmask)
        end
        # Only create raw_constructor for symbols
        # $(FlagType == Symbol && :(function $(esc(typename))(flag::$FlagType, flags::$FlagType...)
        #         $(esc(typename))((flag, flags...))
        #     end))
        # function $(esc(typename))(itr::T) where {T}
        #     Base.isiterable(T) || flagset_argument_error($(Expr(:quote, typename)), itr)
        #     bitmask = zero($BaseType)
        #     for flag ∈ itr
        #         bitmask |= FlagSets.get_flag_bit($(esc(typename)), flag)
        #     end
        #     $(esc(typename))(bitmask)
        # end

        FlagSets.flagset_flags(::Type{$(esc(typename))}) = $(esc(flagset_flags))
        FlagSets.flags(::Type{$(esc(typename))}) = $(esc(flags))
        FlagSets.flag_bit_map(::Type{$(esc(typename))}) = $(esc(flag_bit_map))
        FlagSets.flagkeys(::Type{$(esc(typename))}) =
            $(keys === flags ? :(FlagSets.flags($(esc(typename)))) : esc(keys))
        Base.typemin(x::Type{$(esc(typename))}) = $(esc(typename))($(zero(BaseType)))
        Base.typemax(x::Type{$(esc(typename))}) = $(esc(typename))($mask)
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end
