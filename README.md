[![codecov](https://codecov.io/gh/jessymilare/FlagSets.jl/branch/master/graph/badge.svg?token=9BEVIJH08E)](https://codecov.io/gh/jessymilare/FlagSets.jl)
[![Build Status](https://travis-ci.com/jessymilare/FlagSets.jl.svg?branch=master)](https://travis-ci.com/jessymilare/FlagSets.jl)

# FlagSets.jl

`FlagSet.jl` provides an `Enum`-like type for bit flag option values. The main features
are:

1. Flags have implicit numbering with incrementing powers of 2.
2. Binary OR (`|`), AND (`&`) and XOR(`⊻`) operations are supported among members.
3. Set operations like `union`, `intersect`, `setdiff`, `in` and `issubset` are also
    supported.
4. Values are pretty-printed, showing the FlagSet type and each flag set.

This implementation is based on [BitFlags](https://github.com/jmert/BitFlags.jl), with
some differences:

1. Each flag set is treated as a set of flags and internally represented as an `Integer`
    (whose bits 1 corresponds to the flags in the set).
2. The empty flag set (corresponding to 0) is always valid.
3. The macros `@symbol_flagset` and `@flagset` don't create a constant for each flag.
4. Each flag can be represented by objects of arbitrary type (new in 0.3).

_Note:_ A breaking change has been introduced in version 0.3. The macro `@symbol_flagset`
corresponds to the old `@flagset` macro.


## Symbol flag sets

To create a new `FlagSet{Symbol}` type, use the `@flagset` macro, provide a name, an
optional integer type, and a list of the flag names (with optional associated bits).
A new definition can be given in inline form:
```julia
@symbol_flagset FlagSetName[::BaseType] flag_1[=bit_1] flag_2[=bit_2]
```
or as a block definition:
```julia
@symbol_flagset FlagSetName[::BaseType] begin
    flag_1[=bit_1]
    flag_2[=bit_2]
end
```

Automatic numbering starts at 1. In the following example, we build an 8-bit `FlagSet`
with no value for bit 3 (value of 4).
```julia
julia> @symbol_flagset FontFlags::UInt8 bold italic large=8
```

Instances can be created from integers or flag names and composed with bitwise operations.
Flag names can be symbols or keyword arguments.
```julia
julia> FontFlags(1)
FontFlags with 1 element:
  :bold

julia> FontFlags(:bold, :italic)
FontFlags with 2 elements:
  :bold
  :italic

julia> FontFlags(bold = true, italic = false, large = true)
FontFlags with 2 elements:
  :bold
  :large

julia> FontFlags(3) | FontFlags(8)
FontFlags with 3 elements:
  :bold
  :italic
  :large
```

Flag sets support iteration and other set operations
```julia
julia> for flag in FontFlags(3); println(flag) end
bold
italic

julia> :bold in FontFlags(3)
true
```

Conversion to and from integers is permitted, but only when the integer contains valid
bits for the flag set type.
```julia
julia> Int(FontFlags(:bold))
1

julia> Integer(FontFlags(:italic))    # Abstract Integer uses supplied UInt8 type
0x02

julia> FontFlags(9)
FontFlags with 2 elements:
  :bold
  :large

julia> FontFlags(4)
ERROR: ArgumentError: invalid value for FlagSet FontFlags: 4
Stacktrace:
...
```

The empty and the full set can be created with `typemin` and `typemax` (which is
consistent with these function definitions).
```julia
julia> typemin(FontFlags)
FontFlags() = 0x00

julia> typemax(FontFlags)
FontFlags(:bold, :italic, :large) = 0x0b
```


## Flag sets with custom flag type

The syntax of `@flagset` macro is similar (but incompatible) with `@symbol_flagset`.
These are the most important differences:

1. The flags are evaluated (during macroexpansion).
2. The creator from flag values is absent, used `T([flags...])` instead.
3. To provide an explicit associated bit, use the following syntax: `bit --> flag`.
4. To provide a key to be used as keyword argument, use the following syntax:
    `key = [bit -->] flag` (note that the key comes before the bit, if it is also supplied).
5. `FlagType` and/or `BaseType` can be supplied between braces.

In the example below, the type `RoundingFlags1` is created only from flag values.
```julia
julia> @flagset RoundingFlags1 RoundDown RoundUp RoundNearest

julia> RoundingFlags1([RoundDown, RoundUp])
RoundingFlags1 with 2 elements:
  RoundingMode{:Down}()
  RoundingMode{:Up}()

julia> Integer(RoundingFlags1([RoundDown, RoundUp]))
0x00000003
```

`RoundingFlags2` uses flags with custom bits for some flags:
```
julia> @flagset RoundingFlags2 RoundDown 4--> RoundUp 8 --> RoundNearest

julia> Integer(RoundingFlags2([RoundDown, RoundUp]))
0x00000005
```

`RoundingFlags3` also specifies keys for flags (except the last):
```
julia> @flagset RoundingFlags3 begin
    down = RoundDown
    up = RoundUp
    near = 16 --> RoundNearest
    64 --> RoundNearestTiesUp
end

julia> RoundingFlags3(up = true, near = true)
RoundingFlags3 with 2 elements:
  RoundingMode{:Up}()
  RoundingMode{:Nearest}()
```

## Printing

Printing a `FlagSet` subtype shows usefull information about it:
```julia
julia> FontFlags
FlagSet FontFlags:
 0x01 --> :bold
 0x02 --> :italic
 0x08 --> :large

julia> RoundingFlags3
FlagSet RoundingFlags3:
 0x00000001 --> RoundingMode{:Down}()
 0x00000002 --> RoundingMode{:Up}()
 0x00000010 --> RoundingMode{:Nearest}()
 0x00000040 --> RoundingMode{:NearestTiesUp}()
```

In a compact context (such as in multi-dimensional arrays), the pretty-printing
takes on a shorter form:
```julia
julia> [FontFlags(), FontFlags(:bold, :large)]
2-element Vector{FontFlags}:
 FontFlags([])
 FontFlags([:bold, :large])

julia> [RoundingFlags3() RoundingFlags3([RoundUp, RoundNearest])]
1×2 Matrix{RoundingFlags3}:
 RoundingFlags3(0x00000000)  RoundingFlags3(0x00000012)

julia> show(IOContext(stdout, :compact => true), FontFlags(:bold, :large))
FontFlags(0x09)
```
## Input/Output

`FlagSet`s support writing to and reading from streams as integers:
```julia
julia> io = IOBuffer();

julia> write(io, UInt8(9));

julia> seekstart(io);

julia> read(io, FontFlags)
FontFlags with 2 elements:
  :bold
  :large
```
