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
corresponds to the old `@flagset` macro and the latter has been made more general.


## Symbol flag sets

To create a new `FlagSet{Symbol}` type, you can use the `@symbol_flagset` macro
(old syntax) or the more general and flexible `@flagset` macro.
You need to provide a type name, an optional integer type, and a list of the flag
names (with optional associated bits).
A new definition can be given in inline form:
```julia
@symbol_flagset FlagSetName[::BaseType] flag_1[=bit_1] flag_2[=bit_2] ...

@flagset FlagSetName [{Symbol,BaseType}] [bit_1 -->] :flag_1 [bit_2 -->] :flag_2 ...
```
or as a block definition:
```julia
@symbol_flagset FlagSetName[::BaseType] begin
    flag_1[=bit_1]
    flag_2[=bit_2]
    ...
end

@flagset FlagSetName [{Symbol,BaseType}] begin
    [bit_1 -->] :flag_1
    [bit_2 -->] :flag_2
    ...
end
```

Automatic numbering starts at 1. In the following example, we build an 8-bit `FlagSet`
with no value for bit 3 (value of 4).
```julia
julia> @flagset FontFlags1 :bold :italic 8 --> :large
```

Instances can be created from integers or flag names and composed with bitwise operations.
Flag names can be symbols or keyword arguments.
```julia
julia> FontFlags1(1)
FontFlags1 with 1 element:
  :bold

julia> FontFlags1(:bold, :italic)
FontFlags1 with 2 elements:
  :bold
  :italic

julia> FontFlags1(bold = true, italic = false, large = true)
FontFlags1 with 2 elements:
  :bold
  :large

julia> FontFlags1(3) | FontFlags1(8)
FontFlags1 with 3 elements:
  :bold
  :italic
  :large
```

Flag sets support iteration and other set operations
```julia
julia> for flag in FontFlags1(3); println(flag) end
bold
italic

julia> :bold in FontFlags1(3)
true
```

Conversion to and from integers is permitted, but only when the integer contains valid
bits for the flag set type.
```julia
julia> Int(FontFlags1(:bold))
1

julia> Integer(FontFlags1(:italic))    # Abstract Integer uses base type
0x00000002

julia> FontFlags1(9)
FontFlags1 with 2 elements:
  :bold
  :large

julia> FontFlags1(4)
ERROR: ArgumentError: invalid value for FlagSet FontFlags1: 4
Stacktrace:
...
```

Supplying a different BaseType:

```
julia> @flagset FontFlags2 {Symbol,UInt8} :bold :italic 8 --> :large

# Or let the macro infer the flag type
julia> @flagset FontFlags3 {_,UInt8} :bold :italic 8 --> :large

julia> Integer(FontFlags2(:italic))
0x02

julia> Integer(FontFlags3(:italic))
0x02

julia> eltype(FontFlags2) == eltype(FontFlags3) == Symbol
true
```

The empty and the full set can be created with `typemin` and `typemax`:
```julia
julia> typemin(FontFlags1)
FontFlags1([])

julia> typemax(FontFlags1)
FontFlags1 with 3 elements:
  :bold
  :italic
  :large
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

`RoundingFlags3` also specifies keys for flags (except the last), flag and base types:
```
julia> @flagset RoundingFlags3 {Any,UInt8} begin
    down = RoundDown
    up = RoundUp
    near = 16 --> RoundNearest
    64 --> RoundNearestTiesUp
end

julia> RoundingFlags3(up = true, near = true)
RoundingFlags3 with 2 elements:
  RoundingMode{:Up}()
  RoundingMode{:Nearest}()

julia> eltype(RoundingFlags3)
Any

julia> typeof(Integer(RoundingFlags3()))
UInt8
```

## Printing

Printing a `FlagSet` subtype shows usefull information about it:
```julia
julia> FontFlags1
FlagSet FontFlags1:
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
julia> [FontFlags1(), FontFlags1(:bold, :large)]
2-element Vector{FontFlags1}:
 FontFlags1([])
 FontFlags1([:bold, :large])

julia> [RoundingFlags3() RoundingFlags3([RoundUp, RoundNearest])]
1×2 Matrix{RoundingFlags3}:
 RoundingFlags3(0x00000000)  RoundingFlags3(0x00000012)

julia> show(IOContext(stdout, :compact => true), FontFlags1(:bold, :large))
FontFlags1(0x09)
```
## Input/Output

`FlagSet`s support writing to and reading from streams as integers:
```julia
julia> io = IOBuffer();

julia> write(io, UInt8(9));

julia> seekstart(io);

julia> read(io, FontFlags1)
FontFlags1 with 2 elements:
  :bold
  :large
```
