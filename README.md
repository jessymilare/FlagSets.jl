# FlagSets.jl

`FlagSet.jl` provides an `Enum`-like type for bit flag option values. The main motivations
are:

1. Flags have implicit numbering with incrementing powers of 2.
2. Binary OR (`|`), AND (`&`) and XOR(`⊻`) operations are supported among members.
3. Set operations like `union`, `intersect`, `setdiff`, `in` and `issubset` are also
    supported.
4. Values are pretty-printed, showing the FlagSet type and each flag set.

This implementation is based on [BitFlags](https://github.com/jmert/BitFlags.jl), with
some minor differences:

1. Each flag set is treated as a set of flags, represented by `Symbol`s, which is
    associated to an `Integer` (whose bits 1 corresponds to the flags in the set).
2. The empty flag set (corresponding to 0) is always valid.
3. The `@flagset` macro doesn't create a constant for each flag.


## Basic usage

To create a new `FlagSet` type, use the `@flagset` macro, provide a name, an
optional integer type, and a list of the flag names (and optional values).
A new definition can be given in inline form:
```julia
@flagset FlagSetName[::BaseType] value1[=x] value2[=y]
```
or as a block definition:
```julia
@flagset FlagSetName[::BaseType] begin
    value1[=x]
    value2[=y]
end
```

Automatic numbering starts at 1. In the following example, we build an 8-bit `FlagSet`
with no value for bit 3 (value of 4).
```julia
julia> @flagset FontFlags::UInt8 bold italic large=8
```

Instances can be created from integers or flag names and composed with bitwise operations.
Flag names can be symbols or keyword arguments.
```julia
julia> FontFlags(1)
FontFlags(:bold) = 0x01

julia> FontFlags(:bold, :italic)
FontFlags(:bold, :italic) = 0x03

julia> FontFlags(bold = true, italic = false, large = true)
FontFlags(:bold, :large) = 0x09

julia> FontFlags(3) | FontFlags(8)
FontFlags(:bold, :italic, :large) = 0x0b
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

julia> Integer(FontFlags(:italic))    # Abstract Integer uses native UInt8 type
0x02

julia> FontFlags(9)
FontFlags(:bold, :large) = 0x09

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

## Printing

Printing a `FlagSet` subtype shows usefull information about it:
```julia
julia> FontFlags
FlagSet FontFlags:
 bold = 0x01
 italic = 0x02
 large = 0x08
```

In a compact context (such as in multi-dimensional arrays), the pretty-printing
takes on a shorter form:
```julia
julia> [FontFlags() FontFlags(:bold, :large)]
1×2 Array{FontFlags,2}:
 FontFlags(0x00)  FontFlags(0x09)

julia> show(IOContext(stdout, :compact => true), FontFlags(:bold, :large))
FontFlags(0x09)

julia> print(IOContext(stdout, :compact => true), FontFlags(:bold, :large))
FontFlags(9)
```
## Input/Output

`FlagSet`s support writing to and reading from streams as integers:
```julia
julia> io = IOBuffer();

julia> write(io, UInt8(9));

julia> seekstart(io);

julia> read(io, FontFlags)
FontFlags(:bold, :large) = 0x09
```
