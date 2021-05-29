# This file is part of FlagSets.jl project, which is licensed under BDS-3-Clause.
# See file LICENSE.md for more information.

using FlagSets
using Test, Serialization

macro macrocall(ex)
    @assert Meta.isexpr(ex, :macrocall)
    ex.head = :call
    for i = 2:length(ex.args)
        ex.args[i] = QuoteNode(ex.args[i])
    end
    insert!(ex.args, 3, __module__)
    return esc(ex)
end

@testset "Basic operation" begin
    # Inline definition
    @flagset Flag1 flag1a flag1b flag1c
    @test Int(Flag1(:flag1a)) == 1
    @test Int(Flag1(:flag1b)) == 2
    @test Int(Flag1(:flag1c)) == 4
    @test Flag1(1) == Flag1(:flag1a)
    @test Flag1(2) == Flag1(:flag1b)
    @test Flag1(4) == Flag1(:flag1c)
    @test flags(Flag1) == (:flag1a, :flag1b, :flag1c)
    @test Flag1(flag1a = true, flag1b = false, flag1c = true) == Flag1(:flag1a, :flag1c)

    # Block definition
    @flagset Flag2 begin
        flag2a
        flag2b
        flag2c
    end
    @test Int(Flag2(:flag2a)) == 1
    @test Int(Flag2(:flag2b)) == 2
    @test Int(Flag2(:flag2c)) == 4
    @test Flag2(flag2a = true, flag2b = false, flag2c = true) == Flag2(:flag2a, :flag2c)

    # Explicit numbering, inline
    @flagset Flag3 flag3a = 1 flag3b flag3c = 8
    @test Int(Flag3(:flag3a)) == 1
    @test Int(Flag3(:flag3b)) == 2
    @test Int(Flag3(:flag3c)) == 8
    @test Int(typemin(Flag3)) == 0
    @test Int(typemax(Flag3)) == 11
    @test Flag3(flag3a = true, flag3b = false, flag3c = true) == Flag3(:flag3a, :flag3c)
    @test flags(Flag3) == (:flag3a, :flag3b, :flag3c)

    # Mask operations
    @test Flag1(:flag1a) | Flag1(:flag1b) | Flag1(:flag1c) ==
          Flag1(:flag1a, :flag1b, :flag1c)
    @test Int(Flag1(7)) == 7
    @test Flag1(7) == Flag1(:flag1a) | Flag1(:flag1b) | Flag1(:flag1c)
    @test Flag1(7) & Flag1(:flag1a) == Flag1(:flag1a)
    @test Flag1(7) ⊻ Flag1(:flag1a) == Flag1(:flag1b, :flag1c)
    @test Int(Flag1(:flag1a)) < Int(Flag1(:flag1b)) < Int(Flag1(:flag1c))
    @test Int(Flag1(:flag1a) | Flag1(:flag1b)) < Int(Flag1(:flag1c))
end # testset

@testset "Type properties" begin
    # Default integer typing
    @flagset Flag5 flag5a flag5b
    @test typeof(Integer(Flag5(:flag5a))) == UInt32
    @test typeof(Flag5) == DataType
    @test typeof(Flag5(:flag5a)) <: Flag5 <: FlagSet <: AbstractSet
    @test isbitstype(Flag5)
    @test isbits(Flag5(:flag5a))

    # Construct non-default
    @flagset Flag6::UInt8 flag6a flag6b flag6c
    @test typeof(Integer(Flag6(:flag6a))) == UInt8
    @test UInt8(Flag6(:flag6a)) === 0x01
    @test UInt16(Flag6(:flag6b)) === 0x0002
    @test UInt128(Flag6(:flag6c)) === 0x00000000000000000000000000000004

    # Explicit values of non-default types
    @flagset Flag7::UInt8 flag7a = big"1" flag7b = UInt8(2)
    @test Integer(Flag7(:flag7a)) === UInt8(1)
    @test Integer(Flag7(:flag7b)) === UInt8(2)
end # testset

@testset "Validate type" begin
    @test isvalid(Flag1(:flag1a))
    @test isvalid(Flag1(:flag1b))
    @test isvalid(Flag1(0x0000_0003))
    @test !isvalid(Core.Intrinsics.bitcast(Flag1, UInt32(1<<3)))
    @test !isvalid(Core.Intrinsics.bitcast(Flag1, UInt32(1<<4)))
    @test !isvalid(Core.Intrinsics.bitcast(Flag1, UInt32(1<<5 | 1)))
    @test !isvalid(Core.Intrinsics.bitcast(Flag1, UInt32(1<<29 | 1<<3)))

    @test isvalid(Flag6(:flag6a))
    @test isvalid(Flag6(:flag6b))
    @test isvalid(Flag6(0x03))
    @test !isvalid(Core.Intrinsics.bitcast(Flag6, UInt8(1<<3)))
    @test !isvalid(Core.Intrinsics.bitcast(Flag6, UInt8(1<<4)))
    @test !isvalid(Core.Intrinsics.bitcast(Flag6, UInt8(1<<5 | 1)))
    @test !isvalid(Core.Intrinsics.bitcast(Flag6, UInt8(1<<7 | 1<<3)))
end

@testset "Error conditions" begin

    @test_throws ArgumentError("no arguments given for FlagSet Foo") @macrocall(@flagset Foo)
    @test_throws ArgumentError(
        "invalid base type for FlagSet Foo, " *
        "::Float64; base type must be an unsigned " *
        "integer primitive type",
    ) @macrocall(@flagset Foo::Float64 x = 1.0)

    # Require uniqueness
    @test_throws ArgumentError("values for FlagSet Foo are not unique") @macrocall(@flagset Foo x =
        1 y = 1)
    @test_throws ArgumentError("values for FlagSet Foo are not unique") @macrocall(@flagset Foo x =
        2 y = 2)

    # Explicit values must be powers of two
    @test_throws ArgumentError(
        "invalid value for FlagSet Foo, _three = 3; " *
        "each value must be a positive power of 2",
    ) @macrocall(@flagset Foo _three = 3)

    # Values must be integers
    @test_throws ArgumentError(
        "invalid value for FlagSet Foo, _zero = \"zero\"; " *
        "values must be unsigned integers",
    ) @macrocall(@flagset Foo _zero = "zero")
    @test_throws ArgumentError(
        "invalid value for FlagSet Foo, _zero = '0'; " * "values must be unsigned integers",
    ) @macrocall(@flagset Foo _zero = '0')
    @test_throws ArgumentError(
        "invalid value for FlagSet Foo, _zero = 0.5; " * "values must be unsigned integers",
    ) @macrocall(@flagset Foo _zero = 0.5)

    # Names must be valid identifiers
    @test_throws ArgumentError("""invalid argument for FlagSet Foo: if x
                                      1
                                  else
                                      2
                                  end""") @macrocall(@flagset Foo x ? 1 : 2)
    @test_throws ArgumentError("invalid argument for FlagSet Foo: 1 = 2") @macrocall(@flagset Foo 1 =
        2)

    # Disallow value overflow
    @test_throws ArgumentError("overflow in value \"y\" of FlagSet Foo") @macrocall(@flagset Foo x =
        (UInt32(1) << 31) y)

end # testset

@testset "Input/Output" begin
    @flagset Flag9::UInt64 flag9a = 1 flag9b = 256
    @flagset Flag10::UInt8 flag10a = 128 flag10b = 2
    let io = IOBuffer()
        write(io, Flag10(:flag10a, :flag10b))
        write(io, Flag9(:flag9b))
        write(io, Flag10(:flag10a))
        seekstart(io)
        @test read(io, Flag10) == Flag10(:flag10a, :flag10b)
        @test read(io, Flag9) == Flag9(:flag9b)
        @test read(io, Flag10) == Flag10(:flag10a)
    end
    let io = IOBuffer()
        serialize(io, Flag9(:flag9a))
        seekstart(io)
        @test deserialize(io) === Flag9(:flag9a)
    end
end # testset

# In submodule and not imported to test prefix printing
module SubModule
using ..FlagSets
@flagset Bits::UInt8 one two four eight
end

@testset "String representations" begin
    @flagset FilePerms::UInt8 READ = 4 WRITE = 2 EXEC = 1

    @test string(FilePerms) == "FilePerms"
    @test string(SubModule.Bits) == "Main.SubModule.Bits"
    @test string(FilePerms()) == "FilePerms()"
    @test string(SubModule.Bits(:one)) == "Main.SubModule.Bits(:one)"
    @test repr("text/plain", FilePerms) ==
          "FlagSet $(string(FilePerms)):\n" *
          " EXEC = 0x01\n" *
          " WRITE = 0x02\n" *
          " READ = 0x04"
    @test repr("text/plain", SubModule.Bits) ==
          "FlagSet Main.SubModule.Bits:\n" *
          " one = 0x01\n" *
          " two = 0x02\n" *
          " four = 0x04\n" *
          " eight = 0x08"
    @test repr(FilePerms(:EXEC)) == "FilePerms(:EXEC)"
    @test repr(SubModule.Bits(:one)) == "Main.SubModule.Bits(:one)"
    @test repr(FilePerms(:EXEC, :READ)) == "FilePerms(:EXEC, :READ)"
    @test repr(SubModule.Bits(:one, :eight)) == "Main.SubModule.Bits(:one, :eight)"
    @test repr(FilePerms() | FilePerms(:READ)) == "FilePerms(:READ)"

    let io = IOBuffer(), ioc = IOContext(io, :compact => true, :module => Main)
        show(io, MIME"text/plain"(), FlagSet)
        @test String(take!(io)) == "FlagSet"
        show(ioc, MIME"text/plain"(), FlagSet)
        @test String(take!(io)) == "FlagSet"

        # Explicit :compact => false required for consistency across Julia versions
        iof = IOContext(io, :compact => false, :module => Main)
        show(iof, MIME"text/plain"(), Union{FilePerms,SubModule.Bits})
        @test String(take!(io)) == "Union{FilePerms, Main.SubModule.Bits}"
        show(ioc, MIME"text/plain"(), Union{FilePerms,SubModule.Bits})
        @test String(take!(io)) == "Union{FilePerms, Bits}"

        show(ioc, FilePerms())
        @test String(take!(io)) == "FilePerms(0x00)"
        show(ioc, SubModule.Bits(:one))
        @test String(take!(io)) == "Bits(0x01)"
        show(ioc, FilePerms(:EXEC, :READ))
        @test String(take!(io)) == "FilePerms(0x05)"
        show(ioc, SubModule.Bits(:one, :eight))
        @test String(take!(io)) == "Bits(0x09)"
    end # testset
end
