# This file is part of FlagSets.jl project, which is licensed under BDS-3-Clause.
# See file LICENSE.md for more information.

@enum MyFlagType Foo Bar Baz Quux

@testset "Basic operation" begin
    # Inline definition
    @flagset TFlag1 RoundDown RoundUp RoundNearest
    @test Int(TFlag1([RoundDown])) == 1
    @test Int(TFlag1([RoundUp])) == 2
    @test Int(TFlag1([RoundNearest])) == 4
    @test TFlag1(1) == TFlag1([RoundDown])
    @test TFlag1(2) == TFlag1([RoundUp])
    @test TFlag1(4) == TFlag1([RoundNearest])
    @test flags(TFlag1) == (RoundDown, RoundUp, RoundNearest)
    @test length(TFlag1([RoundDown, RoundUp])) == 2
    @test !iszero(TFlag1([RoundUp, RoundNearest]))
    @test iszero(TFlag1())
    @test eltype(TFlag1) == RoundingMode

    # Block definition
    @flagset TFlag2 begin
        down = RoundDown
        up = RoundUp
        near = RoundNearest
    end
    @test Int(TFlag2(down = true)) == 1
    @test Int(TFlag2(up = true)) == 2
    @test Int(TFlag2(near = true)) == 4
    @test TFlag2(down = true, up = false, near = true) == TFlag2([RoundDown, RoundNearest])
    @test length(TFlag2([RoundNearest])) == 1
    @test !iszero(TFlag2([RoundNearest]))
    @test iszero(TFlag2())
    @test eltype(TFlag2) == RoundingMode

    # Explicit numbering, inline
    @flagset TFlag3 {} down = 2 --> RoundDown RoundUp near = 16 --> RoundNearest
    @test Int(TFlag3(down = true)) == 2
    @test Int(TFlag3([RoundUp])) == 4
    @test Int(TFlag3(near = true)) == 16
    @test isempty(typemin(TFlag3))
    @test Int(typemax(TFlag3)) == 22
    @test length(typemax(TFlag3)) == 3
    @test TFlag3(down = false, near = true) == TFlag3([RoundNearest])
    @test flags(TFlag3) == (RoundDown, RoundUp, RoundNearest)
    @test eltype(TFlag3) == RoundingMode

    # Construct from enums
    @flagset TFlag4 {} begin
        foo = 2 --> Foo
        bar = Bar
        16 --> Baz
        quux = 64 --> Quux
    end
    @test Int(TFlag4(foo = true)) == 2
    @test Int(TFlag4([Bar])) == 4
    @test Int(TFlag4([Baz])) == 16
    @test isempty(typemin(TFlag4))
    @test Int(typemax(TFlag4)) == 86
    @test length(typemax(TFlag4)) == 4
    @test TFlag4(foo = false, quux = true) == TFlag4([Quux])
    @test flags(TFlag4) == (Foo, Bar, Baz, Quux)
    @test eltype(TFlag4) == MyFlagType
end

@testset "Mask and set operations" begin
    # Mask operations
    @test TFlag1([RoundDown]) | TFlag1([RoundUp]) | TFlag1([RoundNearest]) ==
          TFlag1([RoundDown, RoundUp, RoundNearest])
    @test Int(TFlag1(7)) == 7
    @test TFlag1(7) == TFlag1([RoundDown]) | TFlag1([RoundUp]) | TFlag1([RoundNearest])
    @test TFlag1(7) & TFlag1([RoundDown]) == TFlag1([RoundDown])
    @test TFlag1(7) ⊻ TFlag1([RoundDown]) == TFlag1([RoundUp, RoundNearest])
    @test ~TFlag1([RoundDown]) == TFlag1([RoundUp, RoundNearest])
    @test Int(TFlag1([RoundDown])) < Int(TFlag1([RoundUp])) < Int(TFlag1([RoundNearest]))
    @test Int(TFlag1([RoundDown]) | TFlag1([RoundUp])) < Int(TFlag1([RoundNearest]))

    # Set operations
    @test TFlag1([RoundDown]) ∪ TFlag1([RoundUp]) ∪ TFlag1([RoundNearest]) ==
          TFlag1([RoundDown, RoundUp, RoundNearest])
    @test TFlag1(7) == TFlag1([RoundDown]) ∪ TFlag1([RoundUp]) ∪ TFlag1([RoundNearest])
    @test TFlag1(7) ∩ TFlag1([RoundDown]) == TFlag1([RoundDown])
    @test TFlag1(7) ⊻ TFlag1([RoundDown]) == TFlag1([RoundUp, RoundNearest])
    @test setdiff(typemax(TFlag1), TFlag1([RoundDown])) == TFlag1([RoundUp, RoundNearest])
    @test RoundDown ∈ TFlag1([RoundDown])
    @test RoundDown ∉ TFlag1([RoundUp, RoundNearest])
    @test TFlag1([RoundUp]) ⊊ TFlag1([RoundUp, RoundNearest])
    @test TFlag1([RoundNearest]) ⊊ TFlag1([RoundUp, RoundNearest])
    @test !(TFlag1([RoundUp, RoundNearest]) ⊊ TFlag1([RoundUp, RoundNearest]))
    @test !(TFlag1([RoundDown, RoundUp]) ⊊ TFlag1([RoundUp, RoundNearest]))
end # testset

@testset "Type properties" begin
    # Default integer typing
    @flagset TFlag5 :flag5a :flag5b
    @test eltype(TFlag5) == Symbol
    @test typeof(Integer(TFlag5(:flag5a))) == UInt32
    @test typeof(TFlag5) == DataType
    @test typeof(TFlag5(:flag5a)) <: TFlag5 <: FlagSet <: AbstractSet
    @test isbitstype(Flag5)
    @test isbits(TFlag5(:flag5a))

    # Construct non-default and infer type
    @flagset TFlag6 {nothing, UInt16} begin
        foo = 2 --> Foo
        bar = Bar
        16 --> Baz
        quux = 64 --> Quux
    end
    @test typeof(Integer(TFlag6(foo = true))) == UInt16
    @test UInt8(TFlag6(foo = true)) === 0x02
    @test UInt16(TFlag6([Bar])) === 0x0004
    @test UInt128(TFlag6([Baz])) === 0x00000000000000000000000000000010
    @test typeof(TFlag6()) <: TFlag6 <: FlagSet <: AbstractSet

    # Explicit bits of non-default types
    @flagset TFlag7 {Int128, UInt8} sixty_four = big"1" --> 64 one_hundred = UInt8(2) --> 100
    @test Integer(TFlag7(sixty_four = true)) === UInt8(1)
    @test Integer(TFlag7(one_hundred = true)) === UInt8(2)
    @test eltype(TFlag7) == Int128
    @test TFlag7([64]) == TFlag7(sixty_four = true)
    @test TFlag7([100]) == TFlag7(one_hundred = true)
    @test 1 ∉ TFlag7([64])
    @test 64 ∈ TFlag7([64])

    # Correctly detect base type
    @flagset TFlag8 {} missing (Int64(1) << 60) --> nothing
    @test eltype(TFlag8) == Union{Missing,Nothing}
    @test typeof(convert(Integer, TFlag8())) == UInt64
    @test typeof(convert(Int, TFlag8())) == Int

    @flagset TFlag9 {Any} missing (Int128(1) << 120) --> nothing
    @test eltype(TFlag9) == Any
    @test typeof(convert(Integer, TFlag9())) == UInt128
    @test typeof(convert(Int, TFlag9())) == Int
end # testset

@testset "Validate type" begin
    @test isvalid(TFlag1([RoundDown]))
    @test isvalid(TFlag1([RoundUp]))
    @test isvalid(TFlag1(0x0000_0003))
    @test !isvalid(TFlag1, UInt32(1 << 3))
    @test !isvalid(TFlag1, UInt32(1 << 4))
    @test !isvalid(TFlag1, UInt32(1 << 5 | 1))
    @test !isvalid(TFlag1, UInt32(1 << 29 | 1 << 3))

    @test isvalid(TFlag6(foo = true))
    @test isvalid(TFlag6([Baz]))
    @test isvalid(TFlag6(0x06))
    @test !isvalid(TFlag6, UInt8(1 << 3))
    @test !isvalid(TFlag6, UInt8(1 << 5))
    @test !isvalid(TFlag6, UInt8(1 << 4 | 1))
    @test !isvalid(TFlag6, UInt8(1 << 7 | 1 << 2))

    @test isvalid(TFlag1, [RoundDown])
    @test !isvalid(TFlag1, [RoundNearestTiesAway])
    @test isvalid(TFlag1, [RoundDown, RoundUp])
    @test isvalid(TFlag1, ([RoundDown, RoundUp]))
    @test isvalid(TFlag1, Set([RoundDown, RoundUp]))
    @test TFlag1(Set([RoundDown, RoundUp])) == TFlag1([RoundDown, RoundUp])
    @test !isvalid(TFlag1, [:foo, :bar])
end

@testset "Error conditions" begin

    @test_throws(ArgumentError("no arguments given for FlagSet Foo"), @macrocall(@flagset Foo))
    @test_throws(
        ArgumentError("invalid base type for FlagSet Foo: Float64; should be an integer type"),
        @macrocall(@flagset Foo {nothing, Float64} x = 1.0)
    )
    @test_throws(
        ArgumentError("invalid flag type for FlagSet Foo: 1234"),
        @macrocall(@flagset Foo {1234} x = 1.0)
    )

    # Require uniqueness
    @test_throws(
        ArgumentError("bits for FlagSet Foo are not unique"),
        @macrocall(@flagset Foo x = 1 --> :x y = 1 --> :y)
    )
    @test_throws(
        ArgumentError("bits for FlagSet Foo are not unique"),
        @macrocall(@flagset Foo x = 2 --> :x y = 2 --> :y)
    )

    # Explicit bits must be powers of two
    @test_throws(
        ArgumentError(
            "invalid bit for FlagSet Foo: $(:(_three = 3 --> nothing)); " *
            "should be an integer positive power of 2",
        ),
        @macrocall(@flagset Foo _three = 3 --> nothing)
    )

    # Values must be integers
    @test_throws(
        ArgumentError(
            "invalid bit for FlagSet Foo: $(:(_zero = "zero" --> nothing)); " *
            "should be an integer positive power of 2",
        ),
        @macrocall(@flagset Foo _zero = "zero" --> nothing)
    )
    @test_throws(
        ArgumentError(
            "invalid bit for FlagSet Foo: $(:('0' --> nothing)); " *
            "should be an integer positive power of 2",
        ),
        @macrocall(@flagset Foo '0' --> nothing)
    )
    @test_throws(
        ArgumentError(
            "invalid bit for FlagSet Foo: $(:(0.5 --> nothing)); " *
            "should be an integer positive power of 2",
        ),
        @macrocall(@flagset Foo 0.5 --> nothing)
    )

    # Names must be valid identifiers
    @test_throws(
        ArgumentError("invalid argument for FlagSet Foo: 1 = 2"),
        @macrocall(@flagset Foo 1 = 2)
    )

    # Disallow bit overflow
    @test_throws(
        ArgumentError("overflow in bit \"y = nothing\" of FlagSet Foo"),
        @macrocall(@flagset Foo {nothing, UInt32} x = UInt32(1) << 31 --> missing y = nothing)
    )

end # testset

@testset "Input/Output" begin
    @flagset TFlag10 {nothing, UInt64} foo = 1 --> Foo bar = 256 --> Bar
    @flagset TFlag11 {nothing, UInt8} foo = 128 --> Foo bar = 2 --> Bar
    let io = IOBuffer()
        write(io, TFlag11([Foo, Bar]))
        write(io, TFlag10(bar = true))
        write(io, TFlag11([Foo]))
        seekstart(io)
        @test read(io, TFlag11) == TFlag11([Foo, Bar])
        @test read(io, TFlag10) == TFlag10(bar = true)
        @test read(io, TFlag11) == TFlag10([Foo])
    end
    let io = IOBuffer()
        serialize(io, TFlag10([Foo]))
        seekstart(io)
        @test deserialize(io) === TFlag10([Foo])
    end
end # testset

# In submodule and not imported to test prefix printing
module TSubModule
using ..FlagSets
@flagset TBits {Int, UInt8} 1 2 3 4
end

@testset "String representations" begin
    @flagset TFilePerms {String, UInt8} 4 --> "READ" 2 --> "WRITE" 1 --> "EXEC"

    @test string(TFilePerms) == "TFilePerms"
    @test string(TSubModule.TBits) == "Main.TSubModule.TBits"
    @test string(TFilePerms()) == "TFilePerms([])"
    @test string(TSubModule.TBits([1])) == "Main.TSubModule.TBits([1])"
    @test repr("text/plain", TFilePerms) ==
          "FlagSet $(string(TFilePerms)):\n" *
          " 0x01 --> \"EXEC\"\n" *
          " 0x02 --> \"WRITE\"\n" *
          " 0x04 --> \"READ\""
    @test repr("text/plain", TSubModule.TBits) ==
          "FlagSet Main.TSubModule.TBits:\n" *
          " 0x01 --> 1\n" *
          " 0x02 --> 2\n" *
          " 0x04 --> 3\n" *
          " 0x08 --> 4"
    @test repr(TFilePerms(["EXEC"])) == "TFilePerms([\"EXEC\"])"
    @test repr(TSubModule.TBits([1])) == "Main.TSubModule.TBits([1])"
    @test repr(TFilePerms(["EXEC", "READ"])) == "TFilePerms([\"EXEC\", \"READ\"])"
    @test repr(TSubModule.TBits([1, 4])) == "Main.TSubModule.TBits([1, 4])"
    @test repr(TFilePerms() | TFilePerms(["READ"])) == "TFilePerms([\"READ\"])"

    let io = IOBuffer(), ioc = IOContext(io, :compact => true, :module => Main)
        show(io, MIME"text/plain"(), FlagSet)
        @test String(take!(io)) == "FlagSet"
        show(ioc, MIME"text/plain"(), FlagSet)
        @test String(take!(io)) == "FlagSet"

        # Explicit :compact --> false required for consistency across Julia versions
        iof = IOContext(io, :compact => false, :module => Main)
        show(iof, MIME"text/plain"(), Union{TFilePerms,TSubModule.TBits})
        @test String(take!(io)) == "Union{TFilePerms, Main.TSubModule.TBits}"
        show(ioc, MIME"text/plain"(), Union{TFilePerms,TSubModule.TBits})
        @test String(take!(io)) == "Union{TFilePerms, TBits}"

        show(ioc, TFilePerms())
        @test String(take!(io)) == "TFilePerms(0x00)"
        show(ioc, TSubModule.TBits([1]))
        @test String(take!(io)) == "TBits(0x01)"
        show(ioc, TFilePerms(["EXEC", "READ"]))
        @test String(take!(io)) == "TFilePerms(0x05)"
        show(ioc, TSubModule.TBits([1, 4]))
        @test String(take!(io)) == "TBits(0x09)"
    end
end # testset
