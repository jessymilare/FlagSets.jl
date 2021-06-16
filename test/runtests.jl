# This file is part of FlagSets.jl project, which is licensed under BDS-3-Clause.
# See file LICENSE.md for more information.

using FlagSets
using Test, Serialization

using FlagSets: deprecated_syntax_message, undef_var_error_hint
using FlagSets: invalid_bit_error, invalid_flagspec_error, invalid_type_error
using FlagSets: overflow_error, not_unique_error

macro macrocall(ex)
    @assert Meta.isexpr(ex, :macrocall)
    ex.head = :call
    for i = 2:length(ex.args)
        ex.args[i] = QuoteNode(ex.args[i])
    end
    insert!(ex.args, 3, __module__)
    return esc(ex)
end

if VERSION >= v"1.6"
    @testset "FlagSets of symbols" verbose=true begin
        include("symbol_flagsets.jl")
    end

    @testset "FlagSets of other types" verbose=true begin
        include("flagset_type.jl")
    end
else
    @testset "FlagSets of symbols" begin
        include("symbol_flagsets.jl")
    end

    @testset "FlagSets of other types" begin
        include("flagset_type.jl")
    end
end

