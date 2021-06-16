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

@testset "FlagSets of symbols" verbose=true begin
    include("symbol_flagsets.jl")
end

@testset "FlagSets of other types" verbose=true begin
    include("flagset_type.jl")
end
