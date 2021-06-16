# This file is part of FlagSets.jl project, which is licensed under BDS-3-Clause.
# See file LICENSE.md for more information.

module FlagSets

#import Core.Intrinsics.bitcast
export FlagSet, flags, @flagset, @symbol_flagset

"""
    FlagSet{FlagType,BaseType<:Integer} <: AbstractSet{FlagType}

Supertype of sets which are subsets of a finite universe of objects (flags),
where each set is encoded as an integer of type `BaseType`.
Flag sets are implemented similarly to `BitSet`s.

If `T <: FlagSet{B,F}`, then `typemax(T)` is a finite set of flags of type `F`.
If `set isa T`, then `set` is a subset of `typemax(T)`.

See [`@flagset`](@ref) and [`@flagset_type`](@ref) for more information.
"""
abstract type FlagSet{FlagType,BaseType<:Integer} <: AbstractSet{FlagType} end

include("interface.jl")
include("macros.jl")

end # module
