using IMASdd
using Test
import IMASdd.HDF5 as HDF5

"""
Stand-in for a satellite package (e.g. IFEdd) that defines its own concrete
container on top of IMASdd's abstract `DD`. Mirrors the shape `GenerateDD` emits.

Pins the contract IMASdd promises to satellites without depending on any of them.

The types are deliberately named `my_own_*` rather than `dd` so that nothing here
can be confused with `IMASdd.dd`. `my_own_ids` exists only in this satellite —
`IMASdd.dd` has no such field — which is what makes the round-trip tests below
meaningful.
"""
module FakeSatellite

import IMASdd

# --- an IDS owned by the satellite, with no counterpart in IMASdd.dd ---

mutable struct FilledFields____my_own_ids <: IMASdd.FilledFields
    var"my_value"::Bool
end

mutable struct my_own_ids{T} <: IMASdd.IDS{T}
    var"my_value"::T
    _name::Symbol
    _filled::FilledFields____my_own_ids
    _frozen::Bool
    _threads_lock::ReentrantLock
    _in_expression::IMASdd.ThreadSafeDicts.ThreadSafeDict{Int,Vector{Symbol}}
    _parent::WeakRef
end

function my_own_ids{T}(; frozen::Bool=false) where {T}
    return my_own_ids{T}(
        0.0,
        Symbol("my_own_ids"),
        FilledFields____my_own_ids(false),
        frozen,
        ReentrantLock(),
        IMASdd.ThreadSafeDicts.ThreadSafeDict{Int,Vector{Symbol}}(),
        WeakRef(nothing))
end

# --- the satellite's top-level container ---

mutable struct FilledFields____my_own_dd <: IMASdd.FilledFields
    var"my_own_ids"::Bool
    var"requirements"::Bool
end

mutable struct my_own_dd{T} <: IMASdd.DD{T}
    var"my_own_ids"::my_own_ids{T}              # satellite-only
    var"requirements"::IMASdd.requirements{T}   # reused from IMASdd
    global_time::Float64
    _aux::IMASdd.ThreadSafeDicts.ThreadSafeDict{Symbol,Any}
    _name::Symbol
    _filled::FilledFields____my_own_dd
    _frozen::Bool
    _threads_lock::ReentrantLock
    _in_expression::IMASdd.ThreadSafeDicts.ThreadSafeDict{Int,Vector{Symbol}}
    _parent::WeakRef
end

function my_own_dd{T}(; frozen::Bool=false) where {T}
    ids = my_own_dd{T}(
        my_own_ids{T}(; frozen),
        IMASdd.requirements{T}(; frozen),
        0.0,
        IMASdd.ThreadSafeDicts.ThreadSafeDict{Symbol,Any}(),
        Symbol(""),
        FilledFields____my_own_dd(false, false),
        frozen,
        ReentrantLock(),
        IMASdd.ThreadSafeDicts.ThreadSafeDict{Int,Vector{Symbol}}(),
        WeakRef(nothing))
    setfield!(ids.my_own_ids, :_parent, WeakRef(ids))
    setfield!(ids.requirements, :_parent, WeakRef(ids))
    return ids
end

my_own_dd(; frozen::Bool=false) = my_own_dd{Float64}(; frozen)

# satellites register their own paths into the shared registry; keyed by our own
# types, so this cannot collide with IMASdd's entries
merge!(IMASdd._all_info, Dict(
    (my_own_dd, :my_own_ids) => IMASdd.Info(String[], "-", "STRUCTURE", "IDS owned by the satellite", true, String[]),
    (my_own_dd, :requirements) => IMASdd.Info(String[], "-", "STRUCTURE", "Reused IMASdd requirements IDS", true, String[]),
    (my_own_ids, :my_value) => IMASdd.Info(String[], "-", "FLT_0D", "A satellite-only scalar", true, String[])
))

end # module FakeSatellite

@testset "satellite dd" begin

    @testset "type contract" begin
        @test FakeSatellite.my_own_dd{Float64} <: IMASdd.DD{Float64}
        @test FakeSatellite.my_own_dd{Float64} <: IMASdd.IDS{Float64}
        @test isabstracttype(IMASdd.DD)
        @test !isabstracttype(FakeSatellite.my_own_dd{Float64})

        # a satellite container is a sibling of IMASdd.dd, not a subtype of it
        @test !(FakeSatellite.my_own_dd{Float64} <: IMASdd.dd)

        # the satellite-only field is what IMASdd.dd cannot represent
        @test hasfield(FakeSatellite.my_own_dd{Float64}, :my_own_ids)
        @test !hasfield(IMASdd.dd{Float64}, :my_own_ids)

        sat = FakeSatellite.my_own_dd{Float64}()
        @test eltype(sat) === Float64
        @test eltype(FakeSatellite.my_own_dd{Float32}()) === Float32
        @test haskey(IMASdd._all_info, (FakeSatellite.my_own_dd, :my_own_ids))
        @test haskey(IMASdd._all_info, (FakeSatellite.my_own_ids, :my_value))
    end

    @testset "_resolve_concrete_type" begin
        sat = FakeSatellite.my_own_dd{Float64}()
        conc = string(typeof(sat))

        # neither IMASdd nor its satellites export their types, so
        # `string(typeof(ids))` is always module-qualified — this is the shape
        # that must resolve
        @test occursin('.', conc)
        @test IMASdd._resolve_concrete_type(conc) === typeof(sat)
        @test IMASdd._resolve_concrete_type(string(IMASdd.dd{Float64})) === IMASdd.dd{Float64}

        # nested type parameters
        @test IMASdd._resolve_concrete_type("IMASdd.IDSvector{IMASdd.equilibrium__time_slice{Float64}}") ===
              IMASdd.IDSvector{IMASdd.equilibrium__time_slice{Float64}}

        # bare (unqualified) names still resolve
        @test IMASdd._resolve_concrete_type("Float64") === Float64

        # unresolvable root, and expressions that are not type expressions
        @test_throws ErrorException IMASdd._resolve_concrete_type("NoSuchPackage.my_own_dd{Float64}")
        @test_throws ErrorException IMASdd._resolve_concrete_type("error(\"boom\")")
    end

    @testset "HDF5 round-trip" begin
        sat = FakeSatellite.my_own_dd{Float64}()
        sat.my_own_ids.my_value = 7.0
        sat.requirements.cost = 42.0
        sat.global_time = 1.5

        filename = joinpath(mktempdir(), "satellite.h5")
        IMASdd.imas2hdf(sat, filename)

        HDF5.h5open(filename) do fid
            @test HDF5.attrs(fid)["concrete_type"] == string(typeof(sat))
        end

        @testset "no ids given: type comes from the file" begin
            loaded = IMASdd.hdf2imas(filename)
            @test typeof(loaded) === typeof(sat)
            @test loaded.my_own_ids.my_value == 7.0
            @test loaded.requirements.cost == 42.0
            @test loaded.global_time == 1.5
            @test loaded == sat
        end

        @testset "explicit target_path" begin
            loaded = IMASdd.hdf2imas(filename, "/")
            @test typeof(loaded) === typeof(sat)
            @test loaded == sat
        end

        @testset "explicit ids is filled in place, never substituted" begin
            into_sat = FakeSatellite.my_own_dd{Float64}()
            @test IMASdd.hdf2imas(filename, into_sat) === into_sat
            @test into_sat == sat

            # a mismatched container keeps its own type: overlapping fields are
            # filled, satellite-only ones are skipped
            into_plain = IMASdd.dd{Float64}()
            returned = IMASdd.hdf2imas(filename, into_plain)
            @test returned === into_plain
            @test typeof(returned) === IMASdd.dd{Float64}
            @test returned.requirements.cost == 42.0
        end

        @testset "keyword arguments survive the delegation" begin
            # `verbose` is declared only by the target_path method, so its @info
            # firing proves keywords land there rather than falling through to
            # `HDF5.h5open`
            @test_logs (:info, r"Found type of") match_mode = :any IMASdd.hdf2imas(filename; verbose=true)

            for eomc in (true, false)
                @test IMASdd.hdf2imas(filename; error_on_missing_coordinates=eomc) ==
                      IMASdd.hdf2imas(filename, "/"; error_on_missing_coordinates=eomc)
            end
            @test IMASdd.hdf2imas(filename; show_warnings=false) == sat

            # unknown keywords still reach h5open instead of being dropped
            @test_throws Exception IMASdd.hdf2imas(filename; no_such_kwarg=1)
        end

        @testset "file without concrete_type falls back (backward compatibility)" begin
            legacy = joinpath(mktempdir(), "legacy.h5")
            IMASdd.imas2hdf(sat, legacy)
            HDF5.h5open(legacy, "r+") do fid
                HDF5.delete_attribute(fid, "concrete_type")
            end
            HDF5.h5open(legacy) do fid
                @test !haskey(HDF5.attrs(fid), "concrete_type")
            end

            loaded = IMASdd.hdf2imas(legacy)
            @test typeof(loaded) === IMASdd.dd{Float64}
            @test loaded.requirements.cost == 42.0
        end
    end

end
