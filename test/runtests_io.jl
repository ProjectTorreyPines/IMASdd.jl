using IMASdd
import IMASdd as IMAS
using Test
import IMASdd.HDF5 as HDF5

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "IO" begin
    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.h5")
    ddh = IMAS.hdf2imas(filename)
    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample_with_attrs.h5")
    ddh2 = IMAS.hdf2imas(filename)

    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.json")
    ddj = IMAS.json2imas(filename)

    @test ddj == ddh
    @test ddh == ddh2

    test_dir = mktempdir()

    @testset "JSON_strict" begin
        IMAS.imas2json(ddj, joinpath(test_dir, "test.json"); strict=true, freeze=false)
        dd = IMAS.json2imas(joinpath(test_dir, "test.json"))
        @test ddj == dd
    end

    @testset "JSON_strict_frozen" begin
        IMAS.imas2json(ddj, joinpath(test_dir, "test.json"); strict=true, freeze=true)
        dd = IMAS.json2imas(joinpath(test_dir, "test.json"))
        @test IMAS.freeze(ddj) == IMAS.freeze(dd)
    end

    @testset "JSON" begin
        IMAS.imas2json(ddj, joinpath(test_dir, "test.json"); strict=false, freeze=false)
        dd = IMAS.json2imas(joinpath(test_dir, "test.json"))
        @test ddj == dd
    end

    @testset "HDF" begin
        IMAS.imas2hdf(ddh, joinpath(test_dir, "test.hdf"); strict=false, freeze=false)
        dd = IMAS.hdf2imas(joinpath(test_dir, "test.hdf"))
        @test ddh == dd
    end
end


@testset "isequal" begin
    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.h5")
    dd1 = IMAS.hdf2imas(filename)
    dd2 = IMAS.hdf2imas(filename)

    @test dd1 == dd2
    @test dd1.core_sources == dd2.core_sources

    dd2.core_sources.time[] = 1.0
    @test !isequal(dd1, dd2; verbose=true);
    @test !isequal(dd1.core_sources, dd2.core_sources; verbose=true);
    @test isequal(dd1.core_sources.source, dd2.core_sources.source; verbose=true);

    resize!(dd2.core_sources.source, 5)
    @test !isequal(dd1.core_sources.source, dd2.core_sources.source; verbose=true);

    dd2.core_sources.source[2].identifier.index = 0
    @test (dd1.core_sources.source[1:3] .== dd2.core_sources.source[1:3]) == BitVector([1, 0, 1])

    dd2.core_profiles.profiles_1d[1].j_total[2] = 0.0
    dd2.equilibrium.time_slice[1].profiles_1d.psi[[1, 7, 8, 13, 15]] .= rand(5)
    dd2.core_sources.source[1].identifier.name = "abcde"
    dd2.equilibrium.time_slice[].profiles_2d[1].psi[1:2, :] .= 0.0
    dd2.global_time = 100.0

    isequal(dd1, dd2; verbose=true);

    isequal(dd1.equilibrium, dd2; verbose=true);
end