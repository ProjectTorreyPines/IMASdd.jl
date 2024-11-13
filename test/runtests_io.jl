using IMASdd
import IMASdd as IMAS
using Test

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "IO" begin
    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.h5")
    ddh = IMAS.hdf2imas(filename)

    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.json")
    ddj = IMAS.json2imas(filename)

    @test ddj == ddh
    test_dir = mktempdir()

    @testset "JSON_strict" begin
        IMAS.imas2json(ddj, joinpath(test_dir, "test.json"); strict=true, freeze=false)
        dd = IMAS.json2imas(joinpath(test_dir, "test.json"))
        @test ddj==dd
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
