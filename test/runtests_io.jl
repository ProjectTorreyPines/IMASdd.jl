using IMASdd
import IMASdd
using Test
import IMASdd.HDF5 as HDF5

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "IO" begin
    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.h5")
    @test is_h5i(filename) == false
    ddh = IMASdd.hdf2imas(filename)
    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample_with_attrs.h5")
    @test is_h5i(filename) == false
    ddh2 = IMASdd.hdf2imas(filename)

    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.json")
    @test is_h5i(filename) == false
    ddj = IMASdd.json2imas(filename)

    @test ddj == ddh
    @test ddh == ddh2

    test_dir = mktempdir()

    @testset "JSON_strict" begin
        IMASdd.imas2json(ddj, joinpath(test_dir, "test.json"); strict=true, freeze=false)
        dd = IMASdd.json2imas(joinpath(test_dir, "test.json"))
        @test ddj == dd
    end

    @testset "JSON_strict_frozen" begin
        IMASdd.imas2json(ddj, joinpath(test_dir, "test.json"); strict=true, freeze=true)
        dd = IMASdd.json2imas(joinpath(test_dir, "test.json"))
        @test IMASdd.freeze(ddj) == IMASdd.freeze(dd)
    end

    @testset "JSON" begin
        IMASdd.imas2json(ddj, joinpath(test_dir, "test.json"); strict=false, freeze=false)
        dd = IMASdd.json2imas(joinpath(test_dir, "test.json"))
        @test ddj == dd
    end

    @testset "HDF" begin
        IMASdd.imas2hdf(ddh, joinpath(test_dir, "test.hdf"); strict=false, freeze=false)
        dd = IMASdd.hdf2imas(joinpath(test_dir, "test.hdf"))
        @test ddh == dd

        # compression test (compression level is from 0 (no compresson) to 9)
        IMASdd.imas2hdf(ddh, joinpath(test_dir, "test_comp.hdf"); strict=false, freeze=false, compress=9)
        dd_comp = IMASdd.hdf2imas(joinpath(test_dir, "test_comp.hdf"))
        @test ddh == dd_comp

        # Compare file sizes
        uncompressed_size = stat(joinpath(test_dir, "test.hdf")).size
        compressed_size   = stat(joinpath(test_dir, "test_comp.hdf")).size
        @info "Uncompressed file size: $uncompressed_size bytes"
        @info "Compressed file size: $compressed_size bytes"
        @test compressed_size < uncompressed_size
    end
end


@testset "isequal" begin
    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.h5")
    dd1 = IMASdd.hdf2imas(filename)
    dd2 = IMASdd.hdf2imas(filename)

    @test dd1 == dd2
    @test dd1.core_sources == dd2.core_sources

    dd2.equilibrium.time_slice[1].profiles_1d.volume[2] += 1e-15
    @test !isequal(dd1, dd2)
    @test isapprox(dd1, dd2)

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