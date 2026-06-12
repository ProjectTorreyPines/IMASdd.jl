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
        compressed_size = stat(joinpath(test_dir, "test_comp.hdf")).size
        @info "Uncompressed file size: $uncompressed_size bytes"
        @info "Compressed file size: $compressed_size bytes"
        @test compressed_size < uncompressed_size
    end

    rm(test_dir; force=true, recursive=true)
end


@testset "isequal" begin
    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.h5")
    dd1 = IMASdd.hdf2imas(filename)
    dd2 = IMASdd.hdf2imas(filename)

    @test dd1 == dd2
    @test dd1.core_sources == dd2.core_sources

    dd2.core_sources.time[] = 1.0
    @test !isequal(dd1, dd2; verbose=true)
    @test !isequal(dd1.core_sources, dd2.core_sources; verbose=true)
    @test isequal(dd1.core_sources.source, dd2.core_sources.source; verbose=true)

    resize!(dd2.core_sources.source, 5)
    @test !isequal(dd1.core_sources.source, dd2.core_sources.source; verbose=true)

    dd2.core_sources.source[2].identifier.index = 0
    @test (dd1.core_sources.source[1:3] .== dd2.core_sources.source[1:3]) == BitVector([1, 0, 1])

    dd2.core_profiles.profiles_1d[1].j_total[2] = 0.0
    dd2.equilibrium.time_slice[1].profiles_1d.psi[[1, 7, 8, 13, 15]] .= rand(5)
    dd2.core_sources.source[1].identifier.name = "abcde"
    dd2.equilibrium.time_slice[].profiles_2d[1].psi[1:2, :] .= 0.0
    dd2.global_time = 100.0

    isequal(dd1, dd2; verbose=true)

    isequal(dd1.equilibrium, dd2; verbose=true)

    # isapprox test
    dd2 = deepcopy(dd1)
    dd2.equilibrium.time_slice[1].profiles_1d.volume[2] += 1e-13
    @test !isequal(dd1, dd2)
    @test isapprox(dd1, dd2)
    @test dd1 ≈ dd2
    dd2.equilibrium.time_slice[1].profiles_1d.volume[2] = dd1.equilibrium.time_slice[1].profiles_1d.volume[2]

    dd2.equilibrium.time_slice[1].profiles_1d.volume[2] += 1e-5
    @test !isapprox(dd1, dd2)
    @test dd1 ≉ dd2
    @test isapprox(dd1, dd2; atol=1e-4)
    dd2.equilibrium.time_slice[1].profiles_1d.volume[2] = dd1.equilibrium.time_slice[1].profiles_1d.volume[2]

end

@testset "stdout show IO" begin
    dd = IMASdd.hdf2imas(joinpath(dirname(@__DIR__), "sample", "omas_sample.h5"))

    @test_nowarn show(stdout, MIME("text/plain"), dd.equilibrium.time_slice[1].profiles_2d)
    @test_nowarn show(stdout, MIME("text/plain"), deepcopy(dd.equilibrium.time_slice[1].profiles_2d))

    @test_nowarn show(stdout, MIME("text/plain"), dd.equilibrium.time_slice[1].profiles_2d[1])
    @test_nowarn show(stdout, MIME("text/plain"), deepcopy(dd.equilibrium.time_slice[1].profiles_2d[1]))
end

@testset "JSON IO with complex numbers" begin
    dd = IMASdd.dd()
    resize!(dd.gyrokinetics_local.linear.wavevector, 1)
    resize!(dd.gyrokinetics_local.linear.wavevector[1].eigenmode, 1)
    dd.gyrokinetics_local.linear.wavevector[1].eigenmode[1].angle_pol = 0:2π/11:2π
    dd.gyrokinetics_local.linear.wavevector[1].eigenmode[1].time_norm = [1.0]
    dd.gyrokinetics_local.linear.wavevector[1].eigenmode[1].fields.a_field_parallel_perturbed_norm = (1.0 + 2.0im) * rand(10, 2)

    mktempdir() do folder
        @show folder
        IMASdd.imas2json(dd, joinpath(folder, "dd.json"))
        dd1 = IMASdd.json2imas(joinpath(folder, "dd.json"))

        @test dd == dd1
    end
end

# ===================================================================== #
#  Characterization of JSON NaN/Inf behavior, pinned so it stays         #
#  byte-identical across JSON.jl v0.21 and v1 (see io.jl version split).  #
# ===================================================================== #
@testset "JSON NaN/Inf round-trip" begin
    dd = IMASdd.dd()
    resize!(dd.equilibrium.time_slice, 1)
    dd.equilibrium.time_slice[1].profiles_1d.psi = [1.0, NaN, Inf, -Inf, 2.0]

    mktempdir() do folder
        path = joinpath(folder, "nan.json")
        IMASdd.imas2json(dd, path)

        # Golden on-disk format: non-finite floats serialize as these exact tokens
        txt = read(path, String)
        @test occursin("NaN", txt)
        @test occursin("Infinity", txt)
        @test occursin("-Infinity", txt)

        # Round-trip fidelity: assert element-wise (== / isequal would mask NaN handling)
        dd2 = IMASdd.json2imas(path; error_on_missing_coordinates=false)
        psi = dd2.equilibrium.time_slice[1].profiles_1d.psi
        @test length(psi) == 5
        @test psi[1] == 1.0
        @test isnan(psi[2])
        @test isinf(psi[3]) && psi[3] > 0
        @test isinf(psi[4]) && psi[4] < 0
        @test psi[5] == 2.0
    end
end

@testset "JSON global NaN/Inf serialization (downstream pattern)" begin
    # IMASdd's float hook is global: any JSON.json(...) in the session tolerates
    # NaN/Inf, which downstream packages (FUSE, SimulationParameters, OMAS) rely on.
    s = IMASdd.JSON.json(Dict("x" => [1.0, NaN, Inf, -Inf]), 1)
    @test occursin("NaN", s)
    @test occursin("Infinity", s)
    @test occursin("-Infinity", s)
end

@testset "JSON.json(dd) direct-call parity" begin
    # Serializing an IDS object directly (not via imas2json) must work
    dd = IMASdd.dd()
    resize!(dd.equilibrium.time_slice, 1)
    dd.equilibrium.time_slice[1].profiles_1d.psi = [0.1, 0.2, 0.3]

    s = IMASdd.JSON.json(dd)
    @test startswith(strip(s), "{")
    @test occursin("equilibrium", s)

    s2 = IMASdd.JSON.sprint(dd)
    @test occursin("equilibrium", s2)
end