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

@testset "extended HDF IO" begin
    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample_with_attrs.h5")
    dd = IMAS.hdf2imas(filename)
    @testset "hdf_with_attrs" begin
        filename = joinpath(dirname(@__DIR__), "sample", "omas_sample_with_attrs.h5")
        fid = HDF5.h5open(filename)

        # Check attributes of the root (dd)
        attr = HDF5.attrs(fid)
        @test attr["abstract_type"] == "IDS"
        @test attr["concrete_type"] == "IMASdd.dd{Float64}"
        @test attr["description"] == "This is the same omas sample with attributes."
        @test attr["freeze"] == "true"
        @test attr["strict"] == "false"

        # Check attributes of time_slice (example of IDSvector)
        attr = HDF5.attrs(fid["/equilibrium/time_slice"])
        @test attr["abstract_type"] == "IDSvector"
        @test attr["concrete_type"] == "IMASdd.IDSvector{IMASdd.equilibrium__time_slice{Float64}}"
    end

    @testset "hdf2imas with target path" begin
        # Root IDS (dd)
        @test dd == IMAS.hdf2imas(filename, "/")
        @test dd == IMAS.hdf2imas(joinpath(dirname(@__DIR__), "sample", "omas_sample_with_attrs.h5"), "/")

        # IDS example
        eq = IMAS.hdf2imas(filename, "/equilibrium")
        @test dd.equilibrium == eq

        # IDSvector example
        source = IMAS.hdf2imas(filename, "/core_sources/source")
        @test dd.core_sources.source == source
    end


    @testset "h5merge and read_combined_h5" begin
        tmp_dir1 = mktempdir()
        tmp_dir2 = mktempdir()
        tmp_dir3 = mktempdir()

        combined_file_name = joinpath(tmp_dir3, "combined_test.h5")

        IMAS.imas2hdf(dd, joinpath(tmp_dir1, "11.h5"); strict=false, freeze=false, desc="This is in tmp_dir1")
        IMAS.imas2hdf(dd, joinpath(tmp_dir1, "22.h5"); strict=false, freeze=false, desc="This is in tmp_dir1")
        IMAS.imas2hdf(dd, joinpath(tmp_dir2, "aa.h5"); strict=false, freeze=false, desc="This is in tmp_dir2")
        IMAS.imas2hdf(dd, joinpath(tmp_dir2, "bb.h5"); strict=false, freeze=false, desc="This is in tmp_dir2")

        # Merge files in a single directory
        IMAS.h5merge(combined_file_name, tmp_dir1; mode="w", include_base_dir=true)
        fid = HDF5.h5open(combined_file_name, "r")
        @test keys(fid) == [basename(tmp_dir1)]
        @test keys(fid[basename(tmp_dir1)]) == ["11.h5", "22.h5"]
        close(fid)

        IMAS.h5merge(combined_file_name, tmp_dir1; mode="w", include_base_dir=false)
        fid = HDF5.h5open(combined_file_name, "r")
        @test keys(fid) == ["11.h5", "22.h5"]
        close(fid)

        # Merge files in multiple directories
        IMAS.h5merge(combined_file_name, [tmp_dir1, tmp_dir2]; mode="w", include_base_dir=true)
        fid = HDF5.h5open(combined_file_name, "r")
        @test keys(fid) == basename.([tmp_dir1, tmp_dir2])
        @test keys(fid[basename(tmp_dir1)]) == ["11.h5", "22.h5"]
        @test keys(fid[basename(tmp_dir2)]) == ["aa.h5", "bb.h5"]
        close(fid)

        IMAS.h5merge(combined_file_name, [tmp_dir1, tmp_dir2]; mode="w", include_base_dir=false)
        fid = HDF5.h5open(combined_file_name, "r")
        @test keys(fid) == ["11.h5", "22.h5", "aa.h5", "bb.h5"]
        close(fid)

        # Merge including non-hdf5 files
        open(joinpath(tmp_dir1, "test.txt"), "w") do f
            return write(f, "This is a test text file.")
        end
        IMAS.h5merge(combined_file_name, [tmp_dir1, tmp_dir2]; mode="w", include_base_dir=false)

        outDict = IMAS.read_combined_h5(combined_file_name)
        @test keys(outDict) == Set(["/11.h5", "/22.h5", "/aa.h5", "/bb.h5", "/test.txt"])
        @test outDict["/test.txt"] == "This is a test text file."

        # pattern test (read only .h5 files)
        outDict = IMAS.read_combined_h5(combined_file_name; pattern=r"\.h5$")
        @test keys(outDict) == Set(["/11.h5", "/22.h5", "/aa.h5", "/bb.h5"])

        # pattern test (read only .txt files)
        outDict = IMAS.read_combined_h5(combined_file_name; pattern=r"\.txt$")
        @test keys(outDict) == Set(["/test.txt"])

        IMAS.h5merge(combined_file_name, [tmp_dir1, tmp_dir2]; mode="w", include_base_dir=true)
        outDict = IMAS.read_combined_h5(combined_file_name)
        @test outDict[joinpath("/", basename(tmp_dir1), "test.txt")] == "This is a test text file."
    end
end

@testset "isequal" begin
    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.h5")
    dd1 = IMAS.hdf2imas(filename)
    dd2 = IMAS.hdf2imas(filename)

    @test dd1 == dd2
    @test dd1.core_sources == dd2.core_sources

    dd2.core_sources.time[] = 1.0
    @test !isequal(dd1, dd2)
    @test !isequal(dd1.core_sources, dd2.core_sources)
    @test isequal(dd1.core_sources.source, dd2.core_sources.source)

    resize!(dd2.core_sources.source,5)
    @test !isequal(dd1.core_sources.source, dd2.core_sources.source)

    dd2.core_sources.source[2].identifier.index=0
    @test (dd1.core_sources.source[1:3].==dd2.core_sources.source[1:3]) == BitVector([1,0,1])
end