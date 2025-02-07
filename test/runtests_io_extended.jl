using IMASdd
using Test
import IMASdd.HDF5 as HDF5
import IMASdd as IMAS

include(joinpath(@__DIR__,"test_expressions_dicts.jl"))

@testset "extended HDF IO" begin

    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample_with_attrs.h5")
    dd = hdf2imas(filename)
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
        @test dd == hdf2imas(filename, "/")
        @test dd == hdf2imas(joinpath(dirname(@__DIR__), "sample", "omas_sample.h5"), "/")

        hdf2imas(filename, "/"; error_on_missing_coordinates=false)

        # IDS example
        eq = hdf2imas(filename, "/equilibrium")
        @test dd.equilibrium == eq

        # IDSvector example
        source = hdf2imas(filename, "/core_sources/source")
        @test dd.core_sources.source == source

        # Dataset examples
        glob_time = hdf2imas(filename, "/global_time")
        @test dd.global_time == glob_time

        vaccum_b0 = hdf2imas(filename, "/equilibrium/vacuum_toroidal_field/b0")
        @test dd.equilibrium.vacuum_toroidal_field.b0 == vaccum_b0
    end


    @testset "h5merge and read_combined_h5" begin
        tmp_dir1 = mktempdir()
        tmp_dir2 = mktempdir()
        tmp_dir3 = mktempdir()

        combined_file_name = joinpath(tmp_dir3, "combined_test.h5")

        imas2hdf(dd, joinpath(tmp_dir1, "11.h5"); strict=false, freeze=false, desc="This is in tmp_dir1")
        imas2hdf(dd, joinpath(tmp_dir1, "22.h5"); strict=false, freeze=false, desc="This is in tmp_dir1")
        imas2hdf(dd, joinpath(tmp_dir2, "aa.h5"); strict=false, freeze=false, desc="This is in tmp_dir2")
        imas2hdf(dd, joinpath(tmp_dir2, "bb.h5"); strict=false, freeze=false, desc="This is in tmp_dir2")

        # Merge files in a single directory
        h5merge(combined_file_name, tmp_dir1; mode="w", include_base_dir=true)
        fid = HDF5.h5open(combined_file_name, "r")
        @test keys(fid) == [basename(tmp_dir1)]
        @test keys(fid[basename(tmp_dir1)]) == ["11.h5", "22.h5"]
        close(fid)

        h5merge(combined_file_name, tmp_dir1; mode="w", include_base_dir=false)
        fid = HDF5.h5open(combined_file_name, "r")
        @test keys(fid) == ["11.h5", "22.h5"]
        close(fid)

        # Merge files in multiple directories
        h5merge(combined_file_name, [tmp_dir1, tmp_dir2]; mode="w", include_base_dir=true)
        fid = HDF5.h5open(combined_file_name, "r")
        @test Set(keys(fid)) == Set(basename.([tmp_dir1, tmp_dir2]))
        @test keys(fid[basename(tmp_dir1)]) == ["11.h5", "22.h5"]
        @test keys(fid[basename(tmp_dir2)]) == ["aa.h5", "bb.h5"]
        close(fid)

        h5merge(combined_file_name, [tmp_dir1, tmp_dir2]; mode="w", include_base_dir=false)
        fid = HDF5.h5open(combined_file_name, "r")
        @test keys(fid) == ["11.h5", "22.h5", "aa.h5", "bb.h5"]
        close(fid)

        # Test different flags
        h5merge(combined_file_name, [tmp_dir1, tmp_dir2]; mode="w", include_base_dir=false)
        h5merge(combined_file_name, [tmp_dir1, tmp_dir2]; mode="a", include_base_dir=false, skip_existing_entries=false)
        h5merge(combined_file_name, [tmp_dir1, tmp_dir2]; mode="a", include_base_dir=false, skip_existing_entries=true)
        fid = HDF5.h5open(combined_file_name, "r")
        @test keys(fid) == ["11.h5", "22.h5", "aa.h5", "bb.h5"]
        close(fid)

        # Merge including non-hdf5 files (txt and binaryfile)
        touch(joinpath(tmp_dir1,"empty.txt"))
        open(joinpath(tmp_dir1, "test.txt"), "w") do f
            return write(f, "This is a test text file.")
        end
        open(joinpath(tmp_dir1, "test_text"), "w") do f
            return write(f, "This is a test text file, but without extension")
        end
        open(joinpath(tmp_dir2, "tmp_binary_file"), "w") do f
            return write(f, UInt8[0x01, 0x02, 0xFF, 0xAB])
        end
        cp(joinpath(dirname(@__DIR__), "sample", "omas_sample.h5"), joinpath(tmp_dir1, "no_attrs.h5"))

        h5merge(combined_file_name, [tmp_dir1, tmp_dir2]; mode="w", include_base_dir=false)

        # Test read_combined_h5
        outDict = read_combined_h5(combined_file_name)
        @test keys(outDict) == Set(["/11.h5", "/22.h5", "/aa.h5", "/bb.h5", "/empty.txt", "/test.txt","/test_text", "/tmp_binary_file", "/no_attrs.h5"])
        @test outDict["/test.txt"] == "This is a test text file."

        # pattern test (read only .h5 files)
        outDict = read_combined_h5(combined_file_name; pattern=r"\.h5$")
        @test keys(outDict) == Set(["/11.h5", "/22.h5", "/aa.h5", "/bb.h5", "/no_attrs.h5"])

        # pattern test (read only .txt files)
        outDict = read_combined_h5(combined_file_name; pattern=r"\.txt$")
        @test keys(outDict) == Set(["/test.txt", "/empty.txt"])

        h5merge(combined_file_name, [tmp_dir1, tmp_dir2]; mode="w", include_base_dir=true, verbose=true);
        outDict = read_combined_h5(combined_file_name)
        @test outDict[joinpath("/", basename(tmp_dir1), "test.txt")] == "This is a test text file."
    end
end



@testset "Test edge cases" begin

    ori_dd = hdf2imas(joinpath(dirname(@__DIR__), "sample", "omas_sample.h5"))
    new_dd = hdf2imas(joinpath(dirname(@__DIR__), "sample", "omas_sample_with_attrs.h5"))

    @test new_dd == ori_dd

    tmp_dir = mktempdir()

    imas2hdf(ori_dd, joinpath(tmp_dir, "test.h5"); target_group="/")
    imas2hdf(ori_dd, joinpath(tmp_dir, "test.h5"); target_group="///")
    imas2hdf(ori_dd, joinpath(tmp_dir, "test.h5"); target_group="a/b/c")
    imas2hdf(ori_dd, joinpath(tmp_dir, "test.h5"); target_group="/", mode="a")

    imas2hdf(ori_dd, joinpath(tmp_dir, "test.h5"); target_group="/ori_dd", mode="w")
    @test_throws ErrorException imas2hdf(ori_dd, joinpath(tmp_dir, "test.h5"); target_group="/ori_dd", mode="a")
    imas2hdf(ori_dd, joinpath(tmp_dir, "test.h5"); target_group="/ori_dd", mode="a", overwrite=true, verbose=true);

    dd = IMAS.dd()
    resize!(dd.core_profiles.profiles_1d)
    dyexp["core_profiles.profiles_1d[:].electrons.temperature"] = (x; _...) -> x^2
    imas2hdf(dd, joinpath(tmp_dir, "test.h5"))
end