using IMASdd
import IMASdd
using Test

@testset "diagnose_shared_objects" begin
    # Create a dd with ions
    dd = IMASdd.dd()
    resize!(dd.core_profiles.profiles_1d, 1)
    cp1d = dd.core_profiles.profiles_1d[1]
    # Must set coordinate grid first
    cp1d.grid.rho_tor_norm = range(0, 1, 10)
    resize!(cp1d.ion, 3)

    # Test 1: Detect shared objects (intentional sharing - bad practice)
    @testset "detect sharing" begin
        shared_temp = rand(10)
        for ion in cp1d.ion
            ion.temperature = shared_temp  # All ions share the same array
        end

        report = IMASdd.diagnose_shared_objects(dd.core_profiles)

        @test !isempty(report.shared)
        @test report isa IMASdd.SharedObjectReport

        # Should detect 3 paths sharing the same objectid
        found_sharing = false
        for group in report.shared
            if length(group.paths) >= 3
                found_sharing = true
                @test all(contains(p, "ion") && contains(p, "temperature") for p in group.paths)
            end
        end
        @test found_sharing
    end

    # Test 2: Independent arrays should not be flagged
    @testset "independent arrays" begin
        # Reset with independent arrays
        for (i, ion) in enumerate(cp1d.ion)
            ion.temperature = rand(10) .* i  # Each ion gets its own array
        end

        report = IMASdd.diagnose_shared_objects(dd.core_profiles)

        # Check that temperature arrays are not in shared
        temp_shared = false
        for group in report.shared
            if any(contains(p, "temperature") for p in group.paths)
                temp_shared = true
            end
        end
        @test !temp_shared
    end

    # Test 3: Report display and iteration
    @testset "report display and access" begin
        shared_array = rand(10)
        cp1d.ion[1].density = shared_array
        cp1d.ion[2].density = shared_array

        report = IMASdd.diagnose_shared_objects(dd.core_profiles)
        @test !isempty(report.shared)

        # Test indexed access
        @test report[1] isa NamedTuple
        @test haskey(report[1], :id)
        @test haskey(report[1], :paths)

        # Test iteration
        count = 0
        for group in report
            count += 1
            @test group.id isa UInt
            @test group.paths isa Vector{String}
        end
        @test count == length(report)
    end

    # Test 4: SharedObjectReport construction
    @testset "report construction" begin
        # Empty report
        report = IMASdd.SharedObjectReport(IMASdd.SharedObjectGroup[], 0)
        @test isempty(report)
        @test length(report) == 0

        # Non-empty report
        shared = [(id=UInt(12345), paths=["path.a", "path.b"])]
        report2 = IMASdd.SharedObjectReport(shared, 10)
        @test !isempty(report2)
        @test length(report2) == 1
        @test report2[1].id == UInt(12345)
        @test report2[1].paths == ["path.a", "path.b"]

        # Test show method
        buf = IOBuffer()
        show(buf, report2)
        str = String(take!(buf))
        @test contains(str, "SharedObjectReport")
        @test contains(str, "1 shared")
    end

    # Test 5: Works on DD level
    @testset "DD level scan" begin
        dd2 = IMASdd.dd()
        resize!(dd2.core_profiles.profiles_1d, 1)
        dd2.core_profiles.profiles_1d[1].grid.rho_tor_norm = range(0, 1, 10)

        report = IMASdd.diagnose_shared_objects(dd2)
        @test report isa IMASdd.SharedObjectReport
    end

    # Test 6: Works on IDSvector
    @testset "IDSvector scan" begin
        dd3 = IMASdd.dd()
        resize!(dd3.core_profiles.profiles_1d, 2)
        dd3.core_profiles.profiles_1d[1].grid.rho_tor_norm = range(0, 1, 10)
        dd3.core_profiles.profiles_1d[2].grid.rho_tor_norm = range(0, 1, 10)

        report = IMASdd.diagnose_shared_objects(dd3.core_profiles.profiles_1d)
        @test report isa IMASdd.SharedObjectReport
    end

    # Test 7: Empty IDS should work without error
    @testset "empty IDS" begin
        dd4 = IMASdd.dd()
        report = IMASdd.diagnose_shared_objects(dd4)
        @test report isa IMASdd.SharedObjectReport
        @test isempty(report.shared)
    end

    # Test 8: Realistic scenario with sample data
    @testset "realistic scenario" begin
        sample_path = joinpath(@__DIR__, "..", "sample", "omas_sample.json")
        if isfile(sample_path)
            dd5 = IMASdd.json2imas(sample_path)

            # Create cross-IDS sharing (same array in different IDSs)
            aa = rand(11)
            dd5.core_sources.source[2].profiles_1d[1].electrons.energy = aa
            dd5.core_sources.source[5].profiles_1d[1].electrons.energy = aa

            # Create sharing within same IDS and cross-IDS
            bb = rand(11)
            dd5.core_profiles.profiles_1d[1].ion[2].temperature = bb
            dd5.core_profiles.profiles_1d[1].ion[1].temperature = bb
            dd5.core_sources.source[6].profiles_1d[1].electrons.energy = bb

            report = IMASdd.diagnose_shared_objects(dd5)

            # Should detect 2 groups of shared objects
            @test length(report) == 2

            # Check first group has 2 paths (aa)
            # Check second group has 3 paths (bb)
            path_counts = sort([length(g.paths) for g in report])
            @test path_counts == [2, 3]

            # Verify cross-IDS detection (core_profiles and core_sources)
            has_cross_ids = false
            for group in report
                has_profiles = any(contains(p, "core_profiles") for p in group.paths)
                has_sources = any(contains(p, "core_sources") for p in group.paths)
                if has_profiles && has_sources
                    has_cross_ids = true
                end
            end
            @test has_cross_ids
        else
            @warn "Sample file not found, skipping realistic scenario test"
        end
    end
end
