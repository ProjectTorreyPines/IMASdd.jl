using Test
using IMASdd

@testset "f2.jl Regression Tests" begin

    # Setup test data structures
    dd = IMASdd.dd()
    resize!(dd.core_profiles.profiles_1d, 3)
    resize!(dd.core_transport.model, 2)
    resize!(dd.core_sources.source, 2)

    # Wall with nested structure
    wall = IMASdd.wall()
    resize!(wall.description_2d, 2)
    resize!(wall.description_2d[1].mobile.unit, 3)
    resize!(wall.description_2d[1].mobile.unit[2].outline, 4)

    # Standalone IDS without parent
    standalone_crp1d = IMASdd.core_profiles__profiles_1d()
    standalone_wall_desc = IMASdd.wall__description_2d()
    resize!(standalone_wall_desc.mobile.unit, 2)
    resize!(standalone_wall_desc.mobile.unit[2].outline, 2)

    @testset "f2p correctness" begin
        # Basic IDS path
        @test IMASdd.f2p(dd.core_profiles) == ["core_profiles"]

        # IDSvector
        @test IMASdd.f2p(dd.core_profiles.profiles_1d) == ["core_profiles", "profiles_1d", "0"]

        # IDSvectorElement with index
        @test IMASdd.f2p(dd.core_profiles.profiles_1d[1]) == ["core_profiles", "profiles_1d", "1"]
        @test IMASdd.f2p(dd.core_profiles.profiles_1d[2]) == ["core_profiles", "profiles_1d", "2"]
        @test IMASdd.f2p(dd.core_profiles.profiles_1d[3]) == ["core_profiles", "profiles_1d", "3"]

        # Nested IDS path (grid is inside profiles_1d)
        @test IMASdd.f2p(dd.core_profiles.profiles_1d[1].grid) == ["core_profiles", "profiles_1d", "1", "grid"]

        # Deeply nested structure with multiple vector levels
        @test IMASdd.f2p(wall.description_2d[1]) == ["wall", "description_2d", "1"]
        @test IMASdd.f2p(wall.description_2d[1].mobile) == ["wall", "description_2d", "1", "mobile"]
        @test IMASdd.f2p(wall.description_2d[1].mobile.unit[2]) == ["wall", "description_2d", "1", "mobile", "unit", "2"]
        @test IMASdd.f2p(wall.description_2d[1].mobile.unit[2].outline[3]) == ["wall", "description_2d", "1", "mobile", "unit", "2", "outline", "3"]

        # Standalone structure (no parent) - should have 0 for index
        @test IMASdd.f2p(standalone_crp1d) == ["core_profiles", "profiles_1d", "0"]
        @test IMASdd.f2p(standalone_wall_desc.mobile.unit[2].outline[1]) == ["wall", "description_2d", "0", "mobile", "unit", "2", "outline", "1"]
    end

    @testset "f2p with utime=true" begin
        # time_slice is IDSvectorTimeElement
        eq = IMASdd.equilibrium()
        resize!(eq.time_slice, 2)

        # Without utime - shows actual index
        @test IMASdd.f2p(eq.time_slice[1]) == ["equilibrium", "time_slice", "1"]

        # With utime=true - time indices become 0
        @test IMASdd.f2p(eq.time_slice[1]; utime=true) == ["equilibrium", "time_slice", "0"]
        @test IMASdd.f2p(eq.time_slice[2]; utime=true) == ["equilibrium", "time_slice", "0"]
    end

    @testset "f2i correctness" begin
        # f2i returns IMAS location string with actual indices
        @test IMASdd.f2i(dd.core_profiles) == "core_profiles"
        @test IMASdd.f2i(dd.core_profiles.profiles_1d[1]) == "core_profiles.profiles_1d[1]"
        @test IMASdd.f2i(dd.core_profiles.profiles_1d[2]) == "core_profiles.profiles_1d[2]"
        @test IMASdd.f2i(wall.description_2d[1].mobile.unit[2].outline[3]) == "wall.description_2d[1].mobile.unit[2].outline[3]"
    end

    @testset "f2u correctness" begin
        # f2u returns universal location with [:] for all indices
        @test IMASdd.f2u(dd.core_profiles) == "core_profiles"
        @test IMASdd.f2u(dd.core_profiles.profiles_1d[1]) == "core_profiles.profiles_1d[:]"
        @test IMASdd.f2u(dd.core_profiles.profiles_1d[1].grid) == "core_profiles.profiles_1d[:].grid"
        @test IMASdd.f2u(wall.description_2d[1].mobile.unit[2].outline[3]) == "wall.description_2d[:].mobile.unit[:].outline[:]"

        # f2u only accepts IDS objects, not strings or symbols
        @test_throws MethodError IMASdd.f2u(:core_profiles__profiles_1d___grid)
        @test_throws MethodError IMASdd.f2u("core_profiles__profiles_1d___grid")
    end

    @testset "ulocation correctness" begin
        # ulocation(IDS) - same as f2u
        @test IMASdd.ulocation(dd.core_profiles) == "core_profiles"
        @test IMASdd.ulocation(dd.core_profiles.profiles_1d[1]) == "core_profiles.profiles_1d[:]"

        # ulocation(IDS, field) - with field name appended
        @test IMASdd.ulocation(dd.core_profiles, :time) == "core_profiles.time"
        @test IMASdd.ulocation(dd.core_profiles.profiles_1d[1], :grid) == "core_profiles.profiles_1d[:].grid"
    end

    @testset "location correctness" begin
        # location(IDS) - same as f2i
        @test IMASdd.location(dd.core_profiles) == "core_profiles"
        @test IMASdd.location(dd.core_profiles.profiles_1d[1]) == "core_profiles.profiles_1d[1]"

        # location(IDS, field) - with field name appended
        @test IMASdd.location(dd.core_profiles, :time) == "core_profiles.time"
        @test IMASdd.location(dd.core_profiles.profiles_1d[1], :grid) == "core_profiles.profiles_1d[1].grid"
    end

    @testset "i2p correctness" begin
        # Parse IMAS location string to path array
        @test IMASdd.i2p("core_profiles") == ["core_profiles"]
        @test IMASdd.i2p("core_profiles.profiles_1d[1]") == ["core_profiles", "profiles_1d", "1"]
        @test IMASdd.i2p("core_profiles.profiles_1d[1].grid") == ["core_profiles", "profiles_1d", "1", "grid"]
        @test IMASdd.i2p("core_profiles.profiles_1d[:].grid") == ["core_profiles", "profiles_1d", ":", "grid"]
        @test IMASdd.i2p("wall.description_2d[1].mobile.unit[2].outline[3]") == ["wall", "description_2d", "1", "mobile", "unit", "2", "outline", "3"]

        # Edge cases
        @test IMASdd.i2p("dd") == ["dd"]
        @test IMASdd.i2p("core_transport.model[0].coefficients") == ["core_transport", "model", "0", "coefficients"]
    end

    @testset "p2i correctness" begin
        # Convert path array back to IMAS location string
        @test IMASdd.p2i(["core_profiles"]) == "core_profiles"
        @test IMASdd.p2i(["core_profiles", "profiles_1d", "1"]) == "core_profiles.profiles_1d[1]"
        @test IMASdd.p2i(["core_profiles", "profiles_1d", "1", "grid"]) == "core_profiles.profiles_1d[1].grid"
        @test IMASdd.p2i(["core_profiles", "profiles_1d", ":", "grid"]) == "core_profiles.profiles_1d[:].grid"
        @test IMASdd.p2i(["wall", "description_2d", "1", "mobile", "unit", "2", "outline", "3"]) == "wall.description_2d[1].mobile.unit[2].outline[3]"
    end

    @testset "i2u correctness" begin
        # Convert IMAS location to universal location (indices -> [:])
        @test IMASdd.i2u("core_profiles") == "core_profiles"
        @test IMASdd.i2u("core_profiles.time") == "core_profiles.time"
        @test IMASdd.i2u("simple.path") == "simple.path"  # No brackets - fast path
        @test IMASdd.i2u("core_profiles.profiles_1d[1].grid") == "core_profiles.profiles_1d[:].grid"
        @test IMASdd.i2u("core_profiles.profiles_1d[42].grid") == "core_profiles.profiles_1d[:].grid"
        @test IMASdd.i2u("wall.description_2d[0].mobile.unit[1].outline[2]") == "wall.description_2d[:].mobile.unit[:].outline[:]"

        # Already universal - should stay unchanged
        @test IMASdd.i2u("core_profiles.profiles_1d[:].grid") == "core_profiles.profiles_1d[:].grid"
    end

    @testset "Round-trip consistency" begin
        # i2p -> p2i should be identity
        test_paths = [
            "core_profiles",
            "core_profiles.profiles_1d[1].grid",
            "core_profiles.profiles_1d[:].electrons.density",
            "wall.description_2d[1].mobile.unit[2].outline[3]",
            "equilibrium.time_slice[:].boundary.outline",
            "dd"
        ]

        for path in test_paths
            parsed = IMASdd.i2p(path)
            reconstructed = IMASdd.p2i(parsed)
            @test reconstructed == path
        end

        # f2i -> i2p -> p2i should round-trip for IDS objects
        test_structures = [
            dd.core_profiles,
            dd.core_profiles.profiles_1d[1],
            dd.core_profiles.profiles_1d[1].grid,
            wall.description_2d[1].mobile.unit[2]
        ]

        for ids in test_structures
            imas_loc = IMASdd.f2i(ids)
            parsed = IMASdd.i2p(imas_loc)
            reconstructed = IMASdd.p2i(parsed)
            @test reconstructed == imas_loc
        end

        # f2u should equal i2u(f2i())
        for ids in test_structures
            @test IMASdd.f2u(ids) == IMASdd.i2u(IMASdd.f2i(ids))
        end
    end

    @testset "fs2u type-based lookup" begin
        # fs2u takes Type and returns universal location
        @test IMASdd.fs2u(typeof(dd.core_profiles)) == "core_profiles"
        @test IMASdd.fs2u(typeof(dd.core_profiles.profiles_1d[1])) == "core_profiles.profiles_1d[:]"
        @test IMASdd.fs2u(typeof(dd.core_profiles.profiles_1d[1].grid)) == "core_profiles.profiles_1d[:].grid"
    end

    @testset "f2p_name correctness" begin
        # f2p_name returns the IDS name as string
        @test IMASdd.f2p_name(dd) == "dd"
        @test IMASdd.f2p_name(dd.core_profiles) == "core_profiles"
        @test IMASdd.f2p_name(dd.core_profiles.profiles_1d) == "profiles_1d"
        @test IMASdd.f2p_name(dd.core_profiles.profiles_1d[1]) == "1"
        @test IMASdd.f2p_name(dd.core_profiles.profiles_1d[1].grid) == "grid"

        # Type-based f2p_name
        @test IMASdd.f2p_name(typeof(dd.core_profiles)) == "core_profiles"
        @test IMASdd.f2p_name(typeof(dd.core_profiles.profiles_1d[1])) == "profiles_1d"

        # Detached IDS shows [DETACHED] suffix
        @test IMASdd.f2p_name(standalone_crp1d) == "profiles_1d [DETACHED]"
    end

    @testset "Edge cases and error handling" begin
        # Empty vector element access
        empty_ids = IMASdd.core_profiles()
        @test IMASdd.f2p(empty_ids) == ["core_profiles"]

        # DD root
        @test IMASdd.f2p(dd) == ["dd"]
        @test IMASdd.f2i(dd) == "dd"
        @test IMASdd.f2u(dd) == "dd"
    end

end
