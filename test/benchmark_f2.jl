using IMASdd
using BenchmarkTools
using Test
using Printf

# Benchmark suite for f2.jl performance tracking
# This file provides comprehensive benchmarks for all f2.jl functions to track performance improvements

@testset "f2.jl Performance Benchmarks" begin
    println("=== IMASdd f2.jl Performance Benchmarks ===")

    # Setup test data structures
    dd = IMASdd.dd()
    resize!(dd.core_profiles.profiles_1d, 3)
    resize!(dd.core_transport.model, 2)
    resize!(dd.core_sources.source, 2)

    # Create nested structure for comprehensive testing
    wall = IMASdd.wall()
    resize!(wall.description_2d, 2)
    resize!(wall.description_2d[1].mobile.unit, 3)
    resize!(wall.description_2d[1].mobile.unit[2].outline, 4)

    # Standalone structures for testing
    crp1d = IMASdd.core_profiles__profiles_1d()
    eq = IMASdd.equilibrium()

    # Test data for string operations
    test_paths = [
        "core_profiles.profiles_1d[1].grid",
        "core_profiles.profiles_1d[:].electrons.density",
        "wall.description_2d[1].mobile.unit[2].outline[3]",
        "equilibrium.time_slice[:].boundary.outline",
        "core_transport.model[0].coefficients.d_n",
        "dd",
        "summary.global_quantities"
    ]

    test_ulocs = [
        "core_profiles.profiles_1d[:].grid",
        "core_profiles.profiles_1d[:].electrons.density",
        "wall.description_2d[:].mobile.unit[:].outline[:]",
        "equilibrium.time_slice[:].boundary.outline",
        "core_transport.model[:].coefficients.d_n"
    ]

    println("\n--- ulocation() Benchmarks ---")

    @testset "ulocation(IDS, Symbol)" begin
        # Benchmark ulocation with IDS and field
        result = @benchmark IMASdd.ulocation($(dd.core_profiles), :time) seconds = 2
        @printf "ulocation(IDS, Symbol): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time
        @test IMASdd.ulocation(dd.core_profiles, :time) == "core_profiles.time"

        # Test with nested structure
        result = @benchmark IMASdd.ulocation($(dd.core_profiles.profiles_1d[1]), :grid) seconds = 2
        @printf "ulocation(nested IDS, Symbol): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time
    end

    @testset "ulocation(IDS)" begin
        # Benchmark ulocation with IDS only
        result = @benchmark IMASdd.ulocation($(dd.core_profiles)) seconds = 2
        @printf "ulocation(IDS): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        result = @benchmark IMASdd.ulocation($(dd.core_profiles.profiles_1d[1])) seconds = 2
        @printf "ulocation(IDSvectorElement): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        result = @benchmark IMASdd.ulocation($(dd.core_profiles.profiles_1d)) seconds = 2
        @printf "ulocation(IDSvector): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time
    end

    println("\n--- location() Benchmarks ---")

    @testset "location(IDS, Symbol)" begin
        result = @benchmark IMASdd.location($(dd.core_profiles), :time) seconds = 2
        @printf "location(IDS, Symbol): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time
        @test IMASdd.location(dd.core_profiles, :time) == "core_profiles.time"

        result = @benchmark IMASdd.location($(dd.core_profiles.profiles_1d[1]), :grid) seconds = 2
        @printf "location(nested IDS, Symbol): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time
    end

    @testset "location(IDS)" begin
        result = @benchmark IMASdd.location($(dd.core_profiles)) seconds = 2
        @printf "location(IDS): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        result = @benchmark IMASdd.location($(dd.core_profiles.profiles_1d[1])) seconds = 2
        @printf "location(IDSvectorElement): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time
    end

    println("\n--- f2u() Benchmarks ---")

    @testset "f2u/fs2u" begin
        # Test f2u with different IDS types
        result = @benchmark IMASdd.f2u($(dd.core_profiles)) seconds = 2
        @printf "f2u(IDS): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        result = @benchmark IMASdd.f2u($(dd.core_profiles.profiles_1d[1])) seconds = 2
        @printf "f2u(IDSvectorElement): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        result = @benchmark IMASdd.f2u($(dd.core_profiles.profiles_1d)) seconds = 2
        @printf "f2u(IDSvector): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        # Test fs2u (memoized function - should be fast after first call)
        result = @benchmark IMASdd.fs2u($(typeof(dd.core_profiles))) seconds = 2
        @printf "fs2u(Type): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time
    end

    println("\n--- f2i() Benchmarks ---")

    @testset "f2i" begin
        # Test f2i with different structures
        result = @benchmark IMASdd.f2i($(dd.core_profiles)) seconds = 2
        @printf "f2i(IDS): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        result = @benchmark IMASdd.f2i($(dd.core_profiles.profiles_1d[1])) seconds = 2
        @printf "f2i(IDSvectorElement): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        result = @benchmark IMASdd.f2i($(wall.description_2d[1].mobile.unit[2].outline[1])) seconds = 2
        @printf "f2i(deeply nested): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time
    end

    println("\n--- f2p() Benchmarks ---")

    @testset "f2p" begin
        # Test f2p with different structures
        result = @benchmark IMASdd.f2p($(dd.core_profiles)) seconds = 2
        @printf "f2p(IDS): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        result = @benchmark IMASdd.f2p($(dd.core_profiles.profiles_1d[1])) seconds = 2
        @printf "f2p(IDSvectorElement): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        result = @benchmark IMASdd.f2p($(wall.description_2d[1].mobile.unit[2].outline[1])) seconds = 2
        @printf "f2p(deeply nested): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        # Verify correctness
        @test IMASdd.f2p(wall.description_2d[1].mobile.unit[2].outline[1]) == ["wall", "description_2d", "1", "mobile", "unit", "2", "outline", "1"]
    end

    println("\n--- f2p_name() Benchmarks ---")

    @testset "f2p_name" begin
        result = @benchmark IMASdd.f2p_name($(dd.core_profiles.profiles_1d[1])) seconds = 2
        @printf "f2p_name(IDSvectorElement): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        result = @benchmark IMASdd.f2p_name($(typeof(dd.core_profiles))) seconds = 2
        @printf "f2p_name(Type): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time
    end

    println("\n--- String Parsing Benchmarks ---")

    @testset "i2p" begin
        # Test i2p with various path complexities
        for (i, path) in enumerate(test_paths)
            result = @benchmark IMASdd.i2p($path) seconds = 1
            @printf "i2p(path_%d): %.2f ns ± %.2f ns\n" i minimum(result).time maximum(result).time
        end

        # Verify correctness
        @test IMASdd.i2p("core_profiles.profiles_1d[1].grid") == ["core_profiles", "profiles_1d", "1", "grid"]
        @test IMASdd.i2p("wall.description_2d[1].mobile.unit[2].outline[3]") == ["wall", "description_2d", "1", "mobile", "unit", "2", "outline", "3"]
    end

    @testset "p2i" begin
        # Test p2i with various parsed paths
        test_parsed_paths = [
            ["core_profiles", "profiles_1d", "1", "grid"],
            ["core_profiles", "profiles_1d", ":", "electrons", "density"],
            ["wall", "description_2d", "1", "mobile", "unit", "2", "outline", "3"],
            ["equilibrium", "time_slice", ":", "boundary", "outline"],
            ["dd"]
        ]

        for (i, parsed_path) in enumerate(test_parsed_paths)
            result = @benchmark IMASdd.p2i($parsed_path) seconds = 1
            @printf "p2i(parsed_%d): %.2f ns ± %.2f ns\n" i minimum(result).time maximum(result).time
        end

        # Verify correctness  
        @test IMASdd.p2i(["core_profiles", "profiles_1d", "1", "grid"]) == "core_profiles.profiles_1d[1].grid"
        @test IMASdd.p2i(["core_profiles", "profiles_1d", ":", "grid"]) == "core_profiles.profiles_1d[:].grid"
    end

    @testset "i2u" begin
        # Test i2u with various IMAS locations
        for (i, path) in enumerate(test_paths)
            result = @benchmark IMASdd.i2u($path) seconds = 1
            @printf "i2u(path_%d): %.2f ns ± %.2f ns\n" i minimum(result).time maximum(result).time
        end

        # Test fast path (no brackets)
        simple_path = "core_profiles.time"
        result = @benchmark IMASdd.i2u($simple_path) seconds = 1
        @printf "i2u(no_brackets): %.2f ns ± %.2f ns\n" minimum(result).time maximum(result).time

        # Verify correctness
        @test IMASdd.i2u("core_profiles.profiles_1d[1].grid") == "core_profiles.profiles_1d[:].grid"
        @test IMASdd.i2u("wall.description_2d[0].mobile.unit[1].outline[2]") == "wall.description_2d[:].mobile.unit[:].outline[:]"
        @test IMASdd.i2u("simple.path") == "simple.path"  # No brackets case
    end

    println("\n--- Round-trip Consistency Tests ---")

    @testset "Round-trip consistency" begin
        # Test that i2p -> p2i round-trips correctly
        for path in test_paths
            parsed = IMASdd.i2p(path)
            reconstructed = IMASdd.p2i(parsed)
            @test reconstructed == path
        end

        # Test that parsing and reconstruction maintain correctness
        test_structures = [
            dd.core_profiles,
            dd.core_profiles.profiles_1d[1],
            wall.description_2d[1].mobile.unit[2]
        ]

        for test_struct in test_structures
            # f2i -> i2p -> p2i should round-trip
            imas_loc = IMASdd.f2i(test_struct)
            parsed = IMASdd.i2p(imas_loc)
            reconstructed = IMASdd.p2i(parsed)
            @test reconstructed == imas_loc

            # f2u -> i2u should be consistent
            univ_loc = IMASdd.f2u(test_struct)
            from_imas = IMASdd.i2u(imas_loc)
            @test univ_loc == from_imas
        end
    end

    println("\n--- Memory Allocation Tests ---")

    @testset "Memory allocations" begin
        # Test that optimized functions don't allocate excessively
        println("Allocation benchmarks (should show minimal allocations):")

        # f2i allocation test
        alloc_result = @benchmark IMASdd.f2i($(dd.core_profiles.profiles_1d[1])) seconds = 1
        @printf "f2i allocations: %d bytes, %d allocs\n" alloc_result.memory alloc_result.allocs

        # i2p allocation test  
        test_path = "core_profiles.profiles_1d[1].grid.rho_tor_norm"
        alloc_result = @benchmark IMASdd.i2p($test_path) seconds = 1
        @printf "i2p allocations: %d bytes, %d allocs\n" alloc_result.memory alloc_result.allocs

        # i2u allocation test (no brackets - should be very fast)
        simple_path = "core_profiles.time"
        alloc_result = @benchmark IMASdd.i2u($simple_path) seconds = 1
        @printf "i2u (no brackets) allocations: %d bytes, %d allocs\n" alloc_result.memory alloc_result.allocs

        # i2u allocation test (with brackets)
        complex_path = "core_profiles.profiles_1d[1].electrons.density"
        alloc_result = @benchmark IMASdd.i2u($complex_path) seconds = 1
        @printf "i2u (with brackets) allocations: %d bytes, %d allocs\n" alloc_result.memory alloc_result.allocs
    end

    println("\n=== Benchmark Summary Complete ===")
    println("All f2.jl functions benchmarked successfully!")
    println("Use this file to track performance improvements over time.")
end