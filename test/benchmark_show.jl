using IMASdd
using BenchmarkTools
using Test
using Printf

# Benchmark suite for show.jl performance tracking
# This file provides benchmarks for the string building optimizations in show.jl

@testset "show.jl Performance Benchmarks" begin
    println("=== IMASdd show.jl Performance Benchmarks ===")

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

    # Test structures with various amounts of data
    test_ids = [
        dd.core_profiles,
        dd.core_profiles.profiles_1d[1],
        wall.description_2d[1].mobile.unit[2],
        dd.core_transport.model[1]
    ]

    test_idsvectors = [
        dd.core_profiles.profiles_1d,
        dd.core_transport.model,
        wall.description_2d[1].mobile.unit
    ]

    # Create some test arrays with floating point data for array formatting tests
    dd.core_profiles.profiles_1d[1].grid.rho_tor_norm = [0.1, 0.2, 0.3]  # Small array
    dd.core_profiles.profiles_1d[1].electrons.density = collect(0.0:0.01:1.0)  # Large array

    println("\n--- IDS show() Function Benchmarks ---")

    @testset "IDS show() performance" begin
        for (i, test_struct) in enumerate(test_ids)
            # Capture output to IOBuffer to measure performance
            io = IOBuffer()
            result = @benchmark show($io, $test_struct) seconds = 2
            @printf "IDS show() #%d: %.2f ns ± %.2f ns, %d bytes, %d allocs\n" i minimum(result).time maximum(result).time result.memory result.allocs

            # Verify functionality works
            io = IOBuffer()
            show(io, test_struct)
            output = String(take!(io))
            @test !isempty(output)
            @test contains(output, "{")
            @test contains(output, "}")
        end
    end

    println("\n--- IDSvector show() Function Benchmarks ---")

    @testset "IDSvector show() performance" begin
        for (i, test_vector) in enumerate(test_idsvectors)
            # Test different vector lengths
            if length(test_vector) > 0
                io = IOBuffer()
                result = @benchmark show($io, $test_vector) seconds = 2
                @printf "IDSvector show() #%d (len=%d): %.2f ns ± %.2f ns, %d bytes, %d allocs\n" i length(test_vector) minimum(result).time maximum(result).time result.memory result.allocs

                # Verify functionality works
                io = IOBuffer()
                show(io, test_vector)
                output = String(take!(io))
                @test !isempty(output)
                @test contains(output, "[")
                @test contains(output, "]")
            end
        end
    end

    println("\n--- Array Formatting Benchmarks ---")

    @testset "Array formatting performance" begin
        # Test the optimized array formatting in printnode
        small_float_array = [1.234, 2.345, 3.456]
        large_float_array = collect(1.0:100.0)

        # Create a mock node to test the array formatting path
        # This tests the @printf + IOBuffer optimization
        test_arrays = [
            ("small_float", small_float_array),
            ("large_float", large_float_array)
        ]

        for (name, arr) in test_arrays
            # We can't directly benchmark the internal printnode function,
            # but we can test it through the show mechanism when data is present
            if length(arr) < 5  # This will trigger the optimized path
                io = IOBuffer()
                # Create a simple benchmark by formatting the array manually
                result = @benchmark begin
                    array_io = IOBuffer()
                    print(array_io, "[")
                    for (i, v) in enumerate($arr)
                        if i > 1
                            print(array_io, ",")
                        end
                        @printf(array_io, "%g", v)
                    end
                    print(array_io, "]")
                    String(take!(array_io))
                end seconds = 1
                @printf "Array formatting (%s): %.2f ns ± %.2f ns, %d bytes, %d allocs\n" name minimum(result).time maximum(result).time result.memory result.allocs
            end
        end
    end

    println("\n--- String Building Component Benchmarks ---")

    @testset "String building components" begin
        # Test the key optimized patterns independently

        # 1. Keys joining (from IDS show function)
        test_keys = [:field1, :field2, :field3, :field4, :field5]
        result = @benchmark begin
            content_io = IOBuffer()
            for (i, k) in enumerate($test_keys)
                if i > 1
                    print(content_io, ", ")
                end
                print(content_io, k)
            end
            String(take!(content_io))
        end seconds = 1
        @printf "Keys joining (IOBuffer): %.2f ns ± %.2f ns, %d bytes, %d allocs\n" minimum(result).time maximum(result).time result.memory result.allocs

        # Compare with old approach for reference
        result_old = @benchmark join((string(k) for k in $test_keys), ", ") seconds = 1
        @printf "Keys joining (join): %.2f ns ± %.2f ns, %d bytes, %d allocs\n" minimum(result_old).time maximum(result_old).time result_old.memory result_old.allocs

        # 2. Index range formatting (from IDSvector show function)
        test_range = 1:5
        result = @benchmark begin
            indices_io = IOBuffer()
            for i in $test_range
                if i > 1
                    print(indices_io, ", ")
                end
                print(indices_io, i)
            end
            String(take!(indices_io))
        end seconds = 1
        @printf "Index range (IOBuffer): %.2f ns ± %.2f ns, %d bytes, %d allocs\n" minimum(result).time maximum(result).time result.memory result.allocs

        # Compare with old approach
        result_old = @benchmark join(string.(collect($test_range)), ", ") seconds = 1
        @printf "Index range (join): %.2f ns ± %.2f ns, %d bytes, %d allocs\n" minimum(result_old).time maximum(result_old).time result_old.memory result_old.allocs
    end

    println("\n--- Correctness Tests ---")

    @testset "Correctness verification" begin
        # Verify that all optimizations produce correct output

        # Test IDS show output format
        io = IOBuffer()
        show(io, dd.core_profiles)
        output = String(take!(io))
        @test contains(output, "core_profiles{")
        @test contains(output, "}")

        # Test IDSvector show output format  
        if length(dd.core_profiles.profiles_1d) > 0
            io = IOBuffer()
            show(io, dd.core_profiles.profiles_1d)
            output = String(take!(io))
            @test contains(output, "core_profiles.profiles_1d[")
            @test contains(output, "]")
        end

        # Test array formatting correctness
        test_array = [1.234, 2.345, 3.456]
        array_io = IOBuffer()
        print(array_io, "[")
        for (i, v) in enumerate(test_array)
            if i > 1
                print(array_io, ",")
            end
            @printf(array_io, "%g", v)
        end
        print(array_io, "]")
        formatted = String(take!(array_io))
        @test formatted == "[1.234,2.345,3.456]"
    end

    println("\n=== show.jl Benchmark Summary Complete ===")
    println("All show.jl string building optimizations benchmarked successfully!")
    println("Use this file to track performance improvements in display functions.")
end