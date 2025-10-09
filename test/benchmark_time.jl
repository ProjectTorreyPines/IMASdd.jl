using BenchmarkTools
using IMASdd
import IMASdd: nearest_causal_time, get_time_array, set_time_array, time_array_from_parent_ids

"""
Comprehensive benchmark suite for time.jl functions to establish baseline performance
and track improvements after optimization.
"""

# Helper function to create test data
function create_test_dd()
    dd = IMASdd.dd()
    dd.global_time = 0.0

    # Create time-dependent structures
    for i in 1:5
        dd.global_time = Float64(i)
        resize!(dd.equilibrium.time_slice)
        dd.equilibrium.time_slice[].global_quantities.ip = Float64(i)
    end

    # Create time-dependent arrays
    dd.equilibrium.time = collect(1.0:5.0)
    dd.equilibrium.vacuum_toroidal_field.b0 = collect(1.0:5.0)

    # Create multi-dimensional time-dependent arrays
    ecb = resize!(dd.ec_launchers.beam, 1)[1]
    ecb.time = collect(1.0:10.0)

    # 2D array (2 x 10 time points)
    setproperty!(ecb.spot, :size, reshape(collect(1.0:20.0), (2, 10)); error_on_missing_coordinates=false)

    # 1D array for power launched (10 time points) 
    setproperty!(ecb.power_launched, :data, collect(1.0:10.0); error_on_missing_coordinates=false)

    return dd
end

function create_large_time_array(n_times::Int)
    return collect(range(0.0, 100.0; length=n_times))
end

function create_large_multidim_array(dims::Tuple, n_times::Int)
    total_size = prod(dims) * n_times
    return reshape(collect(1.0:total_size), (dims..., n_times))
end

println("=== IMASdd Time Functions Benchmark Suite ===")
println("Julia version: $(VERSION)")
println("Threads: $(Threads.nthreads())")
println()

# Create test data
println("Creating test data...")
dd = create_test_dd()
large_time = create_large_time_array(1000)
large_2d_array = create_large_multidim_array((50,), 1000)
large_3d_array = create_large_multidim_array((10, 20), 1000)
large_4d_array = create_large_multidim_array((5, 8, 12), 1000)

println("Test data created successfully!")
println()

# =============================================================================
# Benchmark 1: nearest_causal_time functions
# =============================================================================
println("=== Benchmark 1: nearest_causal_time ===")

# Test with regular time vector
time_vec = collect(1.0:100.0)
test_times = [1.0, 50.5, 99.9, 100.0]

println("Vector input (100 elements):")
for test_time in test_times
    result = @benchmark nearest_causal_time($time_vec, $test_time)
    println("  time=$test_time: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")
end

# Test with IDSvector
println("\nIDSvector input (5 elements):")
for test_time in [1.0, 2.5, 5.0]
    result = @benchmark nearest_causal_time($(dd.equilibrium.time_slice), $test_time)
    println("  time=$test_time: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")
end

# Test edge cases (last element optimization)
println("\nLast element access optimization:")
result_last = @benchmark nearest_causal_time($time_vec, $(time_vec[end]))
result_middle = @benchmark nearest_causal_time($time_vec, $(time_vec[50]))
println("  Last element: $(BenchmarkTools.prettytime(median(result_last.times))) ($(result_last.allocs) allocs)")
println("  Middle element: $(BenchmarkTools.prettytime(median(result_middle.times))) ($(result_middle.allocs) allocs)")

# =============================================================================
# Benchmark 2: get_time_array functions
# =============================================================================
println("\n=== Benchmark 2: get_time_array ===")

# 1D array access
println("1D array access:")
result = @benchmark get_time_array($(dd.equilibrium.vacuum_toroidal_field), :b0, 3.5, :constant)
println("  Single time: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

result = @benchmark get_time_array($(dd.equilibrium.vacuum_toroidal_field), :b0, $([1.5, 2.5, 3.5, 4.5]), :constant)
println("  Multiple times: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

# Large 1D array
ecb = dd.ec_launchers.beam[1]
println("\nLarge 1D array (1000 time points):")
ecb.power_launched.time = large_time
ecb.power_launched.data = large_2d_array[1, :]  # 1D slice

test_times_large = [10.0, 25.0, 50.0, 75.0, 90.0]
result = @benchmark get_time_array($(ecb.power_launched), :data, $test_times_large, :constant)
println("  Multiple times: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

# Multi-dimensional arrays
println("\n2D array access (2 x 10):")
result = @benchmark get_time_array($(ecb.spot), :size, 5.5, :constant)
println("  Single time: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

result = @benchmark get_time_array($(ecb.spot), :size, $([1.5, 3.5, 7.5]), :constant)
println("  Multiple times: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

# Large multi-dimensional arrays
println("\nLarge 2D array (50 x 1000):")
# Create a temporary test structure for large arrays
dd_large = IMASdd.dd()
ecb_large = resize!(dd_large.ec_launchers.beam, 1)[1]
ecb_large.time = large_time
setproperty!(ecb_large.spot, :size, large_2d_array; error_on_missing_coordinates=false)

result = @benchmark get_time_array($(ecb_large.spot), :size, $test_times_large, :constant)
println("  Multiple times: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

println("\nLarge 1D power launched array (1000 time points):")
setproperty!(ecb_large.power_launched, :data, large_2d_array[1, :]; error_on_missing_coordinates=false)
result = @benchmark get_time_array($(ecb_large.power_launched), :data, $test_times_large, :constant)
println("  Multiple times: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

# =============================================================================
# Benchmark 3: set_time_array functions  
# =============================================================================
println("\n=== Benchmark 3: set_time_array ===")

# Scalar values
println("Scalar value setting:")
dd_test = IMASdd.dd()
dd_test.global_time = 1.0
result = @benchmark begin
    dd_copy = deepcopy($dd_test)
    set_time_array(dd_copy.equilibrium.vacuum_toroidal_field, :b0, 1.0, 5.0)
end
println("  New field: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

# Existing field update
dd_test.equilibrium.time = [1.0, 2.0, 3.0]
dd_test.equilibrium.vacuum_toroidal_field.b0 = [1.0, 2.0, 3.0]
result = @benchmark begin
    dd_copy = deepcopy($dd_test)
    set_time_array(dd_copy.equilibrium.vacuum_toroidal_field, :b0, 2.0, 10.0)
end
println("  Existing field: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

# Array values
println("\nArray value setting:")
test_array_1d = collect(1.0:10.0)
result = @benchmark begin
    dd_copy = deepcopy($dd_test)
    ecb_copy = resize!(dd_copy.ec_launchers.beam, 1)[1]
    set_time_array(ecb_copy.spot, :size, 1.0, $test_array_1d)
end
println("  1D array: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

# =============================================================================
# Benchmark 4: time_array_from_parent_ids
# =============================================================================
println("\n=== Benchmark 4: time_array_from_parent_ids ===")

result = @benchmark time_array_from_parent_ids($(dd.equilibrium.vacuum_toroidal_field), Val(:get))
println("  Deep hierarchy: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

result = @benchmark time_array_from_parent_ids($(dd.equilibrium), Val(:get))
println("  Shallow hierarchy: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

# =============================================================================
# Benchmark 5: High-level operations
# =============================================================================
println("\n=== Benchmark 5: High-level operations ===")

# @ddtime macro
println("@ddtime macro:")
result = @benchmark @ddtime($(dd.equilibrium.vacuum_toroidal_field).b0)
println("  Get: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

result = @benchmark begin
    dd_copy = deepcopy($dd)
    @ddtime(dd_copy.equilibrium.vacuum_toroidal_field.b0 = 10.0)
end
println("  Set: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

# get_timeslice
println("\nget_timeslice:")
result = @benchmark IMASdd.get_timeslice($(dd.equilibrium), 3.5, :constant)
println("  Constant interp: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

result = @benchmark IMASdd.get_timeslice($(dd.equilibrium), 3.5, :linear)
println("  Linear interp: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

# resize! operations
println("\nresize! operations:")
result = @benchmark begin
    dd_copy = deepcopy($dd)
    dd_copy.global_time = 10.0
    resize!(dd_copy.equilibrium.time_slice)
end
println("  New timeslice: $(BenchmarkTools.prettytime(median(result.times))) ($(result.allocs) allocs)")

println("\n=== Benchmark Complete ===")
println("Baseline performance established!")
println("Run this benchmark again after optimizations to measure improvements.")