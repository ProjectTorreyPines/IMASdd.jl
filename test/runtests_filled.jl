using Test
using IMASdd

"""
Regression tests for _filled field handling functions:
- add_filled(ids::IDS, field::Symbol)
- add_filled(ids::Union{IDS,IDSvector})
- del_filled(ids::IDS, field::Symbol)  
- set_parent_filled(ids::Union{IDS,IDSvector})
"""

@testset "_filled field handling regression tests" begin
    
    @testset "add_filled(ids::IDS, field::Symbol)" begin
        dd = IMASdd.dd{Float64}()
        
        # Test basic field marking as filled
        @test !getfield(dd._filled, :equilibrium)
        IMASdd.add_filled(dd, :equilibrium)
        @test getfield(dd._filled, :equilibrium)
        
        # Test with another field
        @test !getfield(dd._filled, :core_profiles)
        IMASdd.add_filled(dd, :core_profiles)
        @test getfield(dd._filled, :core_profiles)
        
        # Test multiple field additions  
        @test !getfield(dd._filled, :magnetics)
        IMASdd.add_filled(dd, :magnetics)
        @test getfield(dd._filled, :magnetics)
        @test getfield(dd._filled, :core_profiles)  # still filled from before
        @test getfield(dd._filled, :equilibrium)  # still filled from before
    end
    
    @testset "add_filled(ids::Union{IDS,IDSvector}) - parent propagation" begin
        dd = IMASdd.dd{Float64}()
        
        # Test parent propagation for IDS
        eq = dd.equilibrium
        @test !getfield(dd._filled, :equilibrium)
        IMASdd.add_filled(eq)
        @test getfield(dd._filled, :equilibrium)
        
        # Test nested parent propagation
        eq_time = dd.equilibrium.time_slice
        @test !getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        IMASdd.add_filled(eq_time)
        @test getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        @test getfield(dd._filled, :equilibrium)  # should propagate up
        
        # Test IDSvector propagation
        dd2 = IMASdd.dd{Float64}()
        ts = dd2.equilibrium.time_slice
        # resize! operation automatically calls add_filled
        @test !getfield(getfield(dd2.equilibrium, :_filled), :time_slice)
        @test !getfield(dd2._filled, :equilibrium)
        resize!(ts, 2)
        @test getfield(getfield(dd2.equilibrium, :_filled), :time_slice)
        @test getfield(dd2._filled, :equilibrium)
        
        # Test explicit add_filled call on IDSvector  
        dd3 = IMASdd.dd{Float64}()
        ts3 = dd3.equilibrium.time_slice
        @test !getfield(getfield(dd3.equilibrium, :_filled), :time_slice)
        IMASdd.add_filled(ts3)
        @test getfield(getfield(dd3.equilibrium, :_filled), :time_slice)
        @test getfield(dd3._filled, :equilibrium)
    end
    
    @testset "del_filled(ids::IDS, field::Symbol)" begin
        dd = IMASdd.dd{Float64}()
        
        # Setup: mark some fields as filled
        IMASdd.add_filled(dd, :equilibrium)
        IMASdd.add_filled(dd, :core_profiles)
        @test getfield(dd._filled, :equilibrium)
        @test getfield(dd._filled, :core_profiles)
        
        # Test deletion of individual field
        IMASdd.del_filled(dd, :equilibrium)
        @test !getfield(dd._filled, :equilibrium)
        @test getfield(dd._filled, :core_profiles)  # should remain filled
        
        # Test deletion of non-filled field (should not error)
        @test !getfield(dd._filled, :magnetics)
        IMASdd.del_filled(dd, :magnetics)
        @test !getfield(dd._filled, :magnetics)
    end
    
    @testset "set_parent_filled recursive behavior" begin
        dd = IMASdd.dd{Float64}()
        
        # Set up a deeply nested structure that is filled
        resize!(dd.equilibrium.time_slice, 1)
        dd.equilibrium.time_slice[1].time = 1.0
        
        # Verify the whole chain is marked as filled
        @test getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        @test getfield(dd._filled, :equilibrium)
        
        # Create an empty nested IDS to test recursive unfilling
        eq_grid = dd.equilibrium.grids_ggd
        resize!(eq_grid, 1)
        # Grid should propagate filled status up
        @test getfield(getfield(dd.equilibrium, :_filled), :grids_ggd)
        @test getfield(dd._filled, :equilibrium)
        
        # Now empty the grid and test recursive unfilling
        resize!(eq_grid, 0)
        @test !getfield(getfield(dd.equilibrium, :_filled), :grids_ggd)
        
        # The equilibrium should still be filled due to time_slice
        @test getfield(dd._filled, :equilibrium)
        
        # Now empty time_slice too
        resize!(dd.equilibrium.time_slice, 0)
        @test !getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        
        # Now equilibrium should be unfilled at top level
        @test !getfield(dd._filled, :equilibrium)
    end
    
    @testset "set_parent_filled IDS vs IDSvector behavior" begin 
        dd = IMASdd.dd{Float64}()
        
        # Test IDS recursion: create nested IDS and empty it
        prof = dd.core_profiles.profiles_1d
        resize!(prof, 1)
        grid = prof[1].grid
        
        # Fill something in grid to mark the whole chain
        grid.rho_tor_norm = [0.0, 1.0]
        @test getfield(getfield(dd.core_profiles, :_filled), :profiles_1d)
        @test getfield(dd._filled, :core_profiles)
        
        # Now empty the grid IDS and verify local unfilling
        empty!(grid)
        @test isempty(grid)
        
        # Grid field is unfilled, but prof[1] still exists so profiles_1d stays filled
        @test !getfield(getfield(prof[1], :_filled), :grid)
        # profiles_1d still filled because prof[1] exists (even if empty)
        @test getfield(getfield(dd.core_profiles, :_filled), :profiles_1d)
        @test getfield(dd._filled, :core_profiles)
        
        # Resize prof to 0 to empty the IDSvector - this unfills parents
        resize!(prof, 0)
        @test isempty(prof)
        @test !getfield(getfield(dd.core_profiles, :_filled), :profiles_1d)
        @test !getfield(dd._filled, :core_profiles)
        
        # Test IDSvector recursion: verify behavior when IDSvector becomes empty
        ts = dd.equilibrium.time_slice
        resize!(ts, 2)
        ts[1].time = 1.0
        @test getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        @test getfield(dd._filled, :equilibrium)
        
        # Empty the IDSvector completely
        resize!(ts, 0)
        @test isempty(ts)
        
        # Should recursively unset parents
        @test !getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        @test !getfield(dd._filled, :equilibrium)
    end
    
    @testset "Integration test: complete workflow" begin
        dd = IMASdd.dd{Float64}()
        
        # Start with empty data structure
        @test !getfield(dd._filled, :equilibrium)
        @test !getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        
        # Add data and verify filled propagation
        ts = dd.equilibrium.time_slice
        resize!(ts, 3)
        
        # Access a field to trigger add_filled internally
        ts[1].time = 1.0
        
        # Verify propagation happened
        @test getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        @test getfield(dd._filled, :equilibrium)
        
        # Manually clear filled fields for testing
        IMASdd.del_filled(dd.equilibrium, :time_slice)
        @test !getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        
        # Manually clear parent
        IMASdd.del_filled(dd, :equilibrium)
        @test !getfield(dd._filled, :equilibrium)
    end
    
    @testset "Recursion depth verification" begin
        dd = IMASdd.dd{Float64}()
        
        # Create a 4-level deep structure: dd -> core_profiles -> profiles_1d[1] -> grid -> (data)
        prof = dd.core_profiles.profiles_1d
        resize!(prof, 1)
        grid = prof[1].grid
        
        # Fill the deepest level
        grid.rho_tor_norm = [0.0, 0.5, 1.0]
        
        # Verify all levels marked filled (upward propagation)
        @test getfield(getfield(grid, :_filled), :rho_tor_norm)
        @test getfield(getfield(prof[1], :_filled), :grid)
        @test getfield(getfield(dd.core_profiles, :_filled), :profiles_1d)
        @test getfield(dd._filled, :core_profiles)
        
        # Now empty the grid and test that recursion unfills all the way to top
        empty!(grid)
        @test isempty(grid)
        
        # Grid unfilled locally, but parents remain filled because prof[1] exists
        @test !getfield(getfield(prof[1], :_filled), :grid)
        @test getfield(getfield(dd.core_profiles, :_filled), :profiles_1d)  # Still filled
        @test getfield(dd._filled, :core_profiles)  # Still filled
        
        # Test that partial filling doesn't trigger unfilling
        grid.rho_tor_norm = [0.0, 1.0]  # Fill again
        @test getfield(dd._filled, :core_profiles)
        
        # Add a second profile element with data
        resize!(prof, 2)
        prof[2].grid.rho_tor_norm = [0.0, 0.2, 0.4, 1.0]
        @test getfield(dd._filled, :core_profiles)
        
        # Empty first grid - parent stays filled since prof[2] has data
        empty!(prof[1].grid)
        @test getfield(getfield(dd.core_profiles, :_filled), :profiles_1d)
        @test getfield(dd._filled, :core_profiles)
        
        # Empty second grid - parents still filled because prof elements exist
        empty!(prof[2].grid)
        @test getfield(getfield(dd.core_profiles, :_filled), :profiles_1d)
        @test getfield(dd._filled, :core_profiles)
        
        # Removing all elements unfills parents
        resize!(prof, 0)
        @test !getfield(getfield(dd.core_profiles, :_filled), :profiles_1d)
        @test !getfield(dd._filled, :core_profiles)
    end
    
    @testset "Cross-structure recursion patterns" begin
        dd = IMASdd.dd{Float64}()
        
        # Test complex hierarchy: dd -> equilibrium (IDS) -> time_slice (IDSvector) -> time_slice[1] (IDS) -> profiles_2d (IDSvector)
        ts = dd.equilibrium.time_slice
        resize!(ts, 1)
        ts[1].time = 1.0
        
        # Add some 2D profiles data  
        profiles_2d = ts[1].profiles_2d
        resize!(profiles_2d, 1)
        profiles_2d[1].grid.dim1 = [0.0, 1.0]
        
        # Verify the whole chain is filled
        @test getfield(getfield(ts[1], :_filled), :profiles_2d)
        @test getfield(getfield(dd.equilibrium, :_filled), :time_slice)  
        @test getfield(dd._filled, :equilibrium)
        
        # Empty the deepest IDSvector to trigger recursion upward
        resize!(profiles_2d, 0)
        @test isempty(profiles_2d)
        
        # profiles_2d field unfilled in time_slice[1]
        @test !getfield(getfield(ts[1], :_filled), :profiles_2d)
        
        # time_slice[1] still exists with other data, so time_slice stays filled
        @test getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        @test getfield(dd._filled, :equilibrium)
        
        # Empty time_slice[1] completely
        empty!(ts[1])
        @test isempty(ts[1])
        
        # time_slice still filled because ts[1] element exists (even if empty)
        @test getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        @test getfield(dd._filled, :equilibrium)
        
        # Resize time_slice to 0 to trigger full recursion
        resize!(ts, 0)
        @test isempty(ts)
        
        # Recursion unfills parent chain
        @test !getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        @test !getfield(dd._filled, :equilibrium)
    end
    
    @testset "Recursion continuation verification" begin
        # Verify recursion continues when parent type varies
        dd = IMASdd.dd{Float64}()
        
        # Create nested structure with varying parent types
        prof = dd.core_profiles.profiles_1d
        resize!(prof, 1)
        prof[1].grid.rho_tor_norm = [0.0, 1.0]
        
        # Verify filled status 
        @test getfield(getfield(dd.core_profiles, :_filled), :profiles_1d)
        @test getfield(dd._filled, :core_profiles)
        
        # Key test: recursion through different types
        empty!(prof[1])
        @test isempty(prof[1])
        
        # prof[1] locally unfilled but prof (IDSvector) still filled
        @test getfield(getfield(dd.core_profiles, :_filled), :profiles_1d)
        @test getfield(dd._filled, :core_profiles)
        
        # Resize prof to 0 - tests IDSvector -> IDS recursion
        resize!(prof, 0)
        @test isempty(prof)
        
        # Triggers recursion through type hierarchy
        @test !getfield(getfield(dd.core_profiles, :_filled), :profiles_1d)
        @test !getfield(dd._filled, :core_profiles)
    end
    
    @testset "Edge cases and error conditions" begin
        dd = IMASdd.dd{Float64}()
        
        # Test multiple calls to same function (idempotency)
        IMASdd.add_filled(dd, :core_profiles)
        IMASdd.add_filled(dd, :core_profiles)
        @test getfield(dd._filled, :core_profiles)
        
        IMASdd.del_filled(dd, :core_profiles)
        IMASdd.del_filled(dd, :core_profiles)
        @test !getfield(dd._filled, :core_profiles)
        
        # Test calling set_parent_filled on already empty structure
        ts = dd.equilibrium.time_slice
        @test isempty(ts)
        @test !getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        
        # Should not error or change anything
        IMASdd.set_parent_filled(ts)
        @test !getfield(getfield(dd.equilibrium, :_filled), :time_slice)
        @test !getfield(dd._filled, :equilibrium)
    end
    
    @testset "Performance test: no unnecessary allocations" begin
        dd = IMASdd.dd{Float64}()
        
        # Operations should have minimal allocations
        allocs1 = @allocated IMASdd.add_filled(dd, :equilibrium)
        @test allocs1 < 100
        
        allocs2 = @allocated IMASdd.del_filled(dd, :equilibrium)
        @test allocs2 < 100
        
        allocs3 = @allocated IMASdd.add_filled(dd.equilibrium)
        @test allocs3 < 100
        
        # Test with non-empty IDSvector
        ts = dd.equilibrium.time_slice
        resize!(ts, 1)
        allocs4 = @allocated IMASdd.set_parent_filled(ts)
        @test allocs4 < 100
    end
end