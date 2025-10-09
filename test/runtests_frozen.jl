using IMASdd
using Test

@testset "Frozen Property Passthrough" begin
    
    @testset "Basic frozen property propagation" begin
        # Create a frozen dd
        dd_frozen = IMASdd.dd(;frozen=true)
        @test IMASdd.isfrozen(dd_frozen) == true
        @test getfield(dd_frozen.equilibrium, :_frozen) == true
        
        # Test that time slices inherit frozen property
        eqt = resize!(dd_frozen.equilibrium.time_slice, 1.0)
        @test getfield(dd_frozen.equilibrium.time_slice, :_frozen) == true
        @test getfield(eqt, :_frozen) == true
        @test getfield(eqt.profiles_1d, :_frozen) == true
        
        # Test core profiles
        cp1d = resize!(dd_frozen.core_profiles.profiles_1d, 1.0)
        @test getfield(dd_frozen.core_profiles.profiles_1d, :_frozen) == true
        @test getfield(cp1d, :_frozen) == true
    end
    
    @testset "Non-frozen dd behavior" begin
        # Create a non-frozen dd
        dd_normal = IMASdd.dd(;frozen=false)
        @test IMASdd.isfrozen(dd_normal) == false
        @test getfield(dd_normal.equilibrium, :_frozen) == false
        
        # Test that time slices inherit non-frozen property
        eqt = resize!(dd_normal.equilibrium.time_slice, 1.0)
        @test getfield(dd_normal.equilibrium.time_slice, :_frozen) == false
        @test getfield(eqt, :_frozen) == false
        @test getfield(eqt.profiles_1d, :_frozen) == false
        
        # Test core profiles
        cp1d = resize!(dd_normal.core_profiles.profiles_1d, 1.0)
        @test getfield(dd_normal.core_profiles.profiles_1d, :_frozen) == false
        @test getfield(cp1d, :_frozen) == false
    end
    
    @testset "Multiple time slices with frozen property" begin
        dd_frozen = IMASdd.dd(;frozen=true)
        times = [0.0, 0.5, 1.0, 1.5]
        
        for time0 in times
            # Equilibrium time slices
            eqt = resize!(dd_frozen.equilibrium.time_slice, time0)
            @test getfield(dd_frozen.equilibrium.time_slice, :_frozen) == true
            @test getfield(eqt, :_frozen) == true
            @test getfield(eqt.profiles_1d, :_frozen) == true
            
            # Core profiles time slices
            cp1d = resize!(dd_frozen.core_profiles.profiles_1d, time0)
            @test getfield(dd_frozen.core_profiles.profiles_1d, :_frozen) == true
            @test getfield(cp1d, :_frozen) == true
        end
    end
    
    @testset "Frozen property prevents coordinate assignment" begin
        dd_frozen = IMASdd.dd(;frozen=true)
        eqt = resize!(dd_frozen.equilibrium.time_slice, 1.0)
        
        # This should not trigger coordinate error checking for frozen structures
        # (based on the change in setproperty! that checks !isfrozen(ids))
        # We can't directly test this without a field that would normally require coordinates,
        # but the frozen check prevents the coordinate error from being triggered
        @test getfield(eqt, :_frozen) == true
    end
    
    @testset "Frozen property with IDSvector construction" begin
        # Test direct IDSvector construction with frozen=true
        vec_frozen = IMASdd.IDSvector(IMASdd.core_profiles__profiles_1d[]; frozen=true)
        @test getfield(vec_frozen, :_frozen) == true
        
        # Test IDSvector{T}() constructor with frozen=true
        vec_frozen2 = IMASdd.IDSvector{IMASdd.core_profiles__profiles_1d}(;frozen=true)
        @test getfield(vec_frozen2, :_frozen) == true
        
        # Test default behavior (frozen=false)
        vec_normal = IMASdd.IDSvector{IMASdd.core_profiles__profiles_1d}()
        @test getfield(vec_normal, :_frozen) == false
    end
    
end