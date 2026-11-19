using IMASdd
using Test

# Flat dicts as imas_composer returns them: one leading dimension per array of structures,
# Float32 numpy-like arrays, nested vectors where awkward_from_buffers returns ragged data
psi_2d = reshape(Float32.(1:2*1*2*3), 2, 1, 2, 3) # time_slice × profiles_2d × dim1 × dim2, non-square to catch transposes

const COMPOSER_FLAT = Dict{String,Any}(
    "equilibrium.ids_properties.homogeneous_time" => 1,
    "equilibrium.code.name" => "EFIT01",
    "equilibrium.time" => Float32[1, 2],
    "equilibrium.time_slice.time" => Float32[1, 2],
    "equilibrium.time_slice.profiles_1d.psi" => Float32[1 2 3; 4 5 6],
    "equilibrium.time_slice.profiles_2d.grid.dim1" => reshape(Float32[1, 2, 1, 2], 2, 1, 2),
    "equilibrium.time_slice.profiles_2d.grid.dim2" => reshape(Float32[1, 2, 3, 1, 2, 3], 2, 1, 3),
    "equilibrium.time_slice.profiles_2d.psi" => psi_2d,
    "equilibrium.time_slice.boundary.outline.r" => [[1.0, 2.0, 3.0], [4.0]],
    "equilibrium.time_slice.boundary.outline.z" => [[0.1, 0.2, 0.3], [0.4]],
    "equilibrium.time_slice.boundary.strike_point.r" => [Float32[1.5, NaN], Float32[1.6, 1.7]],
    "equilibrium._bcentr" => Float32[2, 2],
    "equilibrium.not_a_field" => Float32[0, 0],
    "magnetics.ids_properties.homogeneous_time" => 0,
    "magnetics.ip.time" => [0.5 1.5 2.5],
    "magnetics.ip.data" => [1.0 2.0 3.0],
    "magnetics.ip.data_error_upper" => [0.1 0.2 0.3],
    "magnetics.flux_loop.name" => [nothing, nothing],
    "pf_active.coil.element.identifier" => [["ECOILA", "ECOILA_1"], ["F1A"]],
    "wall.description_2d.limiter.unit.outline.r" => [[[1.0, 2.0], [3.0]]],
)

@testset "imas_composer_dict2imas" begin
    dd = @test_logs (:warn, r"equilibrium.not_a_field") match_mode = :any IMASdd.imas_composer_dict2imas(COMPOSER_FLAT, IMASdd.dd())
    eq = dd.equilibrium

    @testset "scalars and 1-D" begin
        @test eq.code.name == "EFIT01"
        @test eq.time == [1.0, 2.0]
        @test eltype(eq.time) == Float64
        @test [slice.time for slice in eq.time_slice] == [1.0, 2.0]
    end

    @testset "arrays of structures" begin
        @test length(eq.time_slice) == 2
        @test eq.time_slice[2].profiles_1d.psi == [4.0, 5.0, 6.0]
        @test length(eq.time_slice[1].profiles_2d) == 1
    end

    @testset "2-D leaf keeps numpy indexing" begin
        for i in 1:2
            psi = eq.time_slice[i].profiles_2d[1].psi
            @test size(psi) == (2, 3)
            @test psi == psi_2d[i, 1, :, :]
        end
    end

    @testset "ragged" begin
        @test eq.time_slice[1].boundary.outline.r == [1.0, 2.0, 3.0]
        @test eq.time_slice[2].boundary.outline.r == [4.0]
        @test isequal(eq.time_slice[1].boundary.strike_point[2].r, NaN)
        @test dd.wall.description_2d[1].limiter.unit[2].outline.r == [3.0]
        @test [element.identifier for element in dd.pf_active.coil[1].element] == ["ECOILA", "ECOILA_1"]
        @test dd.pf_active.coil[2].element[1].identifier == "F1A"
    end

    @testset "error_upper and skipped values" begin
        @test dd.magnetics.ip[1].data_σ == [0.1, 0.2, 0.3]
        @test length(dd.magnetics.flux_loop) == 2
        @test ismissing(dd.magnetics.flux_loop[1], :name)
    end

    @testset "global_time" begin
        # last of the IDS-level time vectors, magnetics.ip.time is not one
        @test dd.global_time == 2.0
    end

    @testset "fills an existing dd" begin
        dd2 = IMASdd.dd()
        dd2.summary.time = [7.0]
        IMASdd.imas_composer_dict2imas(Dict{String,Any}("magnetics.ip.time" => [0.5 1.5], "magnetics.ip.data" => [1.0 2.0]), dd2)
        @test dd2.summary.time == [7.0]
        @test dd2.magnetics.ip[1].data == [1.0, 2.0]
    end
end
