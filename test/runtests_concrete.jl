using IMASdd
import IMASdd
import IMASdd: @ddtime
using Test

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "concrete_types" begin

    dd = IMASdd.dd()
    cp1d = resize!(dd.core_profiles.profiles_1d)
    cp1d.grid.rho_tor_norm = [0.1]
    cp1d.electrons.temperature = [1.0]

    function g1(cp1d)
        return getproperty(cp1d.electrons, :temperature)
    end
    rets = Base.return_types(g1, Tuple{typeof(cp1d)})
    @test rets[1] == Vector{Float64}
    @test !any(t -> t === Any || t === Union{} || !isconcretetype(t), rets)

    function g2(cp1d)
        return getproperty(cp1d.electrons, :temperature, [2.0])
    end
    rets = Base.return_types(g2, Tuple{typeof(cp1d)})
    @test rets[1] == Vector{Float64}
    @test !any(t -> t === Any || t === Union{} || !isconcretetype(t), rets)

    function g3(cp1d)
        return getproperty(cp1d.electrons, :temperature, missing)
    end
    rets = Base.return_types(g3, Tuple{typeof(cp1d)})
    @test rets == Any[Union{Missing, Vector{Float64}}]
end
