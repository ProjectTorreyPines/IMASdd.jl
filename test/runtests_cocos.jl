using IMASdd
import IMASdd as IMAS
using Test

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "cocos" begin

    eqt1d = IMASdd.equilibrium__time_slice___profiles_1d()
    eqt1d.psi = [0.1]
    eqt1d.rho_tor_norm = [1.0]
    
    @test IMASdd._getproperty(eqt1d, :psi; to_cocos=11) == [0.1]
    @test IMASdd._getproperty(eqt1d, :psi; to_cocos=17) == [-0.1]
    @test IMASdd._getproperty(eqt1d, :rho_tor_norm; to_cocos=11) == [1.0]
    @test IMASdd._getproperty(eqt1d, :rho_tor_norm; to_cocos=17) == [1.0]
end
