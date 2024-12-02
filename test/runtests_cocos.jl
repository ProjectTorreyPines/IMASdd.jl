using IMASdd
import IMASdd as IMAS
using Test

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "cocos" begin

    eqt1d = IMASdd.equilibrium__time_slice___profiles_1d()

    # user COCOS same as internal_cocos
    IMASdd._setproperty!(eqt1d, :psi, [0.1]; from_cocos=11)
    IMASdd._setproperty!(eqt1d, :rho_tor_norm, [1.0]; from_cocos=11)
    @test IMASdd.getfield(eqt1d, :psi) == [0.1]
    @test IMASdd._getproperty(eqt1d, :psi; to_cocos=11) == [0.1]
    @test IMASdd._getproperty(eqt1d, :psi; to_cocos=17) == [-0.1]
    @test IMASdd.getfield(eqt1d, :rho_tor_norm) == [1.0]
    @test IMASdd._getproperty(eqt1d, :rho_tor_norm; to_cocos=11) == [1.0]
    @test IMASdd._getproperty(eqt1d, :rho_tor_norm; to_cocos=17) == [1.0]

    # user COCOS different from internal_cocos
    IMASdd._setproperty!(eqt1d, :psi, [-0.1]; from_cocos=17)
    IMASdd._setproperty!(eqt1d, :rho_tor_norm, [1.0]; from_cocos=17)
    @test IMASdd.getfield(eqt1d, :psi) == [0.1]
    @test IMASdd._getproperty(eqt1d, :psi; to_cocos=11) == [0.1]
    @test IMASdd._getproperty(eqt1d, :psi; to_cocos=17) == [-0.1]
    @test IMASdd.getfield(eqt1d, :rho_tor_norm) == [1.0]
    @test IMASdd._getproperty(eqt1d, :rho_tor_norm; to_cocos=11) == [1.0]
    @test IMASdd._getproperty(eqt1d, :rho_tor_norm; to_cocos=17) == [1.0]

    # the same with macros
    @cocos11(eqt1d.psi = [0.2])
    @test IMASdd.getfield(eqt1d, :psi) == [0.2]
    @test @cocos11(eqt1d.psi) == [0.2]
    @test @cocos17(eqt1d.psi) == [-0.2]
    @cocos17(eqt1d.psi = [-0.2])
    @test IMASdd.getfield(eqt1d, :psi) == [0.2]
    @test @cocos11(eqt1d.psi) == [0.2]
    @test @cocos17(eqt1d.psi) == [-0.2]

end
