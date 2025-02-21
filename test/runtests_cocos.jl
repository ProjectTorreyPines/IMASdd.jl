using IMASdd
import IMASdd
using Test

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "cocos" begin

    eqt1d = IMASdd.equilibrium__time_slice___profiles_1d()

    # user COCOS same as internal_cocos
    setproperty!(eqt1d, :psi, [0.1]; from_cocos=11)
    setproperty!(eqt1d, :rho_tor_norm, [1.0]; from_cocos=11)
    @test getfield(eqt1d, :psi) == [0.1]
    @test getproperty(eqt1d, :psi; to_cocos=11) == [0.1]
    @test getproperty(eqt1d, :psi; to_cocos=17) == [-0.1]
    @test getfield(eqt1d, :rho_tor_norm) == [1.0]
    @test getproperty(eqt1d, :rho_tor_norm; to_cocos=11) == [1.0]
    @test getproperty(eqt1d, :rho_tor_norm; to_cocos=17) == [1.0]

    # user COCOS different from internal_cocos
    setproperty!(eqt1d, :psi, [-0.1]; from_cocos=17)
    setproperty!(eqt1d, :rho_tor_norm, [1.0]; from_cocos=17)
    @test getfield(eqt1d, :psi) == [0.1]
    @test getproperty(eqt1d, :psi; to_cocos=11) == [0.1]
    @test getproperty(eqt1d, :psi; to_cocos=17) == [-0.1]
    @test getfield(eqt1d, :rho_tor_norm) == [1.0]
    @test getproperty(eqt1d, :rho_tor_norm; to_cocos=11) == [1.0]
    @test getproperty(eqt1d, :rho_tor_norm; to_cocos=17) == [1.0]

    # the same with macros
    @cocos11(eqt1d.psi = [0.2])
    @test getfield(eqt1d, :psi) == [0.2]
    @test @cocos11(eqt1d.psi) == [0.2]
    @test @cocos17(eqt1d.psi) == [-0.2]
    @cocos17(eqt1d.psi = [-0.2])
    @test getfield(eqt1d, :psi) == [0.2]
    @test @cocos11(eqt1d.psi) == [0.2]
    @test @cocos17(eqt1d.psi) == [-0.2]

end
