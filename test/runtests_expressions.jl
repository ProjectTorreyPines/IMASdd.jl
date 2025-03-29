using IMASdd
import IMASdd
import IMASdd: @ddtime
using Test

include(joinpath(@__DIR__,"test_expressions_dicts.jl"))

function integrate(x, y)
    h = x[2] - x[1]
    n = length(y) - 1
    n % 2 == 0 || error("`y` length (number of intervals) must be odd")
    s = sum(y[1:2:n] + 4 * y[2:2:n] + y[3:2:n+1])
    return h / 3 * s
end

otexp["equilibrium.time_slice[:].global_quantities.energy_mhd"] =
    (; time_slice, _...) -> 3 / 2 * integrate(time_slice.profiles_1d.volume, time_slice.profiles_1d.pressure)
otexp["core_profiles.profiles_1d[:].grid.volume"] =
    (rho_tor_norm; dd, profiles_1d, _...) -> begin
        eqt = dd.equilibrium.time_slice[Float64(profiles_1d.time)]
        return eqt.profiles_1d.volume
    end
otexp["equilibrium.time_slice[:].time"] =
    (; equilibrium, time_slice_index, _...) -> equilibrium.time[time_slice_index]

@testset "expressions" begin
    @test isempty(IMASdd.get_expressions(Val{:bla}))

    ne0 = 1E20
    Te0 = 1E3
    echarge = 1.602176634e-19
    pe0 = ne0 * Te0 * echarge

    # here we test expressions starting from different heights in the data dictionary
    # also, we are mixing data, user-defined functions, and expressions and using
    # expressions that return vectors and scalars

    # structures linked all the way up to IMASdd.dd
    dd = IMASdd.dd()
    resize!(dd.core_profiles.profiles_1d)
    dd.core_profiles.time = [dd.global_time] # this is not necessary in IMASdd.jl where dd.core_profiles.time is an expressions
    profiles_1d = dd.core_profiles.profiles_1d[1]
    profiles_1d.grid.rho_tor_norm = range(0.0, 1.0, 21)
    profiles_1d.electrons.density_thermal = ne0 .* (1.0 .- profiles_1d.grid.rho_tor_norm .^ 2)
    dyexp["core_profiles.profiles_1d[:].electrons.temperature"] = (x; _...) -> Te0 .* (1.0 .- x .^ 2)
    @test profiles_1d.electrons.temperature[1] ≈ Te0

    # test passing of whole structure
    dyexp["core_profiles.profiles_1d[:].electrons.pressure"] = (x; dd, electrons, profiles_1d, profiles_1d_index, core_profiles) -> pe0 .* (1.0 .- x .^ 2)
    @test profiles_1d.electrons.pressure[1] ≈ pe0

    # test using of macros in expressions
    dyexp["core_profiles.profiles_1d[:].electrons.pressure"] = (x; dd, _...) -> x .* 0.0 .+ @ddtime(dd.core_profiles.time)
    @test profiles_1d.electrons.pressure[1] == 0.0

    # structures linked to top level IDS
    core_profiles = IMASdd.core_profiles()
    resize!(core_profiles.profiles_1d, 1)
    profiles_1d = core_profiles.profiles_1d[1]
    profiles_1d.grid.rho_tor_norm = range(0.0, 1.0, 21)
    profiles_1d.electrons.density = ne0 .* (1.0 .- profiles_1d.grid.rho_tor_norm .^ 2)
    dyexp["core_profiles.profiles_1d[:].electrons.pressure"] = (x; _...) -> pe0 .* (1.0 .- x .^ 2)
    dyexp["core_profiles.profiles_1d[:].electrons.temperature"] = (rho_tor_norm; electrons, _...) -> electrons.pressure ./ (electrons.density * echarge)
    @test profiles_1d.electrons.temperature[1] ≈ Te0

    # test passing of whole structure
    dyexp["core_profiles.profiles_1d[:].electrons.pressure"] = (x; dd, electrons, profiles_1d, profiles_1d_index, core_profiles) -> pe0 .* (1.0 .- x .^ 2)
    @test profiles_1d.electrons.pressure[1] ≈ pe0

    # structures linked after array of structures
    profiles_1d = IMASdd.core_profiles__profiles_1d()
    profiles_1d.grid.rho_tor_norm = range(0.0, 1.0, 21)
    dyexp["core_profiles.profiles_1d[:].electrons.temperature"] = (x; _...) -> Te0 .* (1.0 .- x .^ 2)
    dyexp["core_profiles.profiles_1d[:].electrons.pressure"] = (x; _...) -> pe0 .* (1.0 .- x .^ 2)
    dyexp["core_profiles.profiles_1d[:].electrons.density"] = (rho_tor_norm; electrons, _...) -> electrons.pressure ./ (electrons.temperature * echarge)
    @test profiles_1d.electrons.density[1] ≈ ne0

    # test passing of whole structure
    dyexp["core_profiles.profiles_1d[:].electrons.pressure"] = (x; dd, electrons, profiles_1d, profiles_1d_index, core_profiles) -> pe0 .* (1.0 .- x .^ 2)
    @test profiles_1d.electrons.pressure[1] ≈ pe0

    # test infinite recursion
    profiles_1d = IMASdd.core_profiles__profiles_1d()
    profiles_1d.grid.rho_tor_norm = range(0.0, 1.0, 21)
    dyexp["core_profiles.profiles_1d[:].electrons.pressure"] = (rho_tor_norm; electrons, _...) -> electrons.density .* electrons.temperature * echarge
    dyexp["core_profiles.profiles_1d[:].electrons.temperature"] = (rho_tor_norm; electrons, _...) -> electrons.pressure ./ (electrons.density * echarge)
    dyexp["core_profiles.profiles_1d[:].electrons.density"] = (rho_tor_norm; electrons, _...) -> electrons.pressure ./ (electrons.temperature * echarge)
    @test_throws IMASdd.IMASexpressionRecursion profiles_1d.electrons.density[1]

    # test expressions using scalar quantities
    time_slice = IMASdd.equilibrium__time_slice()
    time_slice.profiles_1d.psi = range(0.0, 1.0, 11)
    time_slice.profiles_1d.volume = range(0.0, 1.0, 11)
    time_slice.profiles_1d.pressure = 1.0 .- range(0.0, 1.0, 11)
    @test time_slice.global_quantities.energy_mhd ≈ 0.75

    # test expressions across different IDSs with global_time information
    dd = IMASdd.dd()
    resize!(dd.equilibrium.time_slice, 1.0)
    resize!(dd.equilibrium.time_slice, 2.0)
    dd.global_time = 2.0
    dd.equilibrium.time_slice[].profiles_1d.psi = LinRange(0, 1, 11)
    dd.equilibrium.time_slice[].profiles_1d.rho_tor_norm = LinRange(0, 1, 11)
    dd.equilibrium.time_slice[].profiles_1d.volume = LinRange(0, 1, 11)
    resize!(dd.core_profiles.profiles_1d, 2.0)
    dd.core_profiles.profiles_1d[].grid.rho_tor_norm = LinRange(0, 1, 11)
    @test all(dd.core_profiles.profiles_1d[1].grid.volume .+ 1 .≈ dd.equilibrium.time_slice[2].profiles_1d.volume .+ 1)

    # test equilibrium.time_slice[:].time expression
    push!(dd.equilibrium.time_slice, IMASdd.equilibrium__time_slice())
    @test length(dd.equilibrium.time_slice) == 3
    @test_throws BoundsError dd.equilibrium.time_slice[3].time

    # test freeze of top-level dd
    @test all(IMASdd.freeze(dd).core_profiles.profiles_1d[].grid.volume .== dd.core_profiles.profiles_1d[].grid.volume)

    # test freeze of substructure with expression that depends on parent
    @test all(IMASdd.freeze(dd.core_profiles.profiles_1d[].grid).volume .== dd.core_profiles.profiles_1d[].grid.volume)

    # test ancestors function
    @test IMASdd.ids_ancestors(dd.core_profiles.profiles_1d[].grid)[:dd] === dd
    @test IMASdd.ids_ancestors(dd.core_profiles.profiles_1d[].grid)[:core_profiles] === dd.core_profiles
    @test IMASdd.ids_ancestors(dd.core_profiles.profiles_1d[].grid)[:profiles_1d] === dd.core_profiles.profiles_1d[]
    @test IMASdd.ids_ancestors(dd.core_profiles.profiles_1d[].grid)[:grid] === dd.core_profiles.profiles_1d[].grid
    @test IMASdd.ids_ancestors(dd.core_profiles.profiles_1d[])[:dd] === dd
    @test IMASdd.ids_ancestors(dd.core_profiles.profiles_1d[])[:core_profiles] === dd.core_profiles
    @test IMASdd.ids_ancestors(dd.core_profiles.profiles_1d[])[:profiles_1d] === dd.core_profiles.profiles_1d[]
    @test IMASdd.ids_ancestors(dd.core_profiles)[:dd] === dd
    @test IMASdd.ids_ancestors(dd.core_profiles)[:core_profiles] === dd.core_profiles
    @test IMASdd.ids_ancestors(dd)[:dd] === dd
end
