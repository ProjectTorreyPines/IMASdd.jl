using IMASdd
import IMASdd
using Test

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "IDS" begin
    # instantiate and populate top-level IDS
    dd = IMASdd.dd()
    resize!(dd.core_profiles.profiles_1d, 1)
    dd.core_profiles.profiles_1d[1].grid.rho_tor_norm = range(0, 1, 10)
    @test length(dd.core_profiles.profiles_1d[1].grid.rho_tor_norm) == 10

    # try adding some dd to separate core_profiles IDS
    crp = IMASdd.core_profiles()
    resize!(crp.profiles_1d, 1)
    crp.profiles_1d[1].grid.rho_tor_norm = range(0, 1, 10)
    crp.profiles_1d[1].electrons.density = (1.0 .- crp.profiles_1d[1].grid.rho_tor_norm) .^ 2
    @test length(crp.profiles_1d[1].grid.rho_tor_norm) == 10

    # resize an array of struct
    resize!(crp.profiles_1d, 2)
    @test length(crp.profiles_1d) == 2

    # deepcopy of one arrray structure element to another
    crp.profiles_1d[2] = deepcopy(crp.profiles_1d[1])
    crp.profiles_1d[2].electrons.density = crp.profiles_1d[2].electrons.density .* 2.0
    @test all(crp.profiles_1d[2].grid.rho_tor_norm .== crp.profiles_1d[1].grid.rho_tor_norm)
    @test all(crp.profiles_1d[2].electrons.density .== (crp.profiles_1d[1].electrons.density * 2.0))

    # working with dd that is not time dependent? --> only use the relevant struct
    crp1d = IMASdd.core_profiles__profiles_1d()
    crp1d.grid.rho_tor_norm = range(0, 1, 10)
    crp1d.electrons.density = (1.0 .- crp1d.grid.rho_tor_norm) .^ 2
    @test length(crp1d.grid.rho_tor_norm) == 10

    # reach top IDS starting at different depths
    @test dd === IMASdd.top_dd(dd)
    @test dd === IMASdd.top_dd(dd.core_profiles.profiles_1d)
    @test dd === IMASdd.top_dd(dd.core_profiles.profiles_1d[1])
    @test dd === IMASdd.top_dd(dd.core_profiles.profiles_1d[1].grid)

    # test that the top_ids() function stops at the IDS level by default
    @test_throws Exception IMASdd.top_ids(dd)
    @test dd.core_profiles === IMASdd.top_ids(dd.core_profiles.profiles_1d)
    @test dd.core_profiles === IMASdd.top_ids(dd.core_profiles.profiles_1d[1])
    @test dd.core_profiles === IMASdd.top_ids(dd.core_profiles.profiles_1d[1].grid)

    # test top_dd() and top_ids() working on a sub structure that does not reach a top-level ids nor dd
    @test nothing === IMASdd.top_ids(crp1d)
    @test nothing === IMASdd.top_ids(crp1d.grid)
    @test nothing === IMASdd.top_dd(crp1d)
    @test nothing === IMASdd.top_dd(crp1d.grid)

    # add structure to an array of structures
    push!(dd.core_profiles.profiles_1d, crp1d)
    @test dd.core_profiles === IMASdd.top_ids(crp1d)

    # test fail of adding dd without coordinate in IDS
    dd = IMASdd.dd()
    resize!(dd.core_profiles.profiles_1d, 1)
    @test_throws Exception dd.core_profiles.profiles_1d[1].electrons.temperature = Vector{Float64}(collect(1:10))

    # make sure an error is raised when trying to access missing dd
    @test_throws Exception dd.core_profiles.profiles_1d[1].j_total

    # resize! using pair of string and values as conditions to create new entry
    dd = IMASdd.dd()
    resize!(dd.core_profiles.profiles_1d)
    ion = resize!(dd.core_profiles.profiles_1d[].ion, "z_ion" => 1.0, "state[1].label" => "hello")
    @test ion === dd.core_profiles.profiles_1d[].ion[end]
    @test length(dd.core_profiles.profiles_1d[].ion) == 1.0
    ion = resize!(dd.core_profiles.profiles_1d[].ion, "z_ion" => 1.0, "state[1].label" => "hello")
    @test length(dd.core_profiles.profiles_1d[].ion) == 1.0
    ion = resize!(dd.core_profiles.profiles_1d[].ion, "z_ion" => 1.0)
    @test ion === dd.core_profiles.profiles_1d[].ion[end]
    @test length(dd.core_profiles.profiles_1d[].ion) == 1.0
    ion = resize!(dd.core_profiles.profiles_1d[].ion, "z_ion" => 2.0)
    @test ion === dd.core_profiles.profiles_1d[].ion[end]
    @test length(dd.core_profiles.profiles_1d[].ion) == 2.0
    ion = resize!(dd.core_profiles.profiles_1d[].ion, "z_ion" => 2.0, "state[1].label" => "hello")
    @test ion === dd.core_profiles.profiles_1d[].ion[end]
    @test length(dd.core_profiles.profiles_1d[].ion) == 3.0
    @test_throws Exception resize!(dd.core_profiles.profiles_1d[].ion, "z_ion" => 2.0)

    # location on a IDS and a IDSvector
    @test IMASdd.location(dd.core_transport.model) == "core_transport.model"
    resize!(dd.core_transport.model, 1)
    @test IMASdd.location(dd.core_transport.model[1]) == "core_transport.model[1]"
end

@testset "goto" begin
    dd = IMASdd.dd()
    @test IMASdd.goto(dd, "equilibrium") === dd.equilibrium
    @test IMASdd.goto(dd.equilibrium, "equilibrium") === dd.equilibrium
    eq = IMASdd.equilibrium()
    @test IMASdd.goto(eq, "equilibrium") === eq
    @test IMASdd.goto(eq.time_slice, "time_slice") === eq.time_slice
    @test IMASdd.goto(eq.vacuum_toroidal_field, "vacuum_toroidal_field") === eq.vacuum_toroidal_field
end

@testset "IDS_IMAS" begin
    dd = IMASdd.dd()
    resize!(dd.core_profiles.profiles_1d, 2)
    @assert :global_time ∉ collect(keys(dd))

    # test nested resizing
    wall = IMASdd.wall()
    resize!(wall.description_2d, 1)
    resize!(wall.description_2d[1].mobile.unit, 2)
    resize!(wall.description_2d[1].mobile.unit[2].outline, 2)
    wall__description_2d = IMASdd.wall__description_2d()
    resize!(wall__description_2d.mobile.unit, 2)
    resize!(wall__description_2d.mobile.unit[2].outline, 2)

    # test info
    @test IMASdd.info("core_profiles.profiles_1d") == IMASdd.info("core_profiles.profiles_1d[:]")

    # test coordinate of a coordinate
    coords = IMASdd.coordinates(dd.core_profiles.profiles_1d[1].grid, :rho_tor_norm)
    @test getproperty(coords[1]) === nothing
    dd.core_profiles.profiles_1d[1].grid.rho_tor_norm = range(0, 1, 10)
    coords = IMASdd.coordinates(dd.core_profiles.profiles_1d[1].grid, :rho_tor_norm)
    @test getproperty(coords[1]) === nothing

    coords = IMASdd.coordinates(dd.core_profiles.profiles_1d[1].grid, :rho_tor_norm; override_coord_leaves=[:psi])
    @test coords[1].field === :psi
    dd.core_profiles.profiles_1d[1].grid.psi = range(0, 1, 10)
    coords = IMASdd.coordinates(dd.core_profiles.profiles_1d[1].grid, :rho_tor_norm; override_coord_leaves=[:psi])
    @test all(getproperty(coords[1]) .== range(0, 1, 10))

    empty!(dd.core_profiles.profiles_1d[1])

    # test coordinate of a 1D array (with uninitialized coordinate)
    coords = IMASdd.coordinates(dd.core_profiles.profiles_1d[1].electrons, :temperature)
    @test IMASdd.location(coords[1].ids, coords[1].field) == "core_profiles.profiles_1d[1].grid.rho_tor_norm"
    @test getproperty(coords[1]) === missing

    # test coordinate of a 1D array (with initialized coordinate)
    dd.core_profiles.profiles_1d[1].grid.rho_tor_norm = range(0, 1, 10)
    dd.core_profiles.profiles_1d[2].grid.rho_tor_norm = range(0, 1, 3)
    coords = IMASdd.coordinates(dd.core_profiles.profiles_1d[1].electrons, :temperature)
    @test IMASdd.location(coords[1].ids, coords[1].field) == "core_profiles.profiles_1d[1].grid.rho_tor_norm"
    @test getproperty(coords[1]) === dd.core_profiles.profiles_1d[1].grid.rho_tor_norm
    coords = IMASdd.coordinates(dd.core_profiles.profiles_1d[2].electrons, :temperature)
    @test IMASdd.location(coords[1].ids, coords[1].field) == "core_profiles.profiles_1d[2].grid.rho_tor_norm"
    @test getproperty(coords[1]) === dd.core_profiles.profiles_1d[2].grid.rho_tor_norm

    # test coordinate of a 2D array (with uninitialized coordinates)
    pf_active = IMASdd.pf_active()
    coil = resize!(pf_active.coil, 1)[1]
    coords = IMASdd.coordinates(coil, :current_limit_max)
    @test getproperty(coords[1]) === missing && getproperty(coords[2]) === missing

    # test working with IDSvectorElement standalone or in a IDSvector
    dd = IMASdd.dd()
    resize!(dd.core_profiles.profiles_1d, 1)
    for profiles_1d in (dd.core_profiles.profiles_1d[1], IMASdd.core_profiles__profiles_1d())
        profiles_1d.grid.rho_tor_norm = x = range(0.0, 1.0, 101)
        profiles_1d.electrons.density = (1.0 .- x .^ 2) .^ 2.0
        profiles_1d.j_total = (1.0 .- x .^ 2) .^ 2.0
        @test length(profiles_1d.electrons.density) == length(profiles_1d.j_total)
    end

    # test conditional resizing
    isource = resize!(dd.core_sources.source, "identifier.index" => 1)
    isource = resize!(dd.core_sources.source, "identifier.index" => 2)
    isource = resize!(dd.core_sources.source, "identifier.index" => 3)
    @test length(dd.core_sources.source) == 3
    isource = resize!(dd.core_sources.source, "identifier.index" => 2)
    @test length(dd.core_sources.source) == 3
end

@testset "utils" begin
    dd = IMASdd.dd()
    @test_throws Exception getproperty(dd.equilibrium.vacuum_toroidal_field, r0, missing) === missing
    @test getproperty(dd.equilibrium.vacuum_toroidal_field, :r0, missing) === missing
    @test getproperty(dd.equilibrium.vacuum_toroidal_field, :r0, 0.0) == 0.0
end

@testset "convert" begin
    dd = IMASdd.dd()
    build_Real = convert(Real, dd.build)
    @test typeof(build_Real).parameters[1] === Real
    build_Fl16 = convert(Float16, dd.build)
    @test typeof(build_Fl16).parameters[1] === Float16
end
