using IMASDD
import IMASDD as IMAS
using Test

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "IDS" begin
    # instantiate and populate top-level IDS
    dd = IMAS.dd()
    resize!(dd.core_profiles.profiles_1d, 1)
    dd.core_profiles.profiles_1d[1].grid.rho_tor_norm = range(0, 1, 10)
    @test length(dd.core_profiles.profiles_1d[1].grid.rho_tor_norm) == 10

    # try adding some dd to separate core_profiles IDS
    crp = IMAS.core_profiles()
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
    crp1d = IMAS.core_profiles__profiles_1d()
    crp1d.grid.rho_tor_norm = range(0, 1, 10)
    crp1d.electrons.density = (1.0 .- crp1d.grid.rho_tor_norm) .^ 2
    @test length(crp1d.grid.rho_tor_norm) == 10

    # reach top IDS starting at different depths
    @test dd === IMAS.top_dd(dd)
    @test dd === IMAS.top_dd(dd.core_profiles.profiles_1d)
    @test dd === IMAS.top_dd(dd.core_profiles.profiles_1d[1])
    @test dd === IMAS.top_dd(dd.core_profiles.profiles_1d[1].grid)

    # test that the top_ids() function stops at the IDS level by default
    @test_throws Exception IMAS.top_ids(dd)
    @test dd.core_profiles === IMAS.top_ids(dd.core_profiles.profiles_1d)
    @test dd.core_profiles === IMAS.top_ids(dd.core_profiles.profiles_1d[1])
    @test dd.core_profiles === IMAS.top_ids(dd.core_profiles.profiles_1d[1].grid)

    # test top_dd() and top_ids() working on a sub structure that does not reach a top-level ids nor dd
    @test nothing === IMAS.top_ids(crp1d)
    @test nothing === IMAS.top_ids(crp1d.grid)
    @test nothing === IMAS.top_dd(crp1d)
    @test nothing === IMAS.top_dd(crp1d.grid)

    # test top() working on a sub structure
    @test crp1d === IMAS.top(crp1d; IDS_is_absolute_top=false)
    @test crp1d === IMAS.top(crp1d.grid; IDS_is_absolute_top=false)
    @test crp1d === IMAS.top(crp1d)
    @test crp1d === IMAS.top(crp1d.grid)

    # add structure to an array of structures
    push!(dd.core_profiles.profiles_1d, crp1d)
    @test dd.core_profiles === IMAS.top_ids(crp1d)

    # test fail of adding dd without coordinate in IDS
    dd = IMAS.dd()
    resize!(dd.core_profiles.profiles_1d, 1)
    @test_throws Exception dd.core_profiles.profiles_1d[1].electrons.temperature = Vector{Float64}(collect(1:10))

    # make sure an error is raised when trying to access missing dd
    @test_throws Exception dd.core_profiles.profiles_1d[1].j_total

    # resize! using pair of string and values as conditions to create new entry
    dd = IMAS.dd()
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
end

@testset "goto" begin
    dd = IMAS.dd()
    @test IMAS.goto(dd, "equilibrium") === dd.equilibrium
    @test IMAS.goto(dd.equilibrium, "equilibrium") === dd.equilibrium
    eq = IMAS.equilibrium()
    @test IMAS.goto(eq, "equilibrium") === eq
    @test IMAS.goto(eq.time_slice, "time_slice") === eq.time_slice
    @test IMAS.goto(eq.time_slice, "vacuum_toroidal_field") === eq.vacuum_toroidal_field
end

@testset "IDS_IMAS" begin
    dd = IMAS.dd()
    resize!(dd.core_profiles.profiles_1d, 2)
    @assert :global_time ∉ collect(keys(dd))

    # test f2u
    @test IMAS.f2u(dd.core_profiles.profiles_1d[1].grid) == "core_profiles.profiles_1d[:].grid"
    @test_throws MethodError IMAS.f2u(:core_profiles__profiles_1d___grid)
    @test_throws MethodError IMAS.f2u("core_profiles__profiles_1d___grid")

    # test i2p
    @test IMAS.i2p("core_profiles.profiles_1d[1].grid") == ["core_profiles", "profiles_1d", "1", "grid"]
    @test IMAS.i2p("core_profiles.profiles_1d[:].grid") == ["core_profiles", "profiles_1d", ":", "grid"]

    # test p2i
    @test IMAS.p2i(["core_profiles", "profiles_1d", "1", "grid"]) == "core_profiles.profiles_1d[1].grid"
    @test IMAS.p2i(["core_profiles", "profiles_1d", ":", "grid"]) == "core_profiles.profiles_1d[:].grid"

    # test nested resizing
    wall = IMAS.wall()
    resize!(wall.description_2d, 1)
    resize!(wall.description_2d[1].mobile.unit, 2)
    resize!(wall.description_2d[1].mobile.unit[2].outline, 2)
    wall__description_2d = IMAS.wall__description_2d()
    resize!(wall__description_2d.mobile.unit, 2)
    resize!(wall__description_2d.mobile.unit[2].outline, 2)

    # test f2p
    @test collect(IMAS.f2p(wall.description_2d[1].mobile.unit[2].outline[1])) == ["wall", "description_2d", "1", "mobile", "unit", "2", "outline", "1"]
    @test collect(IMAS.f2p(wall__description_2d.mobile.unit[2].outline[1])) == ["wall", "description_2d", "0", "mobile", "unit", "2", "outline", "1"]

    # test info
    @test IMAS.info("core_profiles.profiles_1d") == IMAS.info("core_profiles.profiles_1d[:]")

    # test coordinate of a coordinate
    coords = IMAS.coordinates(dd.core_profiles.profiles_1d[1].grid, :rho_tor_norm)
    @test coords.names[1] == "1...N"
    @test typeof(coords.values) <: Vector{Vector{Float64}}
    @test isempty(coords.values[1])

    # test coordinate of a 1D array (with uninitialized coordinate)
    coords = IMAS.coordinates(dd.core_profiles.profiles_1d[1].electrons, :temperature)
    @test coords.names[1] == "core_profiles.profiles_1d[:].grid.rho_tor_norm"
    @test typeof(coords.values) <: Vector{Vector{Float64}}
    @test isempty(coords.values[1])

    # test coordinate of a 1D array (with initialized coordinate)
    dd.core_profiles.profiles_1d[1].grid.rho_tor_norm = range(0, 1, 10)
    dd.core_profiles.profiles_1d[2].grid.rho_tor_norm = range(0, 1, 3)
    coords = IMAS.coordinates(dd.core_profiles.profiles_1d[1].electrons, :temperature)
    @test coords.names[1] == "core_profiles.profiles_1d[:].grid.rho_tor_norm"
    @test coords.values[1] === dd.core_profiles.profiles_1d[1].grid.rho_tor_norm
    @test length(coords.values[1]) == 10
    coords = IMAS.coordinates(dd.core_profiles.profiles_1d[2].electrons, :temperature)
    @test coords.names[1] == "core_profiles.profiles_1d[:].grid.rho_tor_norm"
    @test coords.values[1] === dd.core_profiles.profiles_1d[2].grid.rho_tor_norm
    @test length(coords.values[1]) == 3

    # test coordinate of a 2D array (with uninitialized coordinates)
    pf_active = IMAS.pf_active()
    coil = resize!(pf_active.coil, 1)[1]
    @test all(IMAS.coordinates(coil, :current_limit_max).values .== [Float64[], Float64[]])

    # test working with IDSvectorElement standalone or in a IDSvector
    dd = IMAS.dd()
    resize!(dd.core_profiles.profiles_1d, 1)
    for profiles_1d in (dd.core_profiles.profiles_1d[1], IMAS.core_profiles__profiles_1d())
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
    dd = IMAS.dd()
    @test_throws Exception getproperty(dd.equilibrium.vacuum_toroidal_field, r0, missing) === missing
    @test getproperty(dd.equilibrium.vacuum_toroidal_field, :r0, missing) === missing
    @test getproperty(dd.equilibrium.vacuum_toroidal_field, :r0, 0.0) == 0.0
end

@testset "convert" begin
    dd = IMAS.dd()
    build_Real = IMAS.ids_convert(IMAS.build{Real}, dd.build)
    @test typeof(build_Real).parameters[1] === Real
end

@testset "lazycopy" begin
    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.json")
    dd = IMAS.json2imas(filename)
    eq1 = dd.equilibrium

    push!(eq1.time_slice, IMAS.lazycopy(eq1.time_slice[end]))
    push!(eq1.time_slice, IMAS.lazycopy(eq1.time_slice[end]))

    @test eq1.time_slice[end].global_quantities.ip == eq1.time_slice[end-1].global_quantities.ip == eq1.time_slice[end-2].global_quantities.ip

    @test getfield(eq1.time_slice[end].global_quantities, :ip) != getfield(eq1.time_slice[end-2].global_quantities, :ip)
    @test getfield(eq1.time_slice[end-1].global_quantities, :ip) != getfield(eq1.time_slice[end-2].global_quantities, :ip)

    eq1.time_slice[end].global_quantities.ip += 1.0
    @test eq1.time_slice[end].global_quantities.ip != eq1.time_slice[end-1].global_quantities.ip
    @test eq1.time_slice[end].global_quantities.ip != eq1.time_slice[end-2].global_quantities.ip
    @test eq1.time_slice[end-1].global_quantities.ip == eq1.time_slice[end-2].global_quantities.ip

    eq2 = IMAS.lazycopy(eq1)

    @test length(eq2.time_slice) == length(eq1.time_slice)

    eq2.time_slice[end].global_quantities.ip += 1.0
    @test eq2.time_slice[end].global_quantities.ip != eq1.time_slice[end].global_quantities.ip

    # ===========

    dd = IMAS.dd()
    dd.equilibrium.vacuum_toroidal_field.r0 = 1.0
    dd.equilibrium.time = [1.0]
    ddR = IMAS.lazycopy(Real, dd);

    ddR.equilibrium.vacuum_toroidal_field.b0 = Real[1.0]

    @assert eltype(ddR.equilibrium.vacuum_toroidal_field.b0) === Real
    @assert eltype(ddR.equilibrium.vacuum_toroidal_field.r0) === Float64
end
