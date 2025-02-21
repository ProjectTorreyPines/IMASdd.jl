using IMASdd
import IMASdd
using Test

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "findall" begin

    filename = joinpath(dirname(@__DIR__), "sample", "omas_sample.json")
    dd_json = IMASdd.json2imas(filename)

    # Basic test
    IFF = findall(dd_json, r"psi")
    @test IFF[1].root_name == "dd"
    @test IFF[1].field_path == "dd.core_profiles.profiles_1d[1].grid.psi"
    @test IFF[1].value[1] ≈ -2.2834845617831316

    # custom root name
    IFF = findall(dd_json,r"prof.*1d.*psi$"; root_name = "This is my custrom root_name")
    @test IFF[1].root_name == "This is my custrom root_name"

    # macro test
    eqt = dd_json.equilibrium.time_slice[]

    @test findall(eqt, :psi)[1].field_path == "equilibrium.time_slice[1].profiles_1d.psi"
    @test (@findall eqt  :psi)[1].field_path == "eqt.profiles_1d.psi"
end
