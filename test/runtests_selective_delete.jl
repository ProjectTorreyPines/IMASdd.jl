using IMASdd
using Test

@testset "selective_delete!" begin
    dd = IMASdd.dd()
    path = IMASdd.i2p("core_profiles.profiles_1d[:].ion[:].temperature")

    # empty array of structures: nothing to delete
    @test IMASdd.selective_delete!(dd, path) === false

    resize!(dd.core_profiles.profiles_1d, 2)
    for (k, cp1d) in enumerate(dd.core_profiles.profiles_1d)
        cp1d.time = 0.1 * k
        cp1d.grid.rho_tor_norm = [0.0, 0.5, 1.0]
        resize!(cp1d.ion, 2)
        for ion in cp1d.ion
            ion.temperature = [3.0, 2.0, 1.0]
            ion.density_thermal = [1.0, 1.0, 1.0]
        end
    end
    dd.core_profiles.time = [0.1, 0.2]

    # every element of every array of structures along the path is visited
    @test IMASdd.selective_delete!(dd, path) === true
    for cp1d in dd.core_profiles.profiles_1d, ion in cp1d.ion
        @test !IMASdd.hasdata(ion, :temperature)
        @test IMASdd.hasdata(ion, :density_thermal)
    end

    # nothing left to delete
    @test IMASdd.selective_delete!(dd, path) === false

    # data on a later element only
    dd.core_profiles.profiles_1d[2].ion[2].temperature = [3.0, 2.0, 1.0]
    @test IMASdd.selective_delete!(dd, path) === true
    @test !IMASdd.hasdata(dd.core_profiles.profiles_1d[2].ion[2], :temperature)

    # a path that ends in a plain structure
    dd.core_profiles.profiles_1d[2].electrons.temperature = [3.0, 2.0, 1.0]
    @test IMASdd.selective_delete!(dd, IMASdd.i2p("core_profiles.profiles_1d[:].electrons.temperature")) === true
    @test !IMASdd.hasdata(dd.core_profiles.profiles_1d[2].electrons, :temperature)
end
