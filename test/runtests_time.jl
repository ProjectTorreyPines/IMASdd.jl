using IMASdd
import IMASdd as IMAS
import IMASdd: @ddtime
using Test

include(joinpath(@__DIR__,"test_expressions_dicts.jl"))

@testset "time_ids" begin
    dd = IMAS.dd()

    dd.global_time = 1010.0
    eqt = resize!(dd.equilibrium.time_slice)
    @test length(dd.equilibrium.time_slice) == 1
    n = length(dd.equilibrium.time_slice)
    eqt.global_quantities.ip = 1.0
    @test dd.equilibrium.time_slice[].global_quantities.ip == 1.0
    @test dd.equilibrium.time_slice[1010.0].global_quantities.ip === eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[2000.0].global_quantities.ip === eqt.global_quantities.ip
    @test_throws Exception dd.equilibrium.time_slice[1000.0].global_quantities.ip
    @test dd.equilibrium.time_slice[10000.0].global_quantities.ip === eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[n].global_quantities.ip === eqt.global_quantities.ip
    @test_throws Exception dd.equilibrium.time_slice[n+1].global_quantities.ip
    @test dd.equilibrium.time_slice[].global_quantities.ip === eqt.global_quantities.ip
    @test_throws Exception push!(dd.equilibrium.time_slice, eqt, dd.global_time) == eqt

    dd.global_time = 2020.0
    eqt = resize!(dd.equilibrium.time_slice)
    @test length(dd.equilibrium.time_slice) == 2
    n = length(dd.equilibrium.time_slice)
    dd.equilibrium.time_slice[].global_quantities.ip = 2.0
    @test dd.equilibrium.time_slice[].global_quantities.ip == 2.0
    @test dd.equilibrium.time_slice[2020.0].global_quantities.ip === eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[2000.0].global_quantities.ip === dd.equilibrium.time_slice[1010.0].global_quantities.ip
    @test_throws Exception dd.equilibrium.time_slice[1000.0].global_quantities.ip
    @test dd.equilibrium.time_slice[10000.0].global_quantities.ip === eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[n].global_quantities.ip === eqt.global_quantities.ip
    @test_throws Exception dd.equilibrium.time_slice[n+1].global_quantities.ip
    @test dd.equilibrium.time_slice[].global_quantities.ip === eqt.global_quantities.ip
    @test_throws Exception (dd.equilibrium.time_slice[] = eqt)
    @test_throws  Exception push!(dd.equilibrium.time_slice, eqt, dd.global_time)

    dd.global_time = 3030.0
    eqt = resize!(dd.equilibrium.time_slice)
    @test length(dd.equilibrium.time_slice) == 3
    n = length(dd.equilibrium.time_slice)
    dd.equilibrium.time_slice[].global_quantities.ip = 3.0
    @test dd.equilibrium.time_slice[].global_quantities.ip == 3.0
    @test dd.equilibrium.time_slice[3030.0].global_quantities.ip === eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[3000.0].global_quantities.ip === dd.equilibrium.time_slice[2020.0].global_quantities.ip
    @test_throws Exception dd.equilibrium.time_slice[1000.0].global_quantities.ip
    @test dd.equilibrium.time_slice[10000.0].global_quantities.ip === eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[n].global_quantities.ip === eqt.global_quantities.ip
    @test_throws Exception dd.equilibrium.time_slice[n+1].global_quantities.ip
    @test dd.equilibrium.time_slice[].global_quantities.ip === eqt.global_quantities.ip
    @test_throws Exception (dd.equilibrium.time_slice[] = eqt)
    @test_throws Exception push!(dd.equilibrium.time_slice, eqt, dd.global_time) # push! will complain trying to enter data at an earlier (or equal) time

    # dial back global time at an existing time (closest time slice is 2)
    dd.global_time = 2020.0
    eqt = dd.equilibrium.time_slice[2]
    @test length(dd.equilibrium.time_slice) == 3
    @test dd.equilibrium.time_slice[2020.0].global_quantities.ip === eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[2000.0].global_quantities.ip === dd.equilibrium.time_slice[1010.0].global_quantities.ip
    @test_throws Exception dd.equilibrium.time_slice[1000.0].global_quantities.ip
    @test dd.equilibrium.time_slice[10000.0].global_quantities.ip !== eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[2].global_quantities.ip === eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[].global_quantities.ip === eqt.global_quantities.ip
    @test_throws Exception (dd.equilibrium.time_slice[] = eqt)
    @test_throws Exception push!(dd.equilibrium.time_slice, eqt, dd.global_time) # push! will complain trying to enter data at an earlier time

    # dial back global time time (closest CAUSAL time slice is still 2)
    dd.global_time = 3021.0
    eqt = dd.equilibrium.time_slice[2]
    @test length(dd.equilibrium.time_slice) == 3
    @test dd.equilibrium.time_slice[2020.0].global_quantities.ip === eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[2000.0].global_quantities.ip === dd.equilibrium.time_slice[1010.0].global_quantities.ip
    @test_throws Exception dd.equilibrium.time_slice[1000.0].global_quantities.ip
    @test dd.equilibrium.time_slice[10000.0].global_quantities.ip !== eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[2].global_quantities.ip === eqt.global_quantities.ip
    @test dd.equilibrium.time_slice[].global_quantities.ip === eqt.global_quantities.ip
    @test_throws Exception (dd.equilibrium.time_slice[] = eqt)
    @test_throws Exception push!(dd.equilibrium.time_slice, eqt, dd.global_time) # push! will complain trying to enter data at an earlier time that is not in time array

    # resize! will complain if trying to resize at an earlier time
    @test_throws Exception resize!(dd.equilibrium.time_slice)

    # resize! with global time will complain operating on IDSvectors that are not time dependent
    @test_throws Exception resize!(dd.wall.description_2d)
    @test_throws Exception resize!(dd.wall.description_2d, 1000.0)

    # add an empty time-slice
    dd.global_time = 4040.0
    eqt = resize!(dd.equilibrium.time_slice)
    @test length(dd.equilibrium.time_slice) == 4

    dd.global_time = 5050.0
    eqt = resize!(dd.equilibrium.time_slice)
    @test length(dd.equilibrium.time_slice) == 5
    @test_throws Exception dd.equilibrium.time_slice[].global_quantities.ip # elements within arrays of structures do not time-interpolate
    dd.equilibrium.time_slice[].global_quantities.ip = 5.0
    @test dd.equilibrium.time_slice[].global_quantities.ip === eqt.global_quantities.ip

    # https://github.com/ProjectTorreyPines/FUSE.jl/issues/18
    dd = IMAS.dd()
    for i in 1:2
        isource = resize!(dd.core_sources.source, "identifier.index" => 3)
        resize!(isource.profiles_1d)
        @test dd.core_sources.source[1] === dd.core_sources.source[end] === isource
    end

    # resize!(dd.equilibrium.time_slice) returns an empty IDS
    dd = IMAS.dd()
    eqt = resize!(dd.equilibrium.time_slice)
    eqt.global_quantities.ip = 1.0
    eqt = resize!(dd.equilibrium.time_slice)
    @test ismissing(eqt.global_quantities, :ip)
    @test !ismissing(eqt, :time)
end

@testset "time_array" begin
    dd = IMAS.dd()

    dd.global_time = 1010.0
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0 = 1.0) == 1.0
    @test dd.equilibrium.time == [1010.0]
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0) == 1.0
    @test dd.equilibrium.vacuum_toroidal_field.b0 == [1.0]

    dd.global_time = 2020.0
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0 = 2.0) == 2.0
    @test dd.equilibrium.time == [1010.0, 2020.0]
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0) == 2.0
    @test dd.equilibrium.vacuum_toroidal_field.b0 == [1.0, 2.0]

    dd.global_time = 3030.0
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0) == 2.0
    @test dd.equilibrium.time == [1010.0, 2020.0]
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0 = 3.0) == 3.0
    @test dd.equilibrium.time == [1010.0, 2020.0, 3030.0]
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0) == 3.0
    @test dd.equilibrium.vacuum_toroidal_field.b0 == [1.0, 2.0, 3.0]

    # edit something in the past
    dd.global_time = 2020.0
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0 = -2.0) == -2.0

    # insert a time
    push!(dd.equilibrium.time, 4040.0)
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0 = -2.0) == -2.0

    # test extrapolation when time exists
    dd.global_time = 4040.0
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0) == 3.0
    @test dd.equilibrium.vacuum_toroidal_field.b0 == [1.0, -2.0, 3.0]

    # test interpolation and insertion
    dd.global_time = 5050.0
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0 = 5.0) == 5.0
    @test dd.equilibrium.vacuum_toroidal_field.b0 == [1.0, -2.0, 3.0, 3.0, 5.0]

    # test insertion at later time of an empty field
    empty!(dd.equilibrium.vacuum_toroidal_field.b0)
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0 = 5.0) == 5.0
    @test all(dd.equilibrium.vacuum_toroidal_field.b0 .=== [NaN, NaN, NaN, NaN, 5.0])

    # test write/read at times specified as arguments
    time = 2020.0
    @test IMAS.set_time_array(dd.equilibrium.vacuum_toroidal_field, :b0, time, -2.0) == -2.0
    @test IMAS.get_time_array(dd.equilibrium.vacuum_toroidal_field, :b0, time) == -2.0

    # test time interpolation when time array is longer than data array
    dd = IMAS.dd()
    resize!(dd.equilibrium.time_slice)
    dd.equilibrium.vacuum_toroidal_field.r0 = 1.0
    IMAS.set_time_array(dd.equilibrium.vacuum_toroidal_field, :b0, 10.0)
    dd.global_time = 1.0
    resize!(dd.equilibrium.time_slice)
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0) .== 10.0

    # test get_time_array(dd_location, :Symbol, :Vector{Real})
    ecl = resize!(dd.ec_launchers.beam, 1)[1]
    ecl.power_launched.time = [1.0, 3.0, 5.0, 1000.0]
    ecl.power_launched.data = [0.0, 5e6, 2e6]
    time_wanted = [1.0, 4.0, 100.0]
    @test IMAS.get_time_array(dd.ec_launchers.beam[1].power_launched, :data, time_wanted, :constant) == [0.0, 5.0e6, 2.0e6]
end

@testset "get_timeslice" begin
    dd = IMAS.dd()

    resize!(dd.equilibrium.time_slice, 2)
    dd.equilibrium.time_slice[1].time = 1.0
    dd.equilibrium.time_slice[2].time = 2.0
    dd.equilibrium.time = [1.0, 2.0]
    dd.equilibrium.vacuum_toroidal_field.r0 = 0.0
    dd.equilibrium.vacuum_toroidal_field.b0 = [-1.0, -2.0]

    dd0 = IMAS.get_timeslice(dd, 1.5)
    @test dd0.equilibrium.time_slice[1].time == 1.5
    @test dd0.equilibrium.time == [1.5]
    @test dd0.equilibrium.vacuum_toroidal_field.r0 == 0.0
    @test dd0.equilibrium.vacuum_toroidal_field.b0 == [-1.5]

    dd0 = IMAS.get_timeslice(dd, 10.0)
    @test dd0.equilibrium.time_slice[1].time == 10.0
    @test dd0.equilibrium.time == [10.0]
    @test dd0.equilibrium.vacuum_toroidal_field.r0 == 0.0
    @test dd0.equilibrium.vacuum_toroidal_field.b0 == [-2]

    @test_throws Exception IMAS.get_timeslice(dd, -10.0)

    dd0 = IMAS.get_timeslice(dd, 1.5, :constant)
    @test dd0.equilibrium.time_slice[1].time == 1.5
    @test dd0.equilibrium.time == [1.5]
    @test dd0.equilibrium.vacuum_toroidal_field.r0 == 0.0
    @test dd0.equilibrium.vacuum_toroidal_field.b0 == [-1.0]
end