using IMASdd
import IMASdd
import IMASdd: @ddtime
using Test

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "time_ids" begin
    dd = IMASdd.dd()

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
    @test_throws Exception push!(dd.equilibrium.time_slice, eqt, dd.global_time)

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
    dd = IMASdd.dd()
    for i in 1:2
        isource = resize!(dd.core_sources.source, "identifier.index" => 3)
        resize!(isource.profiles_1d)
        @test dd.core_sources.source[1] === dd.core_sources.source[end] === isource
    end

    # resize!(dd.equilibrium.time_slice) returns an empty IDS
    dd = IMASdd.dd()
    eqt = resize!(dd.equilibrium.time_slice)
    eqt.global_quantities.ip = 1.0
    eqt = resize!(dd.equilibrium.time_slice)
    @test ismissing(eqt.global_quantities, :ip)
    @test !ismissing(eqt, :time)

    # edge case for nearest_causal_time
    @test IMASdd.nearest_causal_time([-Inf], -Inf) == (index=1, perfect_match=true, causal_time=-Inf, out_of_bounds=false)
    @test IMASdd.nearest_causal_time([-Inf, 0.0], 0.0) == (index=2, perfect_match=true, causal_time=0.0, out_of_bounds=false)
    @test IMASdd.nearest_causal_time([-Inf, 0.0], 1.0) == (index=2, perfect_match=false, causal_time=0.0, out_of_bounds=false)
    @test IMASdd.nearest_causal_time([-Inf, 0.0, Inf], Inf) == (index=3, perfect_match=true, causal_time=Inf, out_of_bounds=false)
end

@testset "time_array" begin
    dd = IMASdd.dd()

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
    @test IMASdd.set_time_array(dd.equilibrium.vacuum_toroidal_field, :b0, time, -2.0) == -2.0
    @test IMASdd.get_time_array(dd.equilibrium.vacuum_toroidal_field, :b0, time) == -2.0

    # test time interpolation when time array is longer than data array
    dd = IMASdd.dd()
    resize!(dd.equilibrium.time_slice)
    dd.equilibrium.vacuum_toroidal_field.r0 = 1.0
    IMASdd.set_time_array(dd.equilibrium.vacuum_toroidal_field, :b0, 10.0)
    dd.global_time = 1.0
    resize!(dd.equilibrium.time_slice)
    @test @ddtime(dd.equilibrium.vacuum_toroidal_field.b0) .== 10.0

    # test get_time_array(dd_location, :Symbol, :Vector{Real})
    ecl = resize!(dd.ec_launchers.beam, 1)[1]
    ecl.power_launched.time = [1.0, 3.0, 5.0, 1000.0]
    ecl.power_launched.data = [0.0, 5e6, 2e6]
    time_wanted = [1.0, 4.0, 100.0]
    @test IMASdd.get_time_array(dd.ec_launchers.beam[1].power_launched, :data, time_wanted, :constant) == [0.0, 5.0e6, 2.0e6]

    # test multi-dimensional arrays
    dd = IMASdd.dd()
    dd.global_time = 0.0
    ecb = resize!(dd.ec_launchers.beam, 1)[1]
    @test (@ddtime(ecb.spot.size = [1.1, 0.1])) == [1.1, 0.1]
    @test (@ddtime(ecb.spot.size = [2.2, 0.2])) == [2.2, 0.2]

    dd.global_time = 1.0
    @test (@ddtime(ecb.spot.size = [3.3, 0.3])) == [3.3, 0.3]
    @test (@ddtime(ecb.spot.size = [4.4, 0.4])) == [4.4, 0.4]

    dd.global_time = 0.0
    @test (@ddtime(ecb.spot.size = [5.5, 0.5])) == [5.5, 0.5]
    @test (@ddtime(ecb.spot.size = [6.6, 0.6])) == [6.6, 0.6]

    push!(ecb.time, 2.0)

    dd.global_time = 3.0
    @test (@ddtime(ecb.spot.size = [7.7, 0.7])) == [7.7, 0.7]
    @test @ddtime(ecb.spot.size) == [7.7, 0.7]

    empty!(ecb.spot, :size)
    @test (@ddtime(ecb.spot.size = [7.7, 0.7])) == [7.7, 0.7]
    @test @ddtime(ecb.spot.size) == [7.7, 0.7]
    dd.global_time = 0.0
    @test all(isnan, @ddtime(ecb.spot.size))
end

@testset "get_timeslice" begin
    dd = IMASdd.dd()

    resize!(dd.equilibrium.time_slice, 2)
    dd.equilibrium.time_slice[1].time = 1.0
    dd.equilibrium.time_slice[2].time = 2.0
    dd.equilibrium.time = [1.0, 2.0]
    dd.equilibrium.vacuum_toroidal_field.r0 = 0.0
    dd.equilibrium.vacuum_toroidal_field.b0 = [-1.0, -2.0]

    dd0 = IMASdd.get_timeslice(dd, 1.5, :linear)
    @test dd0.equilibrium.time_slice[1].time == 1.5
    @test dd0.equilibrium.time == [1.5]
    @test dd0.equilibrium.vacuum_toroidal_field.r0 == 0.0
    @test dd0.equilibrium.vacuum_toroidal_field.b0 == [-1.5]

    dd0 = IMASdd.get_timeslice(dd, 10.0, :linear)
    @test dd0.equilibrium.time_slice[1].time == 10.0
    @test dd0.equilibrium.time == [10.0]
    @test dd0.equilibrium.vacuum_toroidal_field.r0 == 0.0
    @test dd0.equilibrium.vacuum_toroidal_field.b0 == [-2]

    @test_throws Exception IMASdd.get_timeslice(dd, -10.0)

    dd0 = IMASdd.get_timeslice(dd, 1.5, :constant)
    @test dd0.equilibrium.time_slice[1].time == 1.5
    @test dd0.equilibrium.time == [1.5]
    @test dd0.equilibrium.vacuum_toroidal_field.r0 == 0.0
    @test dd0.equilibrium.vacuum_toroidal_field.b0 == [-1.0]

    dd0 = IMASdd.get_timeslice(dd, 1.5) # default is :constant
    @test dd0.equilibrium.time_slice[1].time == 1.5
    @test dd0.equilibrium.time == [1.5]
    @test dd0.equilibrium.vacuum_toroidal_field.r0 == 0.0
    @test dd0.equilibrium.vacuum_toroidal_field.b0 == [-1.0]
end

@testset "homogeneous_time" begin
    dd = IMASdd.dd()

    # check propagation of homogeneous_time to arrays of time dependent structures
    dd.equilibrium.time = [0.0, 1.0]
    resize!(dd.equilibrium.time_slice, 2)
    @test dd.equilibrium.time_slice[1].time == 0.0
    @test dd.equilibrium.time_slice[2].time == 1.0

    # check propagation of homogeneous_time to time arrays below when using @ddtime
    dd.nbi.time = [0.0, 1.0]
    resize!(dd.nbi.unit, 1)
    setproperty!(dd.nbi.unit[1].power_launched, :data, [10.0, 20.0]; error_on_missing_coordinates=false)
    @test @ddtime(dd.nbi.unit[1].power_launched.data) == 10.0
    @test dd.nbi.unit[1].power_launched.time == dd.nbi.time

    # check that accessing time at a low level IDS does set it
    dd.nbi.time = [0.0, 1.0]
    resize!(dd.nbi.unit, 1)
    @test dd.nbi.unit[1].power_launched.time == dd.nbi.time

    # check that accessing time at a top-level IDS does not set it
    @test_throws IMASdd.IMASmissingDataException dd.core_profiles.time
    @test !hasdata(dd.core_profiles, :time)
end