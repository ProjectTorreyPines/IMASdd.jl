using IMASdd
import IMASdd as IMAS
using Test

include(joinpath(@__DIR__, "test_expressions_dicts.jl"))

@testset "threads" begin

    dd = IMAS.dd()

    dd.global_time = -1.0
    Threads.@threads for k in range(1,10)
        # test persistance of global time among threads
        @test dd.global_time == -1.0
    end

    Threads.@threads for k in range(1,10)
        # test independence of global time between threads
        dd.global_time = k
        @test dd.global_time != -1.0
    end

end