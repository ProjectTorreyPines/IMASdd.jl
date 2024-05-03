module IMASDD

include("data.jl")

include("expressions.jl")

include("error.jl")

include("dd.jl")

include("io.jl")

include("show.jl")

include("time.jl")

include("f2.jl")

include("math.jl")

# call dd here to cache precompiled data structure
dd()

export @ddtime

end # module
