module IMASdd

import PrecompileTools
import OrderedCollections

const document = OrderedCollections.OrderedDict()

include("data_header.jl")

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
PrecompileTools.@compile_workload begin
    dd()
end

end # module
