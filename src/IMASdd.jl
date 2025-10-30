module IMASdd

import OrderedCollections
import CoordinateConventions
import ThreadSafeDicts
import Memoization

const document = OrderedCollections.OrderedDict()

include("data_header.jl")

include("data.jl")

include("findall.jl")

include("cocos.jl")

include("expressions.jl")

include("error.jl")

include("dd.jl")

include("identifiers.jl")

include("io.jl")

include("show.jl")

include("time.jl")

include("f2.jl")

include("math.jl")

include("precompile_workload.jl")

end # module
