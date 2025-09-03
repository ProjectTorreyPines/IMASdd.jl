# Register expressions in the absence of IMAS.jl

const dynamic_expressions = dyexp = Dict{String,Function}()
IMASdd.set_dynamic_expressions(dynamic_expressions)

const onetime_expressions = otexp = Dict{String,Function}()
IMASdd.set_onetime_expressions(onetime_expressions)
