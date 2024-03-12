function IMASDD.get_expressions(::Type{Val{:dynamic}})
    return dynamic_expressions
end

const dynamic_expressions = dyexp = Dict{String,Function}()

function IMASDD.get_expressions(::Type{Val{:onetime}})
    return onetime_expressions
end

const onetime_expressions = otexp = Dict{String,Function}()