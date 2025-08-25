function IMASdd.get_expressions(::Val{:dynamic})
    return dynamic_expressions
end

const dynamic_expressions = dyexp = Dict{String,Function}()

function IMASdd.get_expressions(::Val{:onetime})
    return onetime_expressions
end

const onetime_expressions = otexp = Dict{String,Function}()