document[:Expressions] = Symbol[]

#= =========== =#
#  expressions  #
#= =========== =#
"""
    get_expressions(::Type{Val{T}}) where {T}

This function is a catchall meant to be extended (done in IMAS.jl) with:

    IMASDD.get_expressions(::Type{Val{:dynamic}})

    IMASDD.get_expressions(::Type{Val{:onetime}})
"""
function get_expressions(::Type{Val{T}}) where {T}
    return Dict{String,Function}()
end

const expression_onetime_weakref = Dict{UInt64,WeakRef}()

"""
    ids_ancestors(@nospecialize(ids::IDS))::Dict{Symbol,Union{Missing,IDS,Int}}

Return dictionary with pointers to ancestors to an IDS
"""
function ids_ancestors(@nospecialize(ids::IDS))::Dict{Symbol,Union{Missing,IDS,Int}}
    ancestors = Dict{Symbol,Union{Missing,IDS,Int}}()
    # initialize ancestors to missing
    ddpath = collect(f2p(ids))
    pushfirst!(ddpath, "dd")
    for (k, p) in enumerate(ddpath)
        if isdigit(p[1])
            ancestors[Symbol(ddpath[k-1] * "_index")] = missing
        else
            ancestors[Symbol(p)] = missing
        end
    end
    # traverse ancestors and assign pointers
    h = ids
    k = length(ddpath)
    while typeof(h) <: Union{IDS,IDSvector}
        if typeof(h) <: IDS
            path = @view ddpath[1:k]
            if !isdigit(path[end][1])
                ancestors[Symbol(path[end])] = h
            else
                ancestors[Symbol(path[end-1])] = h
                n = parse(Int, path[end])
                if n != 0
                    ancestors[Symbol(path[end-1] * "_index")] = n
                end
            end
        end
        h = getfield(h, :_parent).value
        k -= 1
    end
    # for anc in keys(ancestors)
    #     println("$anc => $(typeof(ancestors[anc]))")
    # end
    return ancestors
end

"""
    exec_expression_with_ancestor_args(@nospecialize(ids::IDS), field::Symbol, func::Function)

Execute a function passing the IDS stack as arguments to the function

# Arguments

  - `ids::IDS`: IDS structure
  - `field::Symbol`: Field in the `ids` that is being called
  - `func::Function`: Function to be executed should accept in inputs the list of symbols that are traversed to get to the `ids`

# Example `func`

    function pressure(x; dd, core_profiles, profiles_1d, profiles_1d_index, electrons)
        return electrons.temperature.*electrons.density * 1.60218e-19
    end

# Other example of valid `func`, now using argument splatting

    function pressure(x; electrons, _...)
        return electrons.temperature.*electrons.density * 1.60218e-19
    end
"""
function exec_expression_with_ancestor_args(@nospecialize(ids::IDS), field::Symbol, func::Function)
    in_expression = getfield(ids, :_in_expression)
    if field ∈ in_expression
        return IMASexpressionRecursion(ids, field)
    else
        push!(in_expression, field)

        coords = coordinates(ids, field)
        if !all(coords.fills)
            return IMASbadExpression(ids, field, "Missing coordinates $(coords.names)")

        else
            # find ancestors to this ids
            ancestors = ids_ancestors(ids)
            # execute and in all cases pop the call_stack
            # also check that the return value matches IMAS definition
            tp = typeof(getfield(ids, field)) # fix this 
            value = try
                func(coords.values...; ancestors...)
            catch e
                if typeof(e) <: IMASexpressionRecursion
                    e
                else
                    # we change the type of the error so that it's clear that it comes from an expression, and where it happens
                    IMASbadExpression(ids, field, sprint(showerror, e, catch_backtrace()))
                end
            end
            if !isempty(in_expression)
                @assert pop!(in_expression) === field
            end
            return value
        end
    end
end

"""
    getexpr(@nospecialize(ids::IDS), field::Symbol)

Returns expression function if present or missing

NOTE: Does not evaluate expressions
"""
function getexpr(@nospecialize(ids::IDS), field::Symbol)
    if getfield(ids, :_frozen)
        # frozen IDSs have no expressions
        return missing
    end

    uloc = ulocation(ids, field)

    onetime_expressions = get_expressions(Val{:onetime})
    if uloc ∈ keys(onetime_expressions)
        # onetime expression
        return onetime_expressions[uloc]
    end

    dynamic_expressions = get_expressions(Val{:dynamic})
    if uloc ∈ keys(dynamic_expressions)
        # dynamic expression
        return dynamic_expressions[uloc]
    end

    # missing data and no available expression
    return missing
end

"""
    hasexpr(@nospecialize(ids::IDS), field::Symbol)::Bool

Returns true if the ids field has an expression

NOTE: Does not evaluate expressions
"""
function hasexpr(@nospecialize(ids::IDS), field::Symbol)::Bool
    if getfield(ids, :_frozen)
        # frozen IDSs have no expressions
        return false
    end

    uloc = ulocation(ids, field)
    for expr_type in (Val{:onetime}, Val{:dynamic})
        if uloc ∈ keys(get_expressions(expr_type))
            return true
        end
    end

    return false
end

"""
    hasexpr(@nospecialize(ids::IDS))::Bool

Returns true if the ids field has an expression at any depth below it

NOTE: Does not evaluate expressions
"""
function hasexpr(@nospecialize(ids::IDS))::Bool
    if getfield(ids, :_frozen)
        # frozen IDSs have no expressions
        return false
    end

    uloc = ulocation(ids)
    for expr_type in (Val{:onetime}, Val{:dynamic})
        if any(contains(expr, uloc) for expr in keys(get_expressions(expr_type)))
            return true
        end
    end

    return false
end

export hasexpr
push!(document[:Expressions], :hasexpr)

"""
    hasdata(@nospecialize(ids::IDS), field::Symbol; refs::Bool=true)::Bool

Returns true if the ids field has data, not an expression
"""
function hasdata(@nospecialize(ids::IDS), field::Symbol; refs::Bool=true)::Bool
    if field ∈ getfield(ids, :_filled)
        return true
    elseif refs
        h = ref(ids)
        while h !== nothing
            if field ∈ getfield(h, :_filled)
                return true
            else
                h = ref(h)
            end
        end
    end
    return false
end

"""
    hasdata(@nospecialize(ids::IDS); refs::Bool=true)::Bool

Returns true if any of the IDS fields downstream have data
"""
function hasdata(@nospecialize(ids::IDS); refs::Bool=true)::Bool
    if !isempty(getfield(ids, :_filled))
        return true
    elseif refs
        h = ref(ids)
        while h !== nothing
            if !isempty(getfield(h, :_filled))
                return true
            else
                h = ref(h)
            end
        end
    end
    return false
end

export hasdata
push!(document[:Expressions], :hasdata)

"""
    data_and_expression_ulocations(ids::IDS)

returns a set of ulocations that have data, and a set of ulocations that hare expressions
"""
function data_and_expression_ulocations(ids::IDS)
    data_ulocations = OrderedCollections.OrderedSet{String}()
    expr_ulocations = OrderedCollections.OrderedSet{String}()
    for node_rep in AbstractTrees.Leaves(ids)
        ids = node_rep.ids
        field = node_rep.field
        if hasdata(ids, field)
            push!(data_ulocations, ulocation(ids, field))
        elseif hasexpr(ids, field)
            push!(expr_ulocations, ulocation(ids, field))
        end
    end
    return (data_ulocations=data_ulocations, expr_ulocations=expr_ulocations)
end

export data_and_expression_ulocations
push!(document[:Expressions], :data_and_expression_ulocations)

#= ====== =#
#  freeze  #
#= ====== =#
"""
    freeze(@nospecialize(ids::T))::T where {T<:Union{IDS,IDSvector}}

Return a new IDS with all expressions evaluated (data is copied)

NOTE: Expressions that fail will be `missing`
"""
function freeze(@nospecialize(ids::T))::T where {T<:Union{IDS,IDSvector}}
    tmp = deepcopy(ids)
    freeze!(ids, tmp)
    return tmp
end

export freeze
push!(document[:Expressions], :freeze)

"""
    freeze!(@nospecialize(ids::T))::T where {T<:Union{IDS,IDSvector}}

Evaluates all expressions in place

NOTE: Expressions that fail will be `missing`
"""
function freeze!(@nospecialize(ids::T))::T where {T<:Union{IDS,IDSvector}}
    return freeze!(ids, ids)
end

function freeze!(@nospecialize(ids::T), @nospecialize(frozen_ids::T))::T where {T<:IDS}
    for field in keys_no_missing(ids)
        value = getraw(ids, field)
        if typeof(value) <: Union{IDS,IDSvector} # structures and arrays of structures
            freeze!(value, getfield(frozen_ids, field))
        elseif typeof(value) <: Function
            value = exec_expression_with_ancestor_args(ids, field, value)
            if typeof(value) <: Exception
                # println(value)
            else
                setproperty!(frozen_ids, field, value)
            end
        end
    end
    setfield!(frozen_ids, :_frozen, true)
    return frozen_ids
end

function freeze!(@nospecialize(ids::T), @nospecialize(frozen_ids::T))::T where {T<:IDSvector}
    for k in 1:length(ids)
        freeze!(ids[k], frozen_ids[k])
    end
    return frozen_ids
end

function freeze!(@nospecialize(ids::T), field::Symbol) where {T<:IDS}
    value = getproperty(ids, field, missing)
    if value !== missing
        setproperty!(ids, field, value)
    end
    return value
end

export freeze!
push!(document[:Expressions], :freeze!)