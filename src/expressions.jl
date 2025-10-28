import TimerOutputs

document[:Expressions] = Symbol[]

#= =========== =#
#  expressions  #
#= =========== =#
const __dynamic_expressions = Ref{Dict{String, Function}}(Dict{String, Function}())
const __onetime_expressions = Ref{Dict{String, Function}}(Dict{String, Function}())

"""
    get_dynamic_expressions()

Get the dynamic expressions registered by IMAS.jl.
"""
get_dynamic_expressions() = __dynamic_expressions[]
set_dynamic_expressions(expressions) = (__dynamic_expressions[] = expressions; nothing)

"""
    get_onetime_expressions()

Get the onetime expressions registered by IMAS.jl.
"""
get_onetime_expressions() = __onetime_expressions[]
set_onetime_expressions(expressions) = (__onetime_expressions[] = expressions; nothing)

"""
    ids_ancestors(@nospecialize(ids::IDS))

Return dictionary with pointers to ancestors to an IDS
"""
function ids_ancestors(@nospecialize(ids::IDS))
    ancestors = Dict{Symbol,Union{Missing,IDS,Int}}()
    # initialize ancestors to missing
    ddpath = f2p(ids)
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
function exec_expression_with_ancestor_args(@nospecialize(ids::IDS), field::Symbol, func::Function, throw_on_missing::Bool)
    lock(getfield(ids, :_threads_lock)) do
        in_expr = in_expression(ids)
        if field ∈ in_expr
            if throw_on_missing
                throw(IMASexpressionRecursion(ids, field))
            else
                return missing
            end
        end
        push!(in_expr, field)

        # find ancestors to this ids
        ancestors = ids_ancestors(ids)

        # execute and in all cases pop the call_stack
        # also check that the return value matches IMAS definition
        tp = concrete_fieldtype_typeof(ids, field)
        value = try
            func(; ancestors...)::tp
        catch e
            if throw_on_missing
                if typeof(e) <: IMASexpressionRecursion
                    rethrow(e)
                else
                    # we change the type of the error so that it's clear that it comes from an expression, and where it happens
                    throw(IMASbadExpression(ids, field, e, catch_backtrace()))
                end
            else
                missing
            end
        end
        if !isempty(in_expr)
            @assert pop!(in_expr) === field
        end
        return value
    end
end

function exec_expression_with_ancestor_args(@nospecialize(ids::IDS), field::Symbol; throw_on_missing::Bool)
    uloc = ulocation(ids, field)
    for (onetime, expressions) in zip((true, false), (get_onetime_expressions(), get_dynamic_expressions()))
        if uloc ∈ keys(expressions)
            func = expressions[uloc]

            if onetime
                # Thread-safe onetime expression evaluation with double-checked locking
                # First check: avoid locking if already cached
                if hasdata(ids, field)
                    return true
                end

                # Use the existing threads_lock for coordination
                return lock(getfield(ids, :_threads_lock)) do
                    # Double-check: another thread might have cached it while we were waiting
                    if hasdata(ids, field)
                        return true
                    end

                    # We are the first thread to reach this point for this expression
                    # Evaluate and cache atomically within the lock
                    value = exec_expression_with_ancestor_args(ids, field, func, throw_on_missing)
                    if ismissing(value)
                        return false
                    else
                        if access_log.enabled
                            push!(access_log.expr, uloc)
                        end
                        # Cache the result atomically within the lock
                        setproperty!(ids, field, value; error_on_missing_coordinates=false)
                        return true
                    end
                end
            else
                # Dynamic expressions (not cached) - original behavior
                value = exec_expression_with_ancestor_args(ids, field, func, throw_on_missing)
                if ismissing(value)
                    return false
                else
                    if access_log.enabled
                        push!(access_log.expr, uloc)
                    end
                    setfield!(ids, field, value)
                    return true
                end
            end
        end
    end
    if throw_on_missing
        throw(IMASmissingDataException(ids, field))
    else
        return false
    end
end

"""
    in_expression(@nospecialize(ids::IDS))

Returns thread-safe `in_expression` for current thread
"""
function in_expression(@nospecialize(ids::IDS))
    _in_expression = getfield(ids, :_in_expression)
    t_id = Threads.threadid()
    # create stack for individual threads if not there already
    # ThreadSafeDict handles the locking internally, but we still need to check and create atomically
    if t_id ∉ keys(_in_expression)
        # Use get! for atomic check-and-set operation
        get!(_in_expression, t_id) do
            Symbol[]
        end
    end
    return _in_expression[t_id]
end

"""
    getexpr(@nospecialize(ids::IDS), field::Symbol)

Returns expression function if present or missing

NOTE: Does not evaluate expressions
"""
function getexpr(@nospecialize(ids::IDS), field::Symbol)
    if isfrozen(ids)
        # frozen IDSs have no expressions
        return missing
    end

    uloc = ulocation(ids, field)
    return @something(
        get(get_onetime_expressions(), uloc, nothing),
        get(get_dynamic_expressions(), uloc, nothing),
        missing # missing data and no available expression
    )
end

"""
isexpr(@nospecialize(ids::IDS), field::Symbol)

Returns true if the ids field is an expression

NOTE: Does not evaluate expressions
"""
function isexpr(@nospecialize(ids::IDS), field::Symbol)
    return typeof(getraw(ids, field)) <: Function
end

export isexpr
push!(document[:Expressions], :isexpr)

"""
    hasexpr(@nospecialize(ids::IDS), field::Symbol)

Returns true if the ids field has an expression.

Having an expression does not mean it --is-- an expression. For that, use `isexpr(ids, field)`

NOTE: Does not evaluate expressions
"""
function hasexpr(@nospecialize(ids::IDS), field::Symbol)
    if isfrozen(ids)
        # frozen IDSs have no expressions
        return false
    end

    uloc = ulocation(ids, field)
    return haskey(get_onetime_expressions(), uloc) ||
        haskey(get_dynamic_expressions(), uloc)
end

"""
    hasexpr(@nospecialize(ids::IDS))

Returns true if the ids field has an expression at any depth below it

NOTE: Does not evaluate expressions
"""
function hasexpr(@nospecialize(ids::IDS))
    if isfrozen(ids)
        # frozen IDSs have no expressions
        return false
    end

    uloc = ulocation(ids)
    for expressions in (get_onetime_expressions(), get_dynamic_expressions())
        if any(contains(expr, uloc) for expr in keys(expressions))
            return true
        end
    end

    return false
end

export hasexpr
push!(document[:Expressions], :hasexpr)

"""
    hasdata(@nospecialize(ids::IDS), field::Symbol)

Returns true if the ids field has data, not an expression
"""
function hasdata(@nospecialize(ids::IDS), field::Symbol)
    return getfield(getfield(ids, :_filled), field)
end

"""
    hasdata(@nospecialize(ids::IDS))

Returns true if any of the IDS fields downstream have data
"""
@inline function hasdata(@nospecialize(ids::IDS))
    filled = getfield(ids, :_filled)
    return any(getfield(filled, fitem) for fitem in fieldnames(typeof(filled)))
end

export hasdata
push!(document[:Expressions], :hasdata)

"""
    data_and_expression_ulocations(@nospecialize(ids::IDS))

returns a set of ulocations that have data, and a set of ulocations that hare expressions
"""
function data_and_expression_ulocations(@nospecialize(ids::IDS))
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
    freeze(@nospecialize(ids::T)) where {T<:Union{IDS,IDSvector}}

Return a new IDS with all expressions evaluated (data is copied)

NOTE: Expressions that fail will be `missing`
"""
function freeze(@nospecialize(ids::T)) where {T<:Union{IDS,IDSvector}}
    tmp = deepcopy(ids)
    freeze!(ids, tmp)
    return tmp
end

export freeze
push!(document[:Expressions], :freeze)

"""
    freeze!(@nospecialize(ids::T)) where {T<:Union{IDS,IDSvector}}

Evaluates all expressions in place

NOTE: Expressions that fail will be `missing`
"""
function freeze!(@nospecialize(ids::T)) where {T<:Union{IDS,IDSvector}}
    return freeze!(ids, ids)
end

function freeze!(@nospecialize(ids::T), @nospecialize(frozen_ids::T)) where {T<:IDS}
    if !isfrozen(ids)
        for field in keys_no_missing(ids)
            value = getraw(ids, field)
            if typeof(value) <: Union{IDS,IDSvector} # structures and arrays of structures
                freeze!(value, getfield(frozen_ids, field))
            elseif typeof(value) <: Function # leaves with unvaluated expressions
                throw_on_missing = false # always for freeze
                value = exec_expression_with_ancestor_args(ids, field, value, throw_on_missing)
                if !ismissing(value)
                    setproperty!(frozen_ids, field, value)
                end
            end
        end
        setfield!(frozen_ids, :_frozen, true)
    end
    return frozen_ids
end

function freeze!(@nospecialize(ids::T), @nospecialize(frozen_ids::T)) where {T<:IDSvector}
    for k in 1:length(ids)
        freeze!(ids[k], frozen_ids[k])
    end
    return frozen_ids
end

function freeze!(@nospecialize(ids::T), field::Symbol, @nospecialize(default::Any=missing)) where {T<:IDS}
    value = getproperty(ids, field, default)
    if value !== missing
        setproperty!(ids, field, value)
    end
    return value
end

export freeze!
push!(document[:Expressions], :freeze!)

"""
    refreeze!(@nospecialize(ids::T), field::Symbol, @nospecialize(default::Any=missing)) where {T<:IDS}

If the ids field has an expression associated with, it re-evaluates it in place.

If the expression fails, a default value will be assigned.
"""
function refreeze!(@nospecialize(ids::T), field::Symbol, @nospecialize(default::Any=missing)) where {T<:IDS}
    if hasexpr(ids, field)
        empty!(ids, field)
        freeze!(ids, field, default)
    else
        error(`Cannot refreeze! $(location(ids, field)), since it does not have an expression.`)
    end
end

export refreeze!
push!(document[:Expressions], :refreeze!)

"""
    unfreeze!(@nospecialize(ids::T), field::Symbol) where {T<:IDS}

If the ids field has an expression associated with it, that was frozen, turn it back into an expression.
"""
function unfreeze!(@nospecialize(ids::T), field::Symbol) where {T<:IDS}
    if hasexpr(ids, field)
        empty!(ids, field)
    else
        error(`Cannot unfreeze! $(location(ids, field)), since it does not have an expression.`)
    end
end

export unfreeze!
push!(document[:Expressions], :unfreeze!)
