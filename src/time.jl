using InteractiveUtils: subtypes
document[:Time] = Symbol[]

function Base.getindex(@nospecialize(ids::IDSvector{T}))::T where {T<:IDSvectorTimeElement}
    return getindex(ids, global_time(ids))
end

function Base.getindex(@nospecialize(ids::IDSvector{T}), time0::Float64)::T where {T<:IDSvectorTimeElement}
    if isempty(ids)
        ids[1]
    end
    time = time_array_local(ids)
    i, perfect_match = causal_time_index(time, time0)
    return ids._value[i]
end

"""
    Base.setindex!(@nospecialize(ids::IDSvector{T}), @nospecialize(v::T), time0::Float64)::T where {T<:IDSvectorTimeElement}

Set element of a time dependent IDSvector array

NOTE: this automatically sets the time of the element being set as well as of the time array in the parent IDS
"""
function Base.setindex!(@nospecialize(ids::IDSvector{T}), @nospecialize(v::T), time0::Float64)::T where {T<:IDSvectorTimeElement}
    time = time_array_local(ids)
    i, perfect_match = causal_time_index(time, time0)
    if !perfect_match
        error("Cannot insert data at time $time0 that does not match any existing time")
    end

    unifm_time = time_array_parent(ids)
    if isempty(unifm_time) || time0 != unifm_time[end]
        push!(unifm_time, time0)
    end

    v.time = time0 # note IDSvectorTimeElement should always have a .time field
    ids._value[i] = v
    setfield!(v, :_parent, WeakRef(ids))

    return v
end

"""
    Base.push!(@nospecialize(ids::IDSvector{T}), @nospecialize(v::T))::IDSvector{T} where {T<:IDSvectorTimeElement} 

Push to a time dependent IDSvector array

NOTE: this automatically sets the time of the element being pushed as well as of the time array in the parent IDS
"""
function Base.push!(@nospecialize(ids::IDSvector{T}), @nospecialize(v::T), time0::Float64)::IDSvector{T} where {T<:IDSvectorTimeElement}
    time = time_array_local(ids)
    if time0 <= time[end]
        error("Cannot push! data at $time0 [s] at a time earlier or equal to $(time[end]) [s]")
    end

    unifm_time = time_array_parent(ids)
    if isempty(unifm_time) || time0 != unifm_time[end]
        push!(unifm_time, time0)
    end

    v.time = time0 # note IDSvectorTimeElement should always have a .time field
    push!(ids, v)
    setfield!(v, :_parent, WeakRef(ids))

    return ids
end

"""
    causal_time_index(time::Vector{T}, time0::T) where {T<:Float64} 

Returns the `time` array index that is closest to `time0` and satisfies causality.

This function also returns a boolean indicating if the `time0` is exactly contained in `time`.
"""
function causal_time_index(time::Vector{T}, time0::T) where {T<:Float64}
    i = argmin(abs.(time .- time0))
    if time[i] == time0
        perfect_match = true
    elseif time[i] < time0
        perfect_match = false
    elseif i == 1
        if length(time) == 1
            error("Could not find causal time for time0=$time0. Available time is only [$(time[1])]")
        else
            error("Could not find causal time for time0=$time0. Available time range is [$(time[1])...$(time[end])]")
        end
    else
        i = i - 1
        perfect_match = false
    end
    return i, perfect_match
end

"""
    time_array_parent(@nospecialize(ids::IDS))

Traverse IDS hierarchy upstream and returns the relevant :Time vector
"""
function time_array_parent(@nospecialize(ids::IDS))
    if :time ∈ fieldnames(typeof(ids)) && typeof(getfield(ids, :time)) <: Vector{Float64}
        if ismissing(ids, :time)
            ids.time = Float64[]
        end
        return ids.time
    else
        return time_array_parent(parent(ids))
    end
end

function time_array_parent(@nospecialize(ids::IDSvector))
    return time_array_parent(parent(ids))
end

"""
    time_array_local(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement})) 

Returns the local time
"""
function time_array_local(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}))
    return Float64[v.time for v in ids]
end

"""
    global_time(ids::Union{IDS,IDSvector})

Get the dd.global_time of a given IDS

If top-level dd cannot be reached then returns `Inf`
"""
function global_time(@nospecialize(ids::Union{IDS,IDSvector}))::Float64
    return global_time(top_dd(ids))
end

function global_time(::Nothing)::Float64
    return Inf
end

function global_time(dd::DD)::Float64
    return dd.global_time
end

function global_time(dd::DD, time0::Float64)::Float64
    return dd.global_time = time0
end

export global_time
push!(document[:Time], :global_time)

"""
    set_time_array(@nospecialize(ids::IDS), field::Symbol, value)

Set value of a time-dependent array at the dd.global_time
"""
function set_time_array(@nospecialize(ids::IDS), field::Symbol, value)
    return set_time_array(ids, field, global_time(ids), value)
end

"""
    set_time_array(@nospecialize(ids::IDS), field::Symbol, time0::Float64, value)

Set value of a time-dependent array at time0
"""
function set_time_array(@nospecialize(ids::IDS), field::Symbol, time0::Float64, value)
    time = time_array_parent(ids)
    # no time information
    if length(time) == 0
        push!(time, time0)
        if field !== :time
            setproperty!(ids, field, [value])
        end
    else
        i, perfect_match = causal_time_index(time, time0)
        if perfect_match
            # perfect match --> overwrite
            if field !== :Time
                if ismissing(ids, field) || isempty(getproperty(ids, field))
                    setproperty!(ids, field, vcat([NaN for k in 1:i-1], value))
                else
                    last_value = getproperty(ids, field)
                    # if destination array needs a type upgrade, then go for it
                    if !(typeof(value) <: eltype(last_value))
                        last_value = typeof(value)[v for v in last_value]
                        setproperty!(ids, field, last_value)
                    end
                    if length(last_value) < i
                        reps = i - length(last_value) - 1
                        append!(last_value, vcat([last_value[end] for k in 1:reps], value))
                    else
                        last_value[i] = value
                    end
                end
            end
        elseif time0 > maximum(time)
            # next timeslice --> append
            push!(time, time0)
            if field !== :time
                if ismissing(ids, field) || isempty(getproperty(ids, field))
                    setproperty!(ids, field, vcat([NaN for k in 1:length(time)-1], value))
                else
                    last_value = getproperty(ids, field)
                    reps = length(time) - length(last_value) - 1
                    append!(last_value, vcat([last_value[end] for k in 1:reps], value))
                end
            end
        else
            error("Could not set time array information for `$(location(ids, field))` at time $time0 (NOTE: dd.global_time=$(top_dd(ids).global_time)?)")
        end
    end
    i, perfect_match = causal_time_index(time, time0)
    if access_log.enabled
        push!(access_log.write, ulocation(ids, field)) # make sure that ids.field appears in the `write` access_log
    end
    return getproperty(ids, field)[i]
end

export set_time_array
push!(document[:Time], :set_time_array)

"""
    get_time_array(ids, field)

Get data from a time-dependent array at the dd.global_time
"""
function get_time_array(@nospecialize(ids::IDS), field::Symbol)
    T = eltype(ids)
    return get_time_array(ids, field, global_time(ids))::T
end

"""
    get_time_array(@nospecialize(ids::IDS), field::Symbol, time0::Float64, scheme::Symbol=:linear)

Get data from time dependent array

NOTE: logic for @ddtime array handling:

  - `scheme` (i) interpolation between array bounds
  - constant (c) extrapolation within bounds of time array
  - error (e) when time0 is before minimum(time)

For example:

    time:   -oooo-
    data:   -o-o--
    ddtime: eiiicc
"""
function get_time_array(@nospecialize(ids::IDS), field::Symbol, time0::Float64, scheme::Symbol=:linear)
    T = eltype(ids)
    array = getproperty(ids, field)
    time = time_array_parent(ids)
    if length(time) < length(array)
        error("length(time)=$(length(time)) must be greater than length($(location(ids, field)))=$(length(array))")
    elseif time0 < time[1]
        error("Asking for `$(location(ids, field))` at $time0 [s], before minimum time $(time[1]) [s]")
    end
    i, perfect_match = causal_time_index(time, time0)
    if i <= length(array) && perfect_match
        return array[i]::T
    elseif i > length(array)
        return array[end]::T
    elseif time[i] == -Inf
        return array[i]::T
    else
        return get_time_array(time, array, [time0], scheme)[1]::T
    end
end

"""
    get_time_array(@nospecialize(ids::IDS), field::Symbol, time0::Vector{Float64}, scheme::Symbol=:linear)
"""
function get_time_array(@nospecialize(ids::IDS), field::Symbol, time0::Vector{Float64}, scheme::Symbol=:linear)
    T = eltype(ids)
    time = time_array_parent(ids)
    array = getproperty(ids, field)
    if length(time) < length(array)
        error("length(time)=$(length(time)) must be greater than length($(location(ids, field)))=$(length(array))")
    elseif minimum(time0) < time[1]
        error("Asking for `$(location(ids, field))` at $(minimum(time0)) [s], before minimum time $(time[1]) [s]")
    end
    return get_time_array(time, array, time0, scheme)::Vector{T}
end

function get_time_array(time::Vector{Float64}, array::Vector{T}, time0::Vector{Float64}, scheme::Symbol) where {T<:Real}
    n = length(array)
    itp = @views interp1d_itp(time[1:n], array[1:n], scheme)
    return extrap1d(itp; first=:flat, last=:flat).(time0)::Vector{T}
end

function get_time_array(time::Vector{Float64}, matrix::Matrix{T}, time0::Vector{Float64}, scheme::Symbol) where {T<:Real}
    if size(matrix)[1] == 1
        array = matrix[1, :]
        return get_time_array(time, array, time0, scheme)
    else
        error("get_time_array for Matrix is not fully implemented")
    end
end

export get_time_array
push!(document[:Time], :get_time_array)

"""
    @ddtime( X.Y )

Get data from time dependent array. Equivalent to:

    get_time_array(X, :Y)

and

    @ddtime( X.Y = V)

Set data in a time dependent array. Equivalent to:
    
    set_time_array(X, :Y, V)
"""
macro ddtime(ex)
    return _ddtime(ex)
end

function _ddtime(ex)
    value = gensym()
    ids = gensym()
    field = gensym()
    if ex.head == :(=)
        quote
            $value = $(esc(ex.args[2]))
            $ids = $(esc(ex.args[1].args[1]))
            $field = $(esc(ex.args[1].args[2]))
            set_time_array($ids, $field, $value)
        end
    else
        quote
            $ids = $(esc(ex.args[1]))
            $field = $(esc(ex.args[2]))
            get_time_array($ids, $field)
        end
    end
end

export @ddtime
push!(document[:Time], Symbol("@ddtime"))

"""
    last_time(dd::DD)::Float64

Returns the last time referenced in all the IDSs `dd.XXX.time` vectors (including `dd.global_time`)
"""
function last_time(dd::DD)::Float64
    time = dd.global_time
    for ids in values(dd)
        if hasfield(typeof(ids), :time) && !ismissing(ids, :time) && !isempty(ids.time)
            times = filter(t -> !isinf(t), ids.time)
            if !isempty(times)
                time = max(time, maximum(times))
            end
        end
    end
    return time
end

export last_time
push!(document[:Time], :last_time)

"""
    last_global_time(dd::DD)::Float64

Returns the last time referenced in all the IDSs `dd.XXX.time` vectors (including `dd.global_time`)
"""
function last_global_time(dd::DD)::Float64
    dd.global_time = last_time(dd)
    return dd.global_time
end

export last_global_time
push!(document[:Time], :last_global_time)

"""
    new_timeslice!(ids::IDS, time0::Float64)

Recursively appends a lazycopy at time `time0` of the last time-slice of all time-dependent array structures under a given ids
"""
function new_timeslice!(@nospecialize(ids::IDS), time0::Float64)
    for time_element in subtypes(IDSvectorTimeElement)
        time_path = i2p(fs2u(time_element))
        ok = true
        for path in f2p(ids)
            if path in time_path
                popat!(time_path, 1)
            else
                ok = false
                continue
            end
        end
        if !ok
            continue
        end
        new_timeslice!(ids, [Symbol(path) for path in time_path], time0)
    end
end

function new_timeslice!(@nospecialize(ids::IDS), path::AbstractVector{Symbol}, time0::Float64)
    if path[1] in keys(ids)
        new_timeslice!(getfield(ids, path[1]), @views(path[2:end]), time0)
    end
end

function new_timeslice!(@nospecialize(ids::IDSvector), path::AbstractVector{Symbol}, time0::Float64)
    for k in 1:length(ids)
        new_timeslice!(ids[k], @views(path[2:end]), time0)
    end
end

function new_timeslice!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), path::AbstractVector{Symbol}, time0::Float64)
    if !isempty(ids)
        push!(ids, lazycopy(ids[end]), time0)
    end
end

export new_timeslice!
push!(document[:Time], :new_timeslice!)

"""
    retime!(ids::IDS, time0::Float64)

Recursively change the time of the last time-slices or last time-depedent vector elements in a IDS
"""
function retime!(@nospecialize(ids::IDS), time0::Float64)
    for field in keys(ids)
        if hasdata(ids, field)
            value = getproperty(ids, field)
        else
            continue
        end
        if field == :time
            if typeof(value) <: Float64
                setproperty!(ids, field, time0)
            elseif typeof(value) <: Vector{Float64}
                value[end] = time0
            end
        elseif typeof(value) <: Union{IDS,IDSvector}
            retime!(value, time0)
        end
    end
end

function retime!(@nospecialize(ids::IDSvector), time0::Float64)
    for k in 1:length(ids)
        retime!(ids[k], time0)
    end
end

function retime!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), time0::Float64)
    if !isempty(ids)
        retime!(ids[end], time0)
    end
end

export retime!
push!(document[:Time], :retime!)