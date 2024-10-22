using InteractiveUtils: subtypes
document[:Time] = Symbol[]

function Base.getindex(@nospecialize(ids::IDSvector{T})) where {T<:IDSvectorTimeElement}
    return getindex(ids, global_time(ids))
end

function Base.getindex(@nospecialize(ids::IDSvector{T}), time0::Float64) where {T<:IDSvectorTimeElement}
    if isempty(ids)
        ids[1] # this is to throw a out of bounds getindex error
    end
    time = time_array_local(ids)
    i, perfect_match = try
        causal_time_index(time, time0)
    catch e
        throw("$(location(ids)): $(e)")
    end
    return ids._value[i]
end

"""
    Base.setindex!(@nospecialize(ids::IDSvector{T}), @nospecialize(v::T), time0::Float64) where {T<:IDSvectorTimeElement}

Set element of a time dependent IDSvector array

NOTE: this automatically sets the time of the element being set as well as of the time array in the parent IDS
"""
function Base.setindex!(@nospecialize(ids::IDSvector{T}), @nospecialize(v::T), time0::Float64) where {T<:IDSvectorTimeElement}
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
    Base.push!(@nospecialize(ids::IDSvector{T}), @nospecialize(v::T)) where {T<:IDSvectorTimeElement} 

Push to a time dependent IDSvector array

NOTE: this automatically sets the time of the element being pushed as well as of the time array in the parent IDS
"""
function Base.push!(@nospecialize(ids::IDSvector{T}), @nospecialize(v::T), time0::Float64) where {T<:IDSvectorTimeElement}
    if time0 <= ids[end].time
        error("Cannot push! data at $time0 [s] at a time earlier or equal to $(ids[end].time) [s]")
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
    causal_time_index(time::Union{Base.Generator,AbstractVector{T}}, time0::T) where {T<:Float64}

Returns the `time` index that is closest to `time0` and satisfies causality.

This function also returns a boolean indicating if the `time0` is exactly contained in `time`.
"""
function causal_time_index(time::Union{Base.Generator,AbstractVector{T}}, time0::T) where {T<:Float64}
    len = 0
    start_time = NaN
    end_time = NaN

    for (k, t) in enumerate(time)
        if k == 1
            start_time = t
        end
        if t == time0
            return k, true
        elseif t > time0
            if k == 1
                error("Could not find causal time for time0=$time0. Available time is only [$(start_time)]")
            end
            return k - 1, false
        end
        end_time = t
        len = k
    end

    if time0 < start_time
        if start_time == end_time
            error("Could not find causal time for time0=$time0. Available time is only [$(start_time)]")
        else
            error("Could not find causal time for time0=$time0. Available time range is [$(start_time)...$(end_time)]")
        end
    end

    return len, false
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

Returns a generator pointing to the ids[].time Float64 values
"""
function time_array_local(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}))
    return (v.time for v in ids)
end

"""
    global_time(ids::Union{IDS,IDSvector})

Get the dd.global_time of a given IDS

If top-level dd cannot be reached then returns `Inf`
"""
function global_time(@nospecialize(ids::Union{IDS,IDSvector}))
    return global_time(top_dd(ids))
end

function global_time(::Nothing)
    return Inf
end

function global_time(dd::DD)
    return getfield(dd, :global_time)
end

"""
    global_time(ids::Union{IDS,IDSvector}, time0::Float64)

Set the dd.global_time of a given IDS
"""
function global_time(@nospecialize(ids::Union{IDS,IDSvector}), time0::Float64)
    return setfield!(top_dd(ids), :global_time, time0)
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
    get_time_array(@nospecialize(ids::IDS{T}), field::Symbol, scheme::Symbol=:linear) where {T<:Real}

Get data from a time-dependent array at the dd.global_time
"""
function get_time_array(@nospecialize(ids::IDS{T}), field::Symbol, scheme::Symbol=:linear) where {T<:Real}
    results = get_time_array(ids, field, global_time(ids), scheme)
    tp = typeof(getfield(ids, field))
    if tp <: Vector{T}
        return results::T
    else
        return results
    end
end

"""
    get_time_array(ids::IDS, field::Symbol, time0::Float64, scheme::Symbol=:linear)

Get data from time dependent array

NOTE: logic for @ddtime array handling:

  - interpolation (i) `scheme` between array bounds
  - constant (c) extrapolation within bounds of time array
  - error (e) when time0 is before minimum(time)

For example:

    time:   -oooo-
    data:   -o-o--
    ddtime: eiiicc
"""
function get_time_array(ids::IDS, field::Symbol, time0::Float64, scheme::Symbol=:linear)
    time_coordinate_index = time_coordinate(ids, field; error_if_not_time_dependent=false)
    if time_coordinate_index == 0
        return getproperty(ids, field)
    else
        result = dropdims_view(get_time_array(ids, field, [time0], scheme); dims=time_coordinate_index)
        return isa(result, Array) && ndims(result) == 0 ? result[] : result
    end
end

function dropdims_view(arr; dims::Int)
    indices = ntuple(i -> (i == dims ? 1 : Colon()), ndims(arr))
    result = @view arr[indices...]
    return ndims(result) == 0 ? result[] : result
end

function get_time_array(@nospecialize(ids::IDS{T}), field::Symbol, time0::Vector{Float64}, scheme::Symbol=:linear) where {T<:Real}
    time_coordinate_index = time_coordinate(ids, field; error_if_not_time_dependent=true)
    time = time_array_parent(ids)
    if minimum(time0) < time[1]
        error("Asking for `$(location(ids, field))` at $time0 [s], before minimum time $(time[1]) [s]")
    end
    array = getproperty(ids, field)
    array_time_length = size(array)[time_coordinate_index]
    if length(time) < array_time_length
        error("length(time)=$(length(time)) must be greater than size($(location(ids, field)))[$time_coordinate_index]=$(array_time_length)")
    end
    tp = eltype(getfield(ids, field))
    return get_time_array(time, array, time0, scheme, time_coordinate_index)::Array{tp}
end

function get_time_array(time::Vector{Float64}, vector::AbstractVector{T}, time0::Vector{Float64}, scheme::Symbol, time_coordinate_index::Int=1) where {T<:Real}
    @assert time_coordinate_index == 1
    n = length(vector)
    itp = @views interp1d_itp(time[1:n], vector[1:n], scheme)
    return extrap1d(itp; first=:flat, last=:flat).(time0)::Array{T}
end

function get_time_array(time::Vector{Float64}, array::Array{T}, time0::Vector{Float64}, scheme::Symbol, time_coordinate_index::Int) where {T<:Real}
    # Permute dimensions to bring the time dimension first
    perm = [time_coordinate_index; setdiff(1:ndims(array), time_coordinate_index)]
    array_permuted = PermutedDimsArray(array, perm)

    # Reshape to 2D (time x other dimensions)
    array_reshaped = reshape(array_permuted, size(array_permuted, 1), :)
    n_cols = size(array_reshaped, 2)

    # Preallocate result array
    result = similar(array_reshaped, length(time0), n_cols)

    # Interpolate each column
    @inbounds @simd for col in 1:n_cols
        vector = view(array_reshaped, :, col)
        # Use in-place interpolation if possible
        result[:, col] = get_time_array(time, vector, time0, scheme, 1)
    end

    # Reshape back to original dimensions
    result_reshaped = reshape(result, (length(time0), size(array_permuted)[2:end]...))
    # Permute back to original dimension order
    inv_perm = invperm(perm)
    return permutedims(result_reshaped, inv_perm)
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
    last_time(dd::DD)

Returns the last time referenced in all the IDSs `dd.XXX.time` vectors (including `dd.global_time`)
"""
function last_time(dd::DD)
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
    last_global_time(dd::DD)

Returns the last time referenced in all the IDSs `dd.XXX.time` vectors (including `dd.global_time`)
"""
function last_global_time(dd::DD)
    dd.global_time = last_time(dd)
    return dd.global_time
end

export last_global_time
push!(document[:Time], :last_global_time)

const subtypes_IDSvectorTimeElement = subtypes(IDSvectorTimeElement)

"""
    new_timeslice!(ids::IDS, time0::Float64)

Recursively appends a deepcopy at time `time0` of the last time-slice of all time-dependent array structures under a given ids
"""
function new_timeslice!(@nospecialize(ids::IDS), time0::Float64)
    keys_ids = keys(ids)
    f2p_ids = f2p(ids)
    for time_element in subtypes_IDSvectorTimeElement
        time_path = i2p(fs2u(time_element))
        ok = true
        for path in f2p_ids
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
        time_path = [Symbol(path) for path in time_path]
        if time_path[1] in keys_ids
            new_timeslice!(ids, time_path, time0)
        end
    end
end

function new_timeslice!(@nospecialize(ids::IDS), path::AbstractVector{Symbol}, time0::Float64)
    return new_timeslice!(getfield(ids, path[1]), @views(path[2:end]), time0)
end

function new_timeslice!(@nospecialize(ids::IDSvector), path::AbstractVector{Symbol}, time0::Float64)
    for k in 1:length(ids)
        new_timeslice!(ids[k], @views(path[2:end]), time0)
    end
end

function new_timeslice!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), path::AbstractVector{Symbol}, time0::Float64)
    if !isempty(ids)
        tmp = fill!(typeof(ids[end])(), ids[end])
        push!(ids, tmp, time0)
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

function retime!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), time0::Float64)
    if !isempty(ids)
        retime!(ids[end], time0)
    end
end

function retime!(@nospecialize(ids::IDSvector), time0::Float64)
    for k in 1:length(ids)
        retime!(ids[k], time0)
    end
end

export retime!
push!(document[:Time], :retime!)

"""
    get_timeslice(@nospecialize(ids::IDS), time0::Float64=global_time(ids), scheme::Symbol=:linear; slice_pulse_schedule::Bool=true)

Returns data at the given `time0` (by default at the global_time)

Data is selected from time dependent arrays of structures using closest causal time point.

Data is selected from time dependent arrays using these possible schemes `[:constant, :linear, :quadratic, :cubic, :pchip, :lagrange]`
"""
function get_timeslice(@nospecialize(ids::IDS), time0::Float64=global_time(ids), scheme::Symbol=:linear; slice_pulse_schedule::Bool=false)
    ids0 = typeof(ids)()
    return get_timeslice!(ids, ids0, time0, scheme; slice_pulse_schedule)
end

function get_timeslice!(
    @nospecialize(ids::T1),
    @nospecialize(ids0::T2),
    time0::Float64=global_time(ids),
    scheme::Symbol=:linear;
    slice_pulse_schedule::Bool=false) where {T1<:IDS,T2<:IDS}
    if typeof(ids0) <: DD
        ids0.global_time = time0
    end
    for field in keys(ids)
        if hasdata(ids, field)
            value = getproperty(ids, field)
        else
            continue
        end
        if field == :time
            if typeof(value) <: Float64
                setproperty!(ids0, field, time0; error_on_missing_coordinates=false)
            elseif typeof(value) <: Vector{Float64}
                setproperty!(ids0, field, [time0]; error_on_missing_coordinates=false)
            end
        elseif typeof(value) <: IMASdd.pulse_schedule && !slice_pulse_schedule
            fill!(getproperty(ids0, field), deepcopy(value))
        elseif typeof(value) <: Union{IDS,IDSvector}
            get_timeslice!(value, getfield(ids0, field), time0, scheme; slice_pulse_schedule)
        else
            time_coordinate_index = time_coordinate(ids, field; error_if_not_time_dependent=false)
            if time_coordinate_index == 0
                setproperty!(ids0, field, value; error_on_missing_coordinates=false)
            else
                setproperty!(ids0, field, get_time_array(ids, field, [time0], scheme); error_on_missing_coordinates=false)
            end
        end
    end
    return ids0
end

function get_timeslice!(
    @nospecialize(ids::T1),
    @nospecialize(ids0::T2),
    time0::Float64,
    scheme::Symbol;
    slice_pulse_schedule) where {T1<:IDSvector{<:IDSvectorTimeElement},T2<:IDSvector{<:IDSvectorTimeElement}}

    if !isempty(ids)
        resize!(ids0, 1)
        get_timeslice!(ids[time0], ids0[end], time0, scheme; slice_pulse_schedule)
    end
    return ids0
end

function get_timeslice!(
    @nospecialize(ids::T1),
    @nospecialize(ids0::T2),
    time0::Float64,
    scheme::Symbol;
    slice_pulse_schedule) where {T1<:IDSvector{<:IDSvectorElement},T2<:IDSvector{<:IDSvectorElement}}

    resize!(ids0, length(ids))
    for k in 1:length(ids)
        get_timeslice!(ids[k], ids0[k], time0, scheme; slice_pulse_schedule)
    end
    return ids0
end

export get_timeslice
push!(document[:Time], :get_timeslice)
