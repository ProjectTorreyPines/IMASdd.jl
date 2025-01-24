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
        error("$(location(ids)): $(e)")
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
    causal_time_index(time::Union{Base.Generator,AbstractVector{T}}, time0::T; bounds_error::Bool=true) where {T<:Float64}

Returns the `time` index that is closest to `time0` and satisfies causality.

This function also returns a boolean indicating if the `time0` is exactly contained in `time`.

If `bounds_error=false` the function will not throw an erro if causal time is not available and will return time index=1 instead
"""
function causal_time_index(time::Union{Base.Generator,AbstractVector{T}}, time0::T; bounds_error::Bool=true) where {T<:Float64}
    @assert !isempty(time) "Cannot return a causal_time_index() of an empty time vector"

    len = 0
    start_time = NaN
    end_time = NaN

    for (k, t) in enumerate(time)
        if k == 1
            start_time = t
        end
        if t == time0
            return (index=k, perfect_match=true)
        elseif t > time0
            if k == 1
                if bounds_error
                    error("Could not find causal time for time0=$time0. Available time is only [$(start_time)]")
                else
                    return (index=1, perfect_match=false)
                end
            end
            return (index=k - 1, perfect_match=false)
        end
        end_time = t
        len = k
    end

    if time0 < start_time
        if bounds_error
            if start_time == end_time
                error("Could not find causal time for time0=$time0. Available time is only [$(start_time)]")
            else
                error("Could not find causal time for time0=$time0. Available time range is [$(start_time)...$(end_time)]")
            end
        else
            return (index=1, perfect_match=false)
        end
    end

    return (index=len, perfect_match=false)
end

function causal_time_index(time::Union{Base.Generator,AbstractVector{T}}, time0::T, vector::Vector; bounds_error::Bool=true) where {T<:Float64}
    i, perfect_match = causal_time_index(time, time0; bounds_error)
    return (index=min(i, length(vector)), perfect_match=perfect_match)
end

"""
    time_array_parent(@nospecialize(ids::IDS))

Traverse IDS hierarchy upstream and returns the relevant :time vector
"""
function time_array_parent(@nospecialize(ids::IDS))
    if :time ∈ fieldnames(typeof(ids)) && fieldtype_typeof(ids, :time) <: Vector{Float64}
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
    ns = NoSpecialize(ids)
    return (v.time for v in ns.ids)
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

NOTE: updates the closest causal element of an array
"""
function set_time_array(@nospecialize(ids::IDS{T}), field::Symbol, time0::Float64, value) where {T<:Real}
    time = time_array_parent(ids)
    # no time information
    if isempty(time)
        i = 1
        push!(time, time0)
        if field !== :time
            setproperty!(ids, field, [value]; error_on_missing_coordinates=false)
        end
    else
        i, perfect_match = causal_time_index(time, time0)
        if time0 <= maximum(time)
            if field !== :time
                if ismissing(ids, field) || isempty(getproperty(ids, field))
                    setproperty!(ids, field, vcat([NaN for k in 1:i-1], value); error_on_missing_coordinates=false)
                else
                    last_value = getproperty(ids, field)
                    if length(last_value) < i
                        reps = i - length(last_value) - 1
                        append!(last_value, vcat([last_value[end] for k in 1:reps], value))
                    else
                        last_value[i] = value
                    end
                end
            end
        else
            # next timeslice --> append
            push!(time, time0)
            if field !== :time
                if ismissing(ids, field) || isempty(getproperty(ids, field))
                    setproperty!(ids, field, vcat([NaN for k in 1:length(time)-1], value); error_on_missing_coordinates=false)
                else
                    last_value = getproperty(ids, field)
                    reps = length(time) - length(last_value) - 1
                    append!(last_value, vcat([last_value[end] for k in 1:reps], value))
                end
            end
            i += 1
        end
    end
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
    tp = concrete_fieldtype_typeof(ids, field)
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
    elseif fieldtype_typeof(ids, field) <: AbstractVector
        time = time_array_parent(ids)
        vector = getproperty(ids, field)
        get_time_array(time, vector, time0, scheme, time_coordinate_index)
    else
        result = dropdims_view(get_time_array(ids, field, [time0], scheme, time_coordinate_index); dims=time_coordinate_index)
        return isa(result, Array) && ndims(result) == 0 ? result[] : result
    end
end

function dropdims_view(arr; dims::Int)
    indices = ntuple(i -> (i == dims ? 1 : Colon()), ndims(arr))
    result = @view arr[indices...]
    return ndims(result) == 0 ? result[] : result
end

function get_time_array(@nospecialize(ids::IDS{T}), field::Symbol, time0::Vector{Float64}, scheme::Symbol=:linear) where {T<:Real}
    @assert !isempty(time0) "get_time_array() `time0` must have some times specified"
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
    itp = @views interp1d_itp(time[1:n], vector, scheme)
    return extrap1d(itp; first=:flat, last=:flat).(time0)
end

function get_time_array(time::Vector{Float64}, vector::AbstractVector{T}, time0::Float64, scheme::Symbol, time_coordinate_index::Int=1) where {T<:Real}
    @assert time_coordinate_index == 1
    i, perfect_match = causal_time_index(time, time0, vector)
    if perfect_match
        return vector[i]
    else
        n = length(vector)
        itp = @views interp1d_itp(time[1:n], vector, scheme)
        return extrap1d(itp; first=:flat, last=:flat).(time0)
    end
end

function get_time_array(time::Vector{Float64}, array::AbstractArray{T}, time0::Vector{Float64}, scheme::Symbol, time_coordinate_index::Int) where {T<:Real}
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
            if typeof(value) <: Vector
                value[end] = time0
            else
                setproperty!(ids, field, time0; error_on_missing_coordinates=false)
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
    setfield!(ids0, :_parent, getfield(ids, :_parent))
    return get_timeslice!(ids, ids0, time0, scheme; slice_pulse_schedule)
end

"""
    get_timeslice(el_type::Type{Z}, @nospecialize(ids::IDS), time0::Float64=global_time(ids), scheme::Symbol=:linear; slice_pulse_schedule::Bool=false) where {Z<:Real}

get_timeslice that retuns IDS of type `el_type`
"""
function get_timeslice(el_type::Type{Z}, @nospecialize(ids::IDS), time0::Float64=global_time(ids), scheme::Symbol=:linear; slice_pulse_schedule::Bool=false) where {Z<:Real}
    ids0 = Base.typename(typeof(ids)).wrapper{el_type}()
    setfield!(ids0, :_parent, getfield(ids, :_parent))
    return get_timeslice!(ids, ids0, time0, scheme; slice_pulse_schedule)
end

function get_timeslice!(
    @nospecialize(ids::IDS{T2}),
    @nospecialize(ids0::IDS{T1}),
    time0::Float64,
    scheme::Symbol;
    slice_pulse_schedule::Bool) where {T1<:Real,T2<:Real}

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
            if typeof(value) <: Vector
                setproperty!(ids0, field, [time0]; error_on_missing_coordinates=false)
            else
                setproperty!(ids0, field, time0; error_on_missing_coordinates=false)
            end
        elseif typeof(value) <: IMASdd.pulse_schedule && !slice_pulse_schedule
            fill!(getproperty(ids0, field), value)
        elseif typeof(value) <: Union{IDS,IDSvector}
            get_timeslice!(value, getfield(ids0, field), time0, scheme; slice_pulse_schedule)
        else
            time_coordinate_index = time_coordinate(ids, field; error_if_not_time_dependent=false)
            if time_coordinate_index > 0
                value = get_time_array(ids, field, [time0], scheme)
            end
            if eltype(value) <: T2
                if eltype(value) <: T1
                    setproperty!(ids0, field, value; error_on_missing_coordinates=false)
                else
                    setproperty!(ids0, field, T1.(value); error_on_missing_coordinates=false)
                end
            else
                setproperty!(ids0, field, value; error_on_missing_coordinates=false)
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
    slice_pulse_schedule::Bool) where {T1<:IDSvector{<:IDSvectorTimeElement},T2<:IDSvector{<:IDSvectorTimeElement}}

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
    slice_pulse_schedule::Bool) where {T1<:IDSvector{<:IDSvectorElement},T2<:IDSvector{<:IDSvectorElement}}

    resize!(ids0, length(ids))
    for k in 1:length(ids)
        get_timeslice!(ids[k], ids0[k], time0, scheme; slice_pulse_schedule)
    end

    return ids0
end

export get_timeslice
push!(document[:Time], :get_timeslice)

"""
    trim_time!(@nospecialize(ids::IDS), time_range::Tuple{Float64,Float64}; trim_pulse_schedule::Bool=false)

Recursively remove all time dependent data tha occurs outside of `time_range`
"""
function trim_time!(@nospecialize(ids::IDS), time_range::Tuple{Float64,Float64}; trim_pulse_schedule::Bool=false)
    @assert time_range[end] > time_range[1]
    if time_range == (-Inf, Inf)
        return ids
    end

    # trim time dependent IDSvector, and time dependent data arrays
    for field in keys(ids)
        if hasdata(ids, field)
            value = getproperty(ids, field)
        else
            continue
        end
        if typeof(value) <: IDS && (!(typeof(value) <: pulse_schedule) || trim_pulse_schedule)
            trim_time!(value, time_range; trim_pulse_schedule)
        elseif typeof(value) <: IDSvector{<:IDSvectorTimeElement}
            if isempty(value)
                # pass
            else
                for time in reverse!([subids.time for subids in value])
                    if time > time_range[end]
                        pop!(value)
                    end
                end
                for time in [subids.time for subids in value]
                    if time < time_range[1]
                        popfirst!(value)
                    end
                end
                if isempty(value)
                    @warn "$(location(ids, field)) was emptied since time=[$(times[1])...$(times[end])] and time_range=$(time_range)"
                end
            end
        elseif typeof(value) <: IDSvector
            for sub_ids in value
                trim_time!(sub_ids, time_range; trim_pulse_schedule)
            end
        elseif field == :time
            # pass
        elseif typeof(value) <: Array
            time_coordinate_index = time_coordinate(ids, field; error_if_not_time_dependent=false)
            if time_coordinate_index > 0
                times = coordinates(ids, field).values[time_coordinate_index]
                if !isempty(times)
                    if times[1] > time_range[end] || times[end] < time_range[1]
                        @warn "$(location(ids, field)) was emptied since time=[$(times[1])...$(times[end])] and time_range=$(time_range)"
                        empty!(ids, field)
                    else
                        index = (times .>= time_range[1]) .&& (times .<= time_range[end])
                        setfield!(ids, field, get_time_array(ids, field, times[index], :constant))
                    end
                end
            end
        end
    end

    # trim time arrays
    for field in keys(ids)
        if hasdata(ids, field)
            value = getproperty(ids, field)
        else
            continue
        end
        if typeof(value) <: IDS && (!(typeof(value) <: IMASdd.pulse_schedule) || trim_pulse_schedule)
            trim_time!(value, time_range; trim_pulse_schedule)
        elseif typeof(value) <: IDSvector{<:IDSvectorTimeElement}
            # pass
        elseif typeof(value) <: IDSvector
            for sub_ids in value
                trim_time!(sub_ids, time_range; trim_pulse_schedule)
            end
        elseif field == :time && typeof(value) <: Vector && !isempty(value)
            times = value
            if times[1] > time_range[end] || times[end] < time_range[1]
                @warn "$(location(ids, field)) was emptied since time=[$(times[1])...$(times[end])] and time_range=$(time_range)"
                empty!(ids, field)
            else
                index = (times .>= time_range[1]) .&& (times .<= time_range[end])
                setfield!(ids, field, times[index])
            end
        end
    end

    if typeof(ids) <: DD
        if ids.global_time > time_range[end]
            ids.global_time = time_range[end]
        elseif ids.global_time < time_range[1]
            ids.global_time = time_range[1]
        end
    end

    return ids
end

export trim_time!
push!(document[:Time], :trim_time!)
