using InteractiveUtils: subtypes
document[:Time] = Symbol[]

@nospecializeinfer function Base.getindex(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}))
    return getindex(ids, global_time(ids))
end

@nospecializeinfer function Base.getindex(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), time0::Float64)
    index = nearest_causal_time(ids, time0).index
    return ids._value[index]
end

"""
    Base.setindex!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), @nospecialize(v::IDSvectorTimeElement), time0::Float64)

Set element of a time dependent IDSvector array

NOTE: this automatically sets the time of the element being set as well as of the time array in the parent IDS
"""
@nospecializeinfer function Base.setindex!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), @nospecialize(v::IDSvectorTimeElement), time0::Float64)
    i, perfect_match, _ = nearest_causal_time(ids, time0)
    if !perfect_match
        throw(IMASbadTime("Cannot insert data at time $time0 that does not match any existing time"))
    end

    unifm_time = time_array_from_parent_ids(ids, Val(:set))
    if isempty(unifm_time) || time0 != unifm_time[end]
        push!(unifm_time, time0)
    end

    v.time = time0 # note IDSvectorTimeElement should always have a .time field
    ids._value[i] = v
    setfield!(v, :_parent, WeakRef(ids))

    return v
end

"""
    Base.push!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), @nospecialize(v::IDSvectorTimeElement), time0::Float64)

Push to a time dependent IDSvector array

NOTE: this automatically sets the time of the element being pushed as well as of the time array in the parent IDS
"""
@nospecializeinfer function Base.push!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), @nospecialize(v::IDSvectorTimeElement), time0::Float64)
    if time0 <= ids[end].time
        throw(IMASbadTime("Cannot push! data at $time0 [s] at a time earlier or equal to $(ids[end].time) [s]"))
    end

    unifm_time = time_array_from_parent_ids(ids, Val(:set))
    if isempty(unifm_time) || time0 != unifm_time[end]
        push!(unifm_time, time0)
    end

    v.time = time0 # note IDSvectorTimeElement should always have a .time field
    push!(ids, v)
    setfield!(v, :_parent, WeakRef(ids))

    return ids
end

"""
    nearest_causal_time(time::Union{Base.Generator,AbstractVector{T}}, time0::T; bounds_error::Bool=true) where {T<:Float64}

Returns the `time` index that is closest to `time0` and satisfies causality.

This function also returns a boolean indicating if the `time0` is exactly contained in `time`.

If `bounds_error=false` the function will not throw an error if causal time is not available and will return time index=1 instead
"""
function nearest_causal_time(time::AbstractVector{T}, time0::T; bounds_error::Bool=true) where {T<:Float64}
    time_len = length(time)
    
    # Fast path for empty vector
    if time_len == 0
        throw(IMASbadTime("Cannot return a nearest_causal_time() of an empty time vector"))
    end
    
    # Fast path for last element (optimizes 99% of global_time cases)
    if time0 == time[time_len]
        return (index=time_len, perfect_match=true, causal_time=time0, out_of_bounds=false)
    end
    
    # Fast path for single element
    if time_len == 1
        if bounds_error && time0 < time[1]
            throw(IMASbadTime("Could not find causal time for time0=$time0. Available time is only [$(time[1])]"))
        end
        return (index=1, perfect_match=(time0 == time[1]), causal_time=time[1], out_of_bounds=(time0 < time[1]))
    end
    
    # General search using searchsortedlast
    index = searchsortedlast(time, time0)
    
    if index == 0
        if bounds_error
            throw(IMASbadTime("Could not find causal time for time0=$time0. Available time range is [$(time[1])...$(time[time_len])]"))
        else
            return (index=1, perfect_match=false, causal_time=time[1], out_of_bounds=true)
        end
    end
    
    causal_time = time[index]
    return (index=index, perfect_match=(causal_time == time0), causal_time=causal_time, out_of_bounds=false)
end

@nospecializeinfer function nearest_causal_time(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), time0::Float64; bounds_error::Bool=true)
    ids_len = length(ids)
    
    # Fast path for empty vector
    if ids_len == 0
        throw(IMASbadTime("Cannot return a nearest_causal_time() of an empty time vector"))
    end
    
    # Fast path for last element (optimizes 99% of global_time cases)
    if time0 == ids[ids_len].time
        return (index=ids_len, perfect_match=true, causal_time=time0, out_of_bounds=false)
    end
    
    # Fast path for single element
    if ids_len == 1
        first_time = ids[1].time
        if bounds_error && time0 < first_time
            throw(IMASbadTime("Could not find causal time for time0=$time0. Available time is only [$first_time]"))
        end
        return (index=1, perfect_match=(time0 == first_time), causal_time=first_time, out_of_bounds=(time0 < first_time))
    end
    
    # General search - use searchsortedlast with by function
    index = searchsortedlast(ids, (time=time0,); by=ids1 -> ids1.time)
    
    if index == 0
        if bounds_error
            throw(IMASbadTime("Could not find causal time for time0=$time0. Available time range is [$(ids[1].time)...$(ids[ids_len].time)]"))
        else
            return (index=1, perfect_match=false, causal_time=ids[1].time, out_of_bounds=true)
        end
    end
    
    causal_time = ids[index].time
    return (index=index, perfect_match=(causal_time == time0), causal_time=causal_time, out_of_bounds=false)
end

function nearest_causal_time(time, time0::T, vector::Vector; bounds_error::Bool=true) where {T<:Float64}
    i, perfect_match, causal_time, out_of_bounds = nearest_causal_time(time, time0; bounds_error)
    return (index=min(i, length(vector)), perfect_match=perfect_match, causal_time=causal_time, out_of_bounds=out_of_bounds)
end

"""
    time_array_from_parent_ids(@nospecialize(ids::IDS), mode::Val)

Traverse IDS hierarchy upstream and returns the IDS with the relevant :time vector

mode can be either (Val(:set) or Val(:get))
"""

@nospecializeinfer function time_array_from_parent_ids(@nospecialize(ids::IDS), ::Val{:set})
    # Iterative traversal to reduce function call overhead
    current = ids
    while current !== nothing
        current_type = typeof(current)

        # Check if current node has time field
        if hasfield(current_type, :time) && fieldtype_typeof(current, :time) <: Vector{Float64}
            if ismissing(current, :time)
                current.time = Float64[]
            end
            return getfield(current, :time)
        end

        # Move to parent
        current = parent(current)
    end

    # Reached top without finding time array
    return Float64[]
end

@nospecializeinfer function time_array_from_parent_ids(@nospecialize(ids::IDS), ::Val{:get})
    # Iterative traversal to reduce function call overhead
    current = ids
    while current !== nothing
        current_type = typeof(current)

        # Check if current node has time field
        if hasfield(current_type, :time) && fieldtype_typeof(current, :time) <: Vector{Float64}
            if ismissing(current, :time)
                # Continue searching up the hierarchy for get mode
                current = parent(current)
                continue
            end
            return getfield(current, :time)
        end

        # Move to parent
        current = parent(current)
    end

    # Reached top without finding time array
    return Float64[]
end

@nospecializeinfer function time_array_from_parent_ids(@nospecialize(ids::IDSvector), mode::Val)
    return time_array_from_parent_ids(parent(ids), mode)
end

function time_array_from_parent_ids(::Nothing, mode::Val)
    return Float64[]
end

@nospecializeinfer function time_array_from_parent_ids(@nospecialize(ids::IDStop), ::Val{:set})
    if hasfield(typeof(ids), :time)
        if ismissing(ids, :time)
            ids.time = Float64[]
        end
        return getfield(ids, :time)
    else
        return Float64[]
    end
end

@nospecializeinfer function time_array_from_parent_ids(@nospecialize(ids::IDStop), ::Val{:get})
    if hasfield(typeof(ids), :time)
        if ismissing(ids, :time)
            return Float64[]  # Return empty for get mode if missing
        end
        return getfield(ids, :time)
    else
        return Float64[]
    end
end

"""
    global_time(ids::Union{IDS,IDSvector})

Get the dd.global_time of a given IDS

If top-level dd cannot be reached then returns `Inf`
"""
@inline @nospecializeinfer function global_time(@nospecialize(ids::Union{IDS,IDSvector}))
    return global_time(top_dd(ids))
end

@inline function global_time(::Nothing)
    return Inf
end

@inline function global_time(dd::DD)
    return getfield(dd, :global_time)
end

"""
    global_time(ids::Union{IDS,IDSvector}, time0::Float64)

Set the dd.global_time of a given IDS
"""
@nospecializeinfer function global_time(@nospecialize(ids::Union{IDS,IDSvector}), time0::Float64)
    return setfield!(top_dd(ids), :global_time, time0)
end

export global_time
push!(document[:Time], :global_time)

"""
    set_time_array(@nospecialize(ids::IDS), field::Symbol, value)

Set value of a time-dependent array at the dd.global_time
"""
@nospecializeinfer function set_time_array(@nospecialize(ids::IDS), field::Symbol, value)
    return set_time_array(ids, field, global_time(ids), value)
end

"""
    set_time_array(@nospecialize(ids::IDS), field::Symbol, time0::Float64, value)

Set value of a time-dependent array at time0

NOTE: updates the closest causal element of an array
"""
@nospecializeinfer function set_time_array(@nospecialize(ids::IDS{<:Real}), field::Symbol, time0::Float64, value)
    time = time_array_from_parent_ids(ids, Val(:set))
    time_len = length(time)
    
    # Fast path: no time information
    if time_len == 0
        push!(time, time0)
        if field !== :time
            setproperty!(ids, field, [value]; error_on_missing_coordinates=false)
        end
        i = 1
    else
        # Find insertion point
        i = nearest_causal_time(time, time0).index
        max_time = time[time_len]  # Cache the last time value
        
        if time0 <= max_time
            # Update existing or past time
            if field !== :time
                if ismissing(ids, field) || isempty(getproperty(ids, field))
                    # Create new array with NaN padding
                    nan = typed_nan(value)
                    new_array = Vector{eltype(value)}(undef, i)
                    @inbounds for k in 1:(i-1)
                        new_array[k] = nan
                    end
                    new_array[i] = value
                    setproperty!(ids, field, new_array; error_on_missing_coordinates=false)
                else
                    last_value = getproperty(ids, field)
                    last_len = length(last_value)
                    if last_len < i
                        # Extend array with repeated last value
                        resize!(last_value, i)
                        last_val = last_value[last_len]
                        @inbounds for k in (last_len+1):(i-1)
                            last_value[k] = last_val
                        end
                        last_value[i] = value
                    else
                        last_value[i] = value
                    end
                end
            end
        else
            # Append new time slice
            push!(time, time0)
            if field !== :time
                if ismissing(ids, field) || isempty(getproperty(ids, field))
                    # Create new array with NaN padding
                    nan = typed_nan(value)
                    new_len = time_len + 1
                    new_array = Vector{eltype(value)}(undef, new_len)
                    @inbounds for k in 1:time_len
                        new_array[k] = nan
                    end
                    new_array[new_len] = value
                    setproperty!(ids, field, new_array; error_on_missing_coordinates=false)
                else
                    last_value = getproperty(ids, field)
                    last_len = length(last_value)
                    target_len = time_len + 1
                    
                    # Extend to target length
                    if last_len < target_len
                        resize!(last_value, target_len)
                        if last_len > 0
                            last_val = last_value[last_len]
                            @inbounds for k in (last_len+1):(target_len-1)
                                last_value[k] = last_val
                            end
                        end
                        last_value[target_len] = value
                    end
                end
            end
            i = time_len + 1
        end
    end
    
    if access_log.enabled
        push!(access_log.write, ulocation(ids, field))
    end
    return value
end

@nospecializeinfer function set_time_array(@nospecialize(ids::IDS{<:Real}), field::Symbol, time0::Float64, value::AbstractArray)
    time = time_array_from_parent_ids(ids, Val(:set))
    time_len = length(time)
    value_size = size(value)
    
    T = eltype(ids)

    # Fast path: no time information
    if time_len == 0
        push!(time, time0)
        if field !== :time
            # Create array with time dimension last
            new_size = (value_size..., 1)
            new_array = Array{eltype(value)}(undef, new_size)
            new_array[ntuple(d -> d == length(new_size) ? 1 : Colon(), length(new_size))...] .= value
            setproperty!(ids, field, new_array; error_on_missing_coordinates=false)
        end
        i = 1
    else
        # Find insertion point
        i = nearest_causal_time(time, time0).index
        max_time = time[time_len]
        
        if time0 <= max_time
            # Update existing or past time
            if field !== :time
                if ismissing(ids, field) || isempty(getproperty(ids, field))
                    # Create new array with NaN padding
                    nan = typed_nan(eltype(value))
                    new_size = (value_size..., i)
                    new_array = fill(nan, new_size)
                    
                    # Set the value at position i
                    idx = ntuple(d -> d == length(new_size) ? i : Colon(), length(new_size))
                    new_array[idx...] .= value
                    setproperty!(ids, field, new_array; error_on_missing_coordinates=false)
                else
                    last_value = getproperty(ids, field)
                    last_time_size = size(last_value)[end]
                    
                    if last_time_size < i
                        # Extend array
                        old_size = size(last_value)
                        new_size = (old_size[1:end-1]..., i)
                        new_array = Array{T}(undef, new_size)
                        
                        # Copy existing data
                        old_idx = ntuple(d -> d == length(old_size) ? (1:last_time_size) : Colon(), length(old_size))
                        new_idx = ntuple(d -> d == length(new_size) ? (1:last_time_size) : Colon(), length(new_size))
                        new_array[new_idx...] .= last_value[old_idx...]
                        
                        # Fill gap with last values
                        if last_time_size > 0
                            last_slice_idx = ntuple(d -> d == length(old_size) ? last_time_size : Colon(), length(old_size))
                            last_slice = @view last_value[last_slice_idx...]
                            @inbounds for k in (last_time_size+1):(i-1)
                                gap_idx = ntuple(d -> d == length(new_size) ? k : Colon(), length(new_size))
                                new_array[gap_idx...] .= last_slice
                            end
                        end
                        
                        # Set new value
                        value_idx = ntuple(d -> d == length(new_size) ? i : Colon(), length(new_size))
                        new_array[value_idx...] .= value
                        setproperty!(ids, field, new_array; error_on_missing_coordinates=false)
                    else
                        # Direct assignment
                        value_idx = ntuple(d -> d == ndims(last_value) ? i : Colon(), ndims(last_value))
                        if length(value) == 1
                            last_value[value_idx...] = value[1]
                        else
                            last_value[value_idx...] .= value
                        end
                    end
                end
            end
        else
            # Append new time slice
            push!(time, time0)
            if field !== :time
                if ismissing(ids, field) || isempty(getproperty(ids, field))
                    # Create new array with NaN padding
                    nan = typed_nan(eltype(value))
                    new_len = time_len + 1
                    new_size = (value_size..., new_len)
                    new_array = fill(nan, new_size)
                    
                    # Set the value at the last position
                    value_idx = ntuple(d -> d == length(new_size) ? new_len : Colon(), length(new_size))
                    new_array[value_idx...] .= value
                    setproperty!(ids, field, new_array; error_on_missing_coordinates=false)
                else
                    last_value = getproperty(ids, field)
                    old_size = size(last_value)
                    new_len = time_len + 1
                    new_size = (old_size[1:end-1]..., new_len)
                    new_array = Array{T}(undef, new_size)
                    
                    # Copy existing data
                    old_idx = ntuple(d -> d == length(old_size) ? (1:old_size[end]) : Colon(), length(old_size))
                    new_idx = ntuple(d -> d == length(new_size) ? (1:old_size[end]) : Colon(), length(new_size))
                    new_array[new_idx...] .= last_value[old_idx...]
                    
                    # Fill gap with last values if needed
                    if old_size[end] < time_len
                        last_slice_idx = ntuple(d -> d == length(old_size) ? old_size[end] : Colon(), length(old_size))
                        last_slice = @view last_value[last_slice_idx...]
                        @inbounds for k in (old_size[end]+1):time_len
                            gap_idx = ntuple(d -> d == length(new_size) ? k : Colon(), length(new_size))
                            new_array[gap_idx...] .= last_slice
                        end
                    end
                    
                    # Set new value
                    value_idx = ntuple(d -> d == length(new_size) ? new_len : Colon(), length(new_size))
                    new_array[value_idx...] .= value
                    setproperty!(ids, field, new_array; error_on_missing_coordinates=false)
                end
            end
            i = time_len + 1
        end
    end
    
    if access_log.enabled
        push!(access_log.write, ulocation(ids, field))
    end
    return value
end

export set_time_array
push!(document[:Time], :set_time_array)

"""
    get_time_array(@nospecialize(ids::IDS{<:Real}), field::Symbol, scheme::Symbol=:constant)

Get data from a time-dependent array at the dd.global_time
"""
@nospecializeinfer function get_time_array(@nospecialize(ids::IDS{<:Real}), field::Symbol, scheme::Symbol=:constant)
    T = eltype(ids)
    results = get_time_array(ids, field, global_time(ids), scheme)
    tp = concrete_fieldtype_typeof(ids, field)
    if tp <: Vector{T}
        return results::T
    else
        return results
    end
end

"""
    get_time_array(@nospecialize(ids::IDS), field::Symbol, time0::Float64, scheme::Symbol=:constant)

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
@nospecializeinfer function get_time_array(@nospecialize(ids::IDS), field::Symbol, time0::Float64, scheme::Symbol=:constant; first::Symbol=:error, last::Symbol=:constant)
    data = getproperty(ids, field)
    tidx = time_coordinate_index(ids, field; error_if_not_time_dependent=false)
    if tidx == 0
        return data
    elseif fieldtype_typeof(ids, field) <: AbstractVector # special treatment to maximize speed of what we call 99% of the times
        time = time_array_from_parent_ids(ids, Val(:get))
        get_time_array(time, data, time0, scheme, tidx; first, last)
    else
        time = time_array_from_parent_ids(ids, Val(:get))
        result = dropdims_view(get_time_array(time, data, [time0], scheme, tidx; first, last); dims=tidx)
        return isa(result, Array) && ndims(result) == 0 ? result[] : result
    end
end

function dropdims_view(arr; dims::Int)
    indices = ntuple(i -> (i == dims ? 1 : Colon()), ndims(arr))
    result = @view arr[indices...]
    return ndims(result) == 0 ? result[] : result
end

@nospecializeinfer function get_time_array(@nospecialize(ids::IDS{<:Real}), field::Symbol, time0::Vector{Float64}, scheme::Symbol=:constant; first::Symbol=:error, last::Symbol=:constant)
    @assert !isempty(time0) "get_time_array() `time0` must have some times specified"
    tidx = time_coordinate_index(ids, field; error_if_not_time_dependent=true)
    time = time_array_from_parent_ids(ids, Val(:get))
    array = getproperty(ids, field)
    array_time_length = size(array)[tidx]
    if length(time) < array_time_length
        throw(IMASbadTime("length(time)=$(length(time)) must be greater than size($(location(ids, field)))[$tidx]=$(array_time_length)"))
    end
    tp = eltype(getfield(ids, field))
    return get_time_array(time, array, time0, scheme, tidx; first, last)::Array{tp}
end

function get_time_array(
    time::AbstractVector{Float64},
    vector::AbstractVector{T},
    time0::Vector{Float64},
    scheme::Symbol,
    tidx::Int=1;
    first::Symbol=:error,
    last::Symbol=:constant
) where {T<:Real}
    @assert tidx == 1
    if scheme == :constant
        return constant_time_interp(@views(time[1:length(vector)]), vector, time0; first, last)
    else
        itp = interp1d_itp(@views(time[1:length(vector)]), vector, scheme)
        return extrap1d(itp; first, last).(time0)
    end
end

function get_time_array(
    time::AbstractVector{Float64},
    vector::AbstractVector{T},
    time0::Float64,
    scheme::Symbol,
    tidx::Int=1;
    first::Symbol=:error,
    last::Symbol=:constant
) where {T<:Real}
    @assert tidx == 1
    i, perfect_match, _ = nearest_causal_time(time, time0, vector; bounds_error=(first == :error))
    if perfect_match
        return vector[i]
    elseif scheme == :constant
        return constant_time_interp(@views(time[1:length(vector)]), vector, time0; first, last)
    else
        itp = interp1d_itp(@views(time[1:length(vector)]), vector, scheme)
        return extrap1d(itp; first, last).(time0)
    end
end

function get_time_array(
    time::AbstractVector{Float64},
    array::AbstractArray{T},
    time0::Vector{Float64},
    scheme::Symbol,
    tidx::Int;
    first::Symbol=:error,
    last::Symbol=:constant
) where {T<:Real}
    array_size = size(array)
    n_time_out = length(time0)
    n_time_in = array_size[tidx]
    
    # Create output array with correct dimensions
    result_size = ntuple(i -> i == tidx ? n_time_out : array_size[i], ndims(array))
    result = Array{T}(undef, result_size)
    
    # Pre-compute time subset for efficiency
    time_subset = @view time[1:n_time_in]
    
    if tidx == 1
        # Optimized path for time-first arrays (most common case)
        if ndims(array) == 1
            # 1D case - direct call
            result[:] = get_time_array(time_subset, array, time0, scheme, 1; first, last)
        else
            # Multi-dimensional: process slices efficiently
            other_dims = CartesianIndices(array_size[2:end])
            @inbounds for idx in other_dims
                vector = @view array[:, idx]
                result[:, idx] = get_time_array(time_subset, vector, time0, scheme, 1; first, last)
            end
        end
    else
        # General case: use dimension permutation only when necessary
        perm = [tidx; setdiff(1:ndims(array), tidx)]
        array_permuted = PermutedDimsArray(array, perm)
        result_permuted = PermutedDimsArray(result, perm)
        
        # Reshape to 2D for vectorized processing
        array_2d = reshape(array_permuted, n_time_in, :)
        result_2d = reshape(result_permuted, n_time_out, :)
        n_cols = size(array_2d, 2)
        
        # Process columns in chunks for better cache locality
        chunk_size = min(1000, n_cols)
        @inbounds for start_col in 1:chunk_size:n_cols
            end_col = min(start_col + chunk_size - 1, n_cols)
            for col in start_col:end_col
                vector = @view array_2d[:, col]
                result_2d[:, col] = get_time_array(time_subset, vector, time0, scheme, 1; first, last)
            end
        end
    end
    
    return result
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
    for key in keys(dd)
        ids = getproperty(dd, key)
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
    new_timeslice!(@nospecialize(ids::IDS), time0::Float64=global_time(ids))

Recursively appends a deepcopy at time `time0` of the last time-slice of all time-dependent array structures under a given ids
"""
@nospecializeinfer function new_timeslice!(@nospecialize(ids::IDS), time0::Float64=global_time(ids))
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

@nospecializeinfer function new_timeslice!(@nospecialize(ids::IDS), path::AbstractVector{Symbol}, time0::Float64)
    return new_timeslice!(getfield(ids, path[1]), @views(path[2:end]), time0)
end

@nospecializeinfer function new_timeslice!(@nospecialize(ids::IDSvector), path::AbstractVector{Symbol}, time0::Float64)
    for k in 1:length(ids)
        new_timeslice!(ids[k], @views(path[2:end]), time0)
    end
end

@nospecializeinfer function new_timeslice!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), path::AbstractVector{Symbol}, time0::Float64)
    if !isempty(ids)
        tmp = fill!(typeof(ids[end])(;frozen=getfield(ids, :_frozen)), ids[end])
        push!(ids, tmp, time0)
    end
end

"""
    new_timeslice!(@nospecialize(ids::IDS{<:Real}), times::AbstractVector{Float64})

Extend IDSvector{<:IDSvectorTimeElement} and time dependent data arrays with times
"""
@nospecializeinfer function new_timeslice!(@nospecialize(ids::IDS{<:Real}), times::AbstractVector{Float64})
    @assert all(diff(times) .> 0) "retime!() times must be increasing"
    T = eltype(ids)

    for field in keys(ids)
        if hasdata(ids, field)
            value = getproperty(ids, field)
        else
            continue
        end

        if typeof(value) <: IDS
            retime!(value, times)

        elseif typeof(value) <: IDSvector{<:IDSvectorTimeElement}
            if isempty(value)
                last_time = -Inf
            else
                last_time = value[end].time
            end
            for time in times
                if time > last_time
                    resize!(value, time)
                end
            end

        elseif typeof(value) <: IDSvector
            for sub_ids in value
                retime!(sub_ids, times)
            end

        elseif field == :time && typeof(value) <: Vector
            if isempty(value)
                last_time = -Inf
            else
                last_time = value[end]
            end
            for time in times
                if time > last_time
                    push!(value, time)
                end
            end

        elseif typeof(value) <: Array
            tidx = time_coordinate_index(ids, field; error_if_not_time_dependent=false)
            if tidx > 0
                times = getproperty(coordinates(ids, field)[tidx])
                if isempty(times)
                    last_time = -Inf
                else
                    last_time = times[end]
                end
                M = length(times) + sum(times .> last_time)
                if M > size(value)[tidx]
                    nan = typed_nan(value)
                    current_size = size(value)
                    new_size = ntuple(i -> i == tidx ? M : current_size[i], ndims(value))

                    # Create a new array filled with NaN of the desired size
                    new_value = T.(fill(nan, new_size))

                    # Copy the values from the original array into the new array
                    indices = ntuple(i -> Colon(), ndims(value))  # Full indices for all dimensions
                    indices = Base.setindex(indices, 1:current_size[tidx], tidx)  # Adjust the d-th dimension
                    new_value[indices...] = value

                    setfield!(ids, field, new_value)
                end
            end
        end
    end

    return ids
end

export new_timeslice!
push!(document[:Time], :new_timeslice!)

"""
    resize!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}); wipe::Bool=true)

Resize time dependent array at global_time
"""
@nospecializeinfer function Base.resize!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}); wipe::Bool=true)
    time0 = global_time(ids)
    return resize!(ids, time0; wipe)
end

"""
    resize!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), time0::Float64; wipe::Bool=true)

Resize time dependent array based on time
"""
@nospecializeinfer function Base.resize!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), time0::Float64; wipe::Bool=true)
    # append a time slice
    time_existed = false
    if isempty(ids) || (time0 > ids[end].time)
        k = length(ids) + 1
        resize!(ids, k; wipe)
        ids[k].time = time0

        unifm_time = time_array_from_parent_ids(ids, Val(:set))
        if isempty(unifm_time) || time0 != unifm_time[end]
            push!(unifm_time, time0)
        end

    else
        # modify a time slice
        k = searchsortedlast(ids, (time=time0,); by=ids1 -> ids1.time)
        if k == 0
            throw(IMASbadTime(
                "Cannot resize $(location(ids)) at time $time0 since the structure already ranges between $(ids[1].time) and $(ids[end].time) [s]."
            ))
        elseif ids[k].time != time0
            throw(IMASbadTime(
                "Cannot resize $(location(ids)) at time $time0 since the structure already ranges between $(ids[1].time) and $(ids[end].time) [s]. Closest causal time is at $(ids[k].time) [s]"
            ))
        end
        if ids[k].time == time0
            time_existed = true
        end
        if wipe
            empty!(ids[k])
        end
        ids[k].time = time0
    end

    return ids[k]
end

export resize!
push!(document[:Time], :resize!)

"""
    retime!(@nospecialize(ids::IDS), time0::Float64=global_time(ids))

Recursively change the time of the last time-slices or last time-depedent vector elements in a IDS
"""
@nospecializeinfer function retime!(@nospecialize(ids::IDS), time0::Float64=global_time(ids))
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

@nospecializeinfer function retime!(@nospecialize(ids::IDSvector{<:IDSvectorTimeElement}), time0::Float64)
    if !isempty(ids)
        retime!(ids[end], time0)
    end
end

@nospecializeinfer function retime!(@nospecialize(ids::IDSvector), time0::Float64)
    for k in 1:length(ids)
        retime!(ids[k], time0)
    end
end

export retime!
push!(document[:Time], :retime!)

"""
    get_timeslice(@nospecialize(ids::IDS), time0::Float64=global_time(ids), scheme::Symbol=:constant; slice_pulse_schedule::Bool=true)

Returns data at the given `time0` (by default at the global_time)

Data is selected from time dependent arrays of structures using closest causal time point.

Data is selected from time dependent arrays using these possible schemes `[:constant, :linear, :quadratic, :cubic, :pchip, :lagrange]`
"""
@nospecializeinfer function get_timeslice(@nospecialize(ids::IDS), time0::Float64=global_time(ids), scheme::Symbol=:constant; slice_pulse_schedule::Bool=false)
    return get_timeslice(eltype(ids), ids, time0, scheme; slice_pulse_schedule)
end

"""
    get_timeslice(el_type::Type{<:Real}, @nospecialize(ids::IDS), time0::Float64=global_time(ids), scheme::Symbol=:constant; slice_pulse_schedule::Bool=false)

get_timeslice that retuns IDS of type `el_type`
"""
@nospecializeinfer function get_timeslice(el_type::Type{<:Real}, @nospecialize(ids::IDS), time0::Float64=global_time(ids), scheme::Symbol=:constant; slice_pulse_schedule::Bool=false)
    ids0 = Base.typename(typeof(ids)).wrapper{el_type}(;frozen=getfield(ids, :_frozen))
    setfield!(ids0, :_parent, getfield(ids, :_parent))
    copy_timeslice!(ids0, ids, time0, scheme; slice_pulse_schedule)
    if typeof(ids0) <: DD
        ids0.global_time = time0
    end
    return ids0
end

export get_timeslice
push!(document[:Time], :get_timeslice)

"""
    copy_timeslice!(
        @nospecialize(ids0::IDS{T1}),
        @nospecialize(ids::IDS{T2}),
        time0::Float64,
        scheme::Symbol;
        slice_pulse_schedule::Bool) where {T1<:Real,T2<:Real}

Copy data at a given time from `ids` to `ids0`
"""
@nospecializeinfer function copy_timeslice!(
    @nospecialize(ids0::IDS{<:Real}),
    @nospecialize(ids::IDS{<:Real}),
    time0::Float64,
    scheme::Symbol=:constant;
    slice_pulse_schedule::Bool=false) 

    T1 = eltype(ids0)
    T2 = eltype(ids)

    for field in keys(ids)
        if hasdata(ids, field)
            value = getproperty(ids, field)
        else
            continue
        end
        if field == :time
            if typeof(value) <: Vector
                set_time_array(ids0, field, time0, time0)
            else
                setproperty!(ids0, field, time0; error_on_missing_coordinates=false)
            end
        elseif typeof(value) <: IMASdd.pulse_schedule && !slice_pulse_schedule
            fill!(getproperty(ids0, field), value)
        elseif typeof(value) <: Union{IDS,IDSvector}
            copy_timeslice!(getfield(ids0, field), value, time0, scheme; slice_pulse_schedule)
        else
            tidx = time_coordinate_index(ids, field; error_if_not_time_dependent=false)
            if tidx > 0
                value = get_time_array(ids, field, time0, scheme)
                if eltype(value) <: T2
                    if eltype(value) <: T1
                        set_time_array(ids0, field, time0, value)
                    else
                        set_time_array(ids0, field, time0, T1.(value))
                    end
                else
                    set_time_array(ids0, field, time0, value)
                end
            else
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
    end

    return ids0
end

@nospecializeinfer function copy_timeslice!(
    @nospecialize(ids0::IDSvector{<:IDSvectorTimeElement}),
    @nospecialize(ids::IDSvector{<:IDSvectorTimeElement}),
    time0::Float64,
    scheme::Symbol;
    slice_pulse_schedule::Bool)

    if !isempty(ids)
        resize!(ids0, time0)
        copy_timeslice!(ids0[time0], ids[time0], time0, scheme; slice_pulse_schedule)
    end

    return ids0
end

@nospecializeinfer function copy_timeslice!(
    @nospecialize(ids0::IDSvector{<:IDSvectorElement}),
    @nospecialize(ids::IDSvector{<:IDSvectorElement}),
    time0::Float64,
    scheme::Symbol;
    slice_pulse_schedule::Bool)

    resize!(ids0, length(ids))
    for k in 1:length(ids)
        copy_timeslice!(ids0[k], ids[k], time0, scheme; slice_pulse_schedule)
    end

    return ids0
end

export copy_timeslice!
push!(document[:Time], :copy_timeslice!)

"""
    trim_time!(@nospecialize(ids::IDS); trim_pulse_schedule::Bool=false)

Recursively remove all time dependent data tha occurs after global_time
"""
@nospecializeinfer function trim_time!(@nospecialize(ids::IDS); trim_pulse_schedule::Bool=false)
    return trim_time!(ids, (-Inf, top_dd(ids).global_time); trim_pulse_schedule)
end

"""
    trim_time!(@nospecialize(ids::IDS), time_range::Tuple{Float64,Float64}; trim_pulse_schedule::Bool=false)

Recursively remove all time dependent data tha occurs outside of `time_range`
"""
@nospecializeinfer function trim_time!(@nospecialize(ids::IDS), time_range::Tuple{Float64,Float64}; trim_pulse_schedule::Bool=false)
    @assert time_range[end] > time_range[1]
    if time_range == (-Inf, Inf)
        return ids
    end

    # Pre-compute range checks
    min_time, max_time = time_range
    
    # trim time dependent IDSvector, and time dependent data arrays
    for field in keys(ids)
        if !hasdata(ids, field)
            continue
        end
        
        value = getproperty(ids, field)
        value_type = typeof(value)
        
        if value_type <: IDS
            if (!(value_type <: IMASdd.pulse_schedule) || trim_pulse_schedule) && !isempty(value)
                # Process sub-IDSs
                trim_time!(value, time_range; trim_pulse_schedule)
            end
        elseif value_type <: IDSvector{<:IDSvectorTimeElement}
            if !isempty(value)
                # Remove from end
                while !isempty(value) && value[end].time > max_time
                    pop!(value)
                end
                # Remove from beginning
                while !isempty(value) && value[1].time < min_time
                    popfirst!(value)
                end
            end
        elseif value_type <: IDSvector
            # Process sub-IDSs
            for sub_ids in value
                trim_time!(sub_ids, time_range; trim_pulse_schedule)
            end
        elseif field != :time && value_type <: Array
            tidx = time_coordinate_index(ids, field; error_if_not_time_dependent=false)
            if tidx > 0
                times = getproperty(coordinates(ids, field)[tidx])
                if times === missing
                    throw(IMASbadTime(location(ids, field)))
                end
                if !isempty(times)
                    first_time, last_time = times[1], times[end]
                    if first_time > max_time || last_time < min_time
                        #@warn "$(location(ids, field)) was emptied since time=[$first_time...$last_time] and time_range=$(time_range)"
                        empty!(ids, field)
                    else
                        # Use bit operations for better performance
                        valid_indices = (times .>= min_time) .& (times .<= max_time)
                        if any(valid_indices)
                            filtered_times = times[valid_indices]
                            setfield!(ids, field, get_time_array(ids, field, filtered_times, :constant))
                        else
                            empty!(ids, field)
                        end
                    end
                end
            end
        end
    end

    # trim time arrays in this IDS
    if hasfield(typeof(ids), :time) && fieldtype_typeof(ids, :time) <: Vector && hasdata(ids, :time)
        value = getproperty(ids, :time)
        if !isempty(value)
            first_time, last_time = value[1], value[end]
            if first_time > max_time || last_time < min_time
                empty!(ids, :time)
            else
                # Filter time array efficiently
                valid_indices = (value .>= min_time) .& (value .<= max_time)
                if any(valid_indices)
                    setfield!(ids, :time, value[valid_indices])
                else
                    empty!(ids, :time)
                end
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

"""
    time_dependent_leaves(@nospecialize(ids::IDS{<:Real}))

Returns Dict{String,Vector{IMASnodeRepr{T}}} mapping time coordinate locations
to vectors of data fields that use that time coordinate.

NOTE: Excludes :time fields and error fields ending in "_σ"
"""
@nospecializeinfer function time_dependent_leaves(@nospecialize(ids::IDS{<:Real})) 
    tg = Dict{String,Vector{IMASnodeRepr{T}}}()
    for leaf in AbstractTrees.Leaves(ids)
        if typeof(leaf) <: IMASnodeRepr && leaf.field != :time && !endswith(string(leaf.field), "_σ") && time_coordinate_index(leaf.ids, leaf.field; error_if_not_time_dependent=false) != 0
            id = ulocation(leaf.ids, leaf.field)
            if id ∉ keys(tg)
                tg[id] = Vector{IMASnodeRepr{T}}()
            end
            push!(tg[id], leaf)
        end
    end
    return tg
end

export time_dependent_leaves
push!(document[:Time], :time_dependent_leaves)

"""
    time_groups(ids::IDS{<:Real}; min_channels::Int=0)

Groups identical time vectors and optionally filters by minimum group size.

Returns Vector{Vector{IMASnodeRepr{T}}} containing groups of time fields
that share identical time arrays, keeping only groups with at least min_channels members.
"""
@nospecializeinfer function time_groups(@nospecialize(ids::IDS{<:Real}); min_channels::Int=0)
    T = eltype(ids)
    tg = Dict{String,Vector{IMASnodeRepr{T}}}()
    for leaf in IMASdd.AbstractTrees.Leaves(ids)
        if typeof(leaf) <: IMASnodeRepr && leaf.field == :time
            t = getproperty(leaf.ids, :time)
            id = "$(ulocation(leaf.ids))_$(hash(t))"
            if id ∉ keys(tg)
                tg[id] = Vector{IMASnodeRepr{T}}()
            end
            push!(tg[id], leaf)
        end
    end

    for id in collect(keys(tg))
        if length(tg[id]) < min_channels
            delete!(tg, id)
        end
    end

    return collect(values(tg))
end

export time_groups
push!(document[:Time], :time_groups)
