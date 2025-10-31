"""
    utlocation(@nospecialize(ids::IDS), field::Symbol)

Returns string with IDS universal time location
"""
@nospecializeinfer function utlocation(@nospecialize(ids::IDS), field::Symbol)
    return "$(utlocation(ids)).$(field)"
end

"""
    utlocation(@nospecialize(ids::IDS))
"""
@nospecializeinfer function utlocation(@nospecialize(ids::IDS))
    return p2i(f2p(ids; utime=true); zero_to_column=true)
end

"""
    ulocation(@nospecialize(ids::IDS), field::Symbol)

Returns string with IDS universal location
"""
@nospecializeinfer function ulocation(@nospecialize(ids::IDS), field::Symbol)
    return string(f2u(ids), ".", field)
end

function ulocation(ids::DD, field::Symbol)
    return string(field)
end

@nospecializeinfer function ulocation(@nospecialize(ids_type::Type{<:IDS}), field::Symbol)
    return string(fs2u(ids_type), ".", field)
end

"""
    ulocation(@nospecialize(ids::Union{IDS,IDSvector}))
"""
@nospecializeinfer function ulocation(@nospecialize(ids::IDS))
    return f2u(ids)
end

function ulocation(ids::DD)
    return "dd"
end

@nospecializeinfer function ulocation(@nospecialize(ids::IDSvector))
    return f2u(ids)[1:end-3]
end

"""
    location(@nospecialize(ids::IDS), field::Symbol)

Returns string with IDS location
"""
@nospecializeinfer function location(@nospecialize(ids::IDS), field::Symbol)
    return string(f2i(ids), ".", field)
end

@nospecializeinfer function location(@nospecialize(ids::DD), field::Symbol)
    return string(field)
end

"""
    location(@nospecialize(ids::Union{IDS,IDSvector}))
"""
@nospecializeinfer function location(@nospecialize(ids::IDS))
    return f2i(ids)
end

@nospecializeinfer function location(@nospecialize(ids::DD))
    return "dd"
end

@nospecializeinfer function location(@nospecialize(ids::IDSvector))
    return f2i(ids)[1:end-3]
end

"""
    f2u(@nospecialize(ids))

Returns universal IDS location
"""
@nospecializeinfer function f2u(@nospecialize(ids::IDS))
    return fs2u(typeof(ids))
end

@nospecializeinfer function f2u(@nospecialize(ids::IDSvector))
    return fs2u(eltype(ids))
end

"""
    fs2u(@nospecialize(ids_type::Type))

Returns universal IDS location
"""
@nospecializeinfer function fs2u(@nospecialize(ids_type::Type{<:DD}))
    return "dd"
end

@nospecializeinfer function fs2u(@nospecialize(ids_type::Type{<:IDS}))
    return fs2u(nameof(ids_type), ids_type)
end

@nospecializeinfer function fs2u(@nospecialize(ids_type::Type{<:IDSvector}))
    return fs2u(nameof(eltype(ids_type)), ids_type)
end

const UNDERSCORE_REGEX = r"___|__"

# Memoization.@memoize ThreadSafeDicts.ThreadSafeDict function fs2u(ids::Symbol, ids_type::Type)
function fs2u(ids::Symbol, ids_type::Type)
    ids_str = string(ids)
    tmp = rstrip(replace(ids_str, UNDERSCORE_REGEX => s -> s == "___" ? "[:]." : "."), '.')
    if ids_type <: IDSvectorElement
        return string(tmp, "[:]")
    else
        return tmp
    end
end

"""
    f2p(@nospecialize(ids::Union{IDS,IDSvector}); utime::Bool=false)

Returns a vector of strings with parsed IDS path

NOTE: indexes of arrays of structures that cannot be determined are set to 0

NOTE: utime=true will set to 0 time elements
"""
@nospecializeinfer function f2p(@nospecialize(ids::Union{IDS,IDSvector}); utime::Bool=false)
    # Step 1: Build the base path name from the type
    T = typeof(ids)
    name = if T <: DD
        "dd"
    elseif T <: IDSvectorElement
        string(Base.typename(T).name, "__:__")
    elseif T <: IDSvector
        string(Base.typename(eltype(ids)).name, "__:__")
    elseif T <: IDS
        string(Base.typename(T).name)
    else
        error("Unsupported type: $T")
    end

    name = replace(name, "___" => "__:__")
    name_parts = eachsplit(name, "__")
    N = count(":", name)

    # Step 2: Collect indices for all vector levels
    idx = zeros(Int, N)
    h = ids
    child = nothing
    k = N
    while k > 0 && h isa Union{IDS,IDSvector}
        if utime && (h isa IDSvector) && eltype(h) <: IDSvectorTimeElement
            idx[k] = 0
            k -= 1
        elseif h isa IDSvector
            idx[k] = child === nothing ? 0 : index(child)
            k -= 1
        end
        child = h
        h = parent(h)
    end

    # Step 3: Build final path by replacing ":" with collected indices
    result = Vector{String}(undef, count(!isempty, name_parts))  # Pre-allocate
    result_idx = 0
    idx_pos = 1
    for part in name_parts
        if part == ":"
            result_idx += 1
            result[result_idx] = string(idx[idx_pos])
            idx_pos += 1
        elseif !isempty(part)
            result_idx += 1
            result[result_idx] = String(part)
        end
    end

    return result
end

"""
    f2p_name(ids)

Returns string with IDS name
"""
function f2p_name(ids)
    return f2p_name(ids, parent(ids))
end

function f2p_name(ids::DD, ::Nothing)
    return "dd"
end

@nospecializeinfer function f2p_name(@nospecialize(ids::IDS), @nospecialize(parent::IDS))
    typename_str = string(Base.typename(typeof(ids)).name)
    return rsplit(typename_str, "__")[end]
end

@nospecializeinfer function f2p_name(@nospecialize(ids::IDS), ::Nothing)
    return f2p_name(typeof(ids)) * " [DETACHED]"
end

@nospecializeinfer function f2p_name(@nospecialize(ids::IDSvector), ::Nothing)
    return f2p_name(eltype(ids)) * " [DETACHED]"
end

@nospecializeinfer function f2p_name(@nospecialize(ids::IDSvectorElement), ::Nothing)
    return f2p_name(typeof(ids)) * " [DETACHED]"
end

@nospecializeinfer function f2p_name(@nospecialize(ids::IDSvectorElement), @nospecialize(parent::IDSvector))
    return string(index(ids))
end

@nospecializeinfer function f2p_name(@nospecialize(ids::IDSvector), @nospecialize(parent::IDS))
    return f2p_name(eltype(ids))
end

function f2p_name(ids_type::Type)
    typename_str = string(Base.typename(ids_type).name)
    return rsplit(typename_str, "__")[end]
end

"""
    f2i(@nospecialize(ids::Union{IDS,IDSvector}))

Returns string with IDS location
"""
@nospecializeinfer function f2i(@nospecialize(ids::Union{IDS,IDSvector}))
    # figure out base name
    T = typeof(ids)
    name = if T <: DD
        "dd"
    elseif T <: IDSvectorElement
        string(Base.typename(T).name, "___")
    elseif T <: IDSvector
        string(Base.typename(eltype(ids)).name, "___")
    elseif T <: IDS
        string(Base.typename(T).name)
    else
        error("Unsupported type: $T")
    end

    name = replace(name, "___" => "__:__")
    path_parts = eachsplit(name, "__")

    # build index list
    N = count(":", name)
    idx = zeros(Int, N)
    h = ids
    child = nothing
    k = N
    while k > 0 && typeof(h) <: Union{IDS,IDSvector}
        if h isa IDSvector
            idx[k] = child === nothing ? 0 : index(child)
            k -= 1
        end
        child = h
        h = parent(h)
    end

    # build the final string using IOBuffer approach but more efficiently
    io = IOBuffer()
    idx_pos = 1
    for p in path_parts
        isempty(p) && continue
        if p == ":"
            print(io, "[", idx[idx_pos], "]")
            idx_pos += 1
        else
            if position(io) > 0
                print(io, ".")
            end
            print(io, p)
        end
    end

    return String(take!(io))
end

"""
    i2p(loc::AbstractString)

Returns vector of substrings with parsed IDS path (ie. splits location in its elements)
"""
@inline function i2p(loc::AbstractString)
    parts = eachsplit(loc, '.')

    # First pass: count total elements needed
    N = 0
    for k in parts
        if !isempty(k)
            bracket_idx = findfirst('[', k)
            N += isnothing(bracket_idx) ? 1 : 2
        end
    end

    # Second pass: build result
    result = Vector{SubString{String}}(undef, N)
    j = 0
    for k in parts
        isempty(k) && continue
        j += 1
        bracket_idx = findfirst('[', k)
        if isnothing(bracket_idx)
            result[j] = k
        else
            result[j] = SubString(k, 1, bracket_idx - 1)
            j += 1
            result[j] = SubString(k, bracket_idx + 1, lastindex(k) - 1)  # strip ']'
        end
    end
    return result
end

"""
    p2i(path::AbstractVector{<:AbstractString}; zero_to_column::Bool=false)

Returns string from IDS path (vector of strings)

NOTE: zero_to_column=true, converts zero indexes to ":"
"""
function p2i(path::AbstractVector{<:AbstractString}; zero_to_column::Bool=false)
    isempty(path) && return ""

    io = IOBuffer()
    for (k, p) in enumerate(path)
        if !isempty(p) && (isdigit(p[1]) || p == ":")
            if zero_to_column && p == "0"
                print(io, "[:]")
            else
                print(io, "[", p, "]")
            end
        elseif k == 1
            print(io, p)
        else
            print(io, ".", p)
        end
    end

    return String(take!(io))
end

"""
    i2u(loc::AbstractString)

Return universal IDS location from IDS location string
"""
function i2u(loc::AbstractString)
    # Fast path for strings without brackets
    '[' ∉ loc && return String(loc)

    io = IOBuffer()
    i = 1
    len = lastindex(loc)

    while i <= len
        c = loc[i]
        if c == '['
            close_idx = findnext(']', loc, i)
            if isnothing(close_idx)
                error("Unmatched '[' in path: $loc")
            end
            print(io, "[:]")
            i = close_idx + 1
        else
            print(io, c)
            i += 1
        end
    end

    return String(take!(io))
end
