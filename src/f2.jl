"""
    ulocation(@nospecialize(ids::IDS), field::Symbol)

Returns IMAS universal location given IDS and field
"""
function ulocation(@nospecialize(ids::IDS), field::Symbol)
    return string(f2u(ids), ".", field)
end

function ulocation(@nospecialize(ids::DD), field::Symbol)
    return string(field)
end

function ulocation(@nospecialize(ids_type::Type{<:IDS}), field::Symbol)
    return string(fs2u(ids_type), ".", field)
end

"""
    ulocation(@nospecialize(ids::Union{IDS,IDSvector}))

Returns IMAS universal location of a given IDS
"""
function ulocation(@nospecialize(ids::IDS))
    return f2u(ids)
end

function ulocation(@nospecialize(ids::DD))
    return "dd"
end

function ulocation(@nospecialize(ids::IDSvector))
    return f2u(ids)[1:end-3]
end

"""
    location(@nospecialize(ids::IDS), field::Symbol)

Returns IMAS location of a given IDS and field
"""
function location(@nospecialize(ids::IDS), field::Symbol)
    return string(f2i(ids), ".", field)
end

function location(@nospecialize(ids::DD), field::Symbol)
    return string(field)
end

"""
    location(@nospecialize(ids::Union{IDS,IDSvector}))

Returns IMAS location of a give IDS
"""
function location(@nospecialize(ids::IDS))
    return f2i(ids)
end

function location(@nospecialize(ids::DD))
    return "dd"
end

function location(@nospecialize(ids::IDSvector))
    return f2i(ids)[1:end-3]
end

"""
    f2u(ids)

Returns universal IMAS location of a given IDS
"""
function f2u(ids::T) where {T<:IDS}
    return fs2u(T)
end

function f2u(ids::IDSvector{T}) where {T}
    return fs2u(T)
end

"""
    fs2u(ids)

Returns universal IMAS location of a given IDS type
"""
function fs2u(@nospecialize(ids_type::Type{<:DD}))
    return "dd"
end

function fs2u(@nospecialize(ids_type::Type{<:IDS}))
    return fs2u(nameof(ids_type), ids_type)
end

function fs2u(@nospecialize(ids_type::Type{<:IDSvector}))
    return fs2u(nameof(eltype(ids_type)), ids_type)
end

Memoization.@memoize ThreadSafeDicts.ThreadSafeDict function fs2u(ids::Symbol, ids_type::Type)
    tmp = rstrip(replace(string(ids), r"___|__" => s -> s == "___" ? "[:]." : "."), '.')
    if ids_type <: IDSvectorElement
        return string(tmp, "[:]")
    else
        return tmp
    end
end

"""
    f2p(@nospecialize(ids::Union{IDS,IDSvector}))

Return parsed IMAS path of a given IDS

NOTE: indexes of arrays of structures that cannot be determined are set to 0
"""
function f2p(@nospecialize(ids::Union{IDS,IDSvector}))
    # Step 1: Build the base path name from the type
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
    name_parts = eachsplit(name, "__")
    N = count(":", name)

    # Step 2: Collect indices for all vector levels
    idx = zeros(Int, N)
    h = ids
    child = nothing
    k = N
    while k > 0 && h isa Union{IDS,IDSvector}
        if h isa IDSvector
            idx[k] = child === nothing ? 0 : index(child)
            k -= 1
        end
        child = h
        h = parent(h)
    end

    # Step 3: Build final path by replacing ":" with collected indices
    result = String[]
    for part in name_parts
        if part == ":"
            push!(result, string(popfirst!(idx)))
        elseif !isempty(part)
            push!(result, part)
        end
    end

    return result
end

"""
    f2p_name(ids)

Returns string with name of current IDS
"""
function f2p_name(ids)
    return f2p_name(ids, parent(ids))
end

function f2p_name(ids::DD, ::Nothing)
    return ("dd",)
end

function f2p_name(@nospecialize(ids::IDS), @nospecialize(parent::IDS))
    return (rsplit(string(Base.typename(typeof(ids)).name), "__")[end],)
end

function f2p_name(@nospecialize(ids::IDS), ::Nothing)
    return (f2p_name(typeof(ids)) * " [DETACHED]",)
end

function f2p_name(@nospecialize(ids::IDSvector), ::Nothing)
    return (f2p_name(eltype(ids)) * " [DETACHED]",)
end

function f2p_name(@nospecialize(ids::IDSvectorElement), ::Nothing)
    return (f2p_name(typeof(ids)) * " [DETACHED]",)
end

function f2p_name(@nospecialize(ids::IDSvectorElement), @nospecialize(parent::IDSvector))
    return (string(index(ids)),)
end

function f2p_name(@nospecialize(ids::IDSvector), @nospecialize(parent::IDS))
    return (f2p_name(eltype(ids)),)
end

function f2p_name(ids_type::Type)
    return rsplit(string(Base.typename(ids_type).name), "__")[end]
end

"""
    f2i(@nospecialize(ids::Union{IDS,IDSvector}))

return IMAS location of a given IDS
"""
function f2i(@nospecialize(ids::Union{IDS,IDSvector}))
    # figure out base name
    T = typeof(ids)
    if T <: DD
        name = "dd"
    elseif T <: IDSvectorElement
        name = string(Base.typename(T).name, "___")
    elseif T <: IDSvector
        name = string(Base.typename(eltype(ids)).name, "___")
    elseif T <: IDS
        name = string(Base.typename(T).name)
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

    # build the final string directly
    io = IOBuffer()
    for p in path_parts
        isempty(p) && continue
        if p == ":"
            i = popfirst!(idx)
            print(io, "[$i]")
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
    i2p(imas_location::AbstractString)

return parsed IMAS path (ie. splits IMAS location in its elements)
"""
function i2p(imas_location::AbstractString)
    parts = eachsplit(imas_location, '.')
    N  = 0
    for k in parts
        if !isempty(k)
            idx = findfirst('[', k)
            N += isnothing(idx) ? 1 : 2
        end
    end
    result = Vector{SubString{String}}(undef, N)
    j = 0
    for k in parts
        isempty(k) && continue
        j += 1
        idx = findfirst('[', k)
        if isnothing(idx)
            result[j] = k
        else
            result[j] = SubString(k, 1, idx - 1)
            j += 1
            result[j] = SubString(k, idx + 1, lastindex(k) - 1)  # strip ']'
        end
    end
    return result
end

"""
    p2i(path::Union{AbstractVector{<:AbstractString},Base.Generator})

Combine list of IMAS location elements into a string
"""
function p2i(path::AbstractVector{<:AbstractString})
    io = IOBuffer()
    for (k, p) in enumerate(path)
        if isdigit(p[1]) || p == ":"
            print(io, "[$p]")
        elseif k == 1
            print(io, p)
        else
            print(io, ".", p)
        end
    end
    return String(take!(io))
end

"""
    i2u(imas_location::String)

return universal IMAS location from IMAS location
ie. replaces indexes of arrays of structures with [:]
"""
function i2u(imas_location::AbstractString)
    io = IOBuffer()
    i = 1
    len = lastindex(imas_location)

    while i <= len
        c = imas_location[i]
        if c == '['
            close_idx = findnext(']', imas_location, i)
            if isnothing(close_idx)
                error("Unmatched '[' in path: $imas_location")
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