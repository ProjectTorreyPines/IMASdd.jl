"""
    ulocation(@nospecialize(ids::IDS), field::Symbol)

Returns IMAS universal location given IDS and field
"""
@inline function ulocation(@nospecialize(ids::IDS), field::Symbol)
    return string(f2u(ids), ".", field)
end

@inline function ulocation(@nospecialize(ids::DD), field::Symbol)
    return string(field)
end

@inline function ulocation(@nospecialize(ids::Type{<:IDSvectorElement}), field::Symbol)
    return string(fs2u(ids), ".", field)
end

@inline function ulocation(@nospecialize(ids::Type{<:IDS}), field::Symbol)
    return string(fs2u(ids), ".", field)
end

"""
    ulocation(@nospecialize(ids::Union{IDS,IDSvector}))

Returns IMAS universal location of a given IDS
"""
@inline function ulocation(@nospecialize(ids::IDS))
    return f2u(ids)
end

@inline function ulocation(@nospecialize(ids::DD))
    return "dd"
end

@inline function ulocation(@nospecialize(ids::IDSvector))
    return f2u(ids)[1:end-3]
end

"""
    location(@nospecialize(ids::IDS), field::Symbol)

Returns IMAS location of a given IDS and field
"""
@inline function location(@nospecialize(ids::IDS), field::Symbol)
    return string(f2i(ids), ".", field)
end

@inline function location(@nospecialize(ids::DD), field::Symbol)
    return string(field)
end

"""
    location(@nospecialize(ids::Union{IDS,IDSvector}))

Returns IMAS location of a give IDS
"""
@inline function location(@nospecialize(ids::IDS))
    return f2i(ids)
end

@inline function location(@nospecialize(ids::DD))
    return "dd"
end

@inline function location(@nospecialize(ids::IDSvector))
    return f2i(ids)[1:end-3]
end

"""
    f2u(ids)

Returns universal IMAS location of a given IDS
"""
@inline function f2u(@nospecialize(ids::IDS))
    return fs2u(typeof(ids))
end

@inline function f2u(@nospecialize(ids::IDSvector))
    return fs2u(eltype(ids))
end

"""
    fs2u(ids)

Returns universal IMAS location of a given IDS type
"""
@inline function fs2u(@nospecialize(ids::Type{<:DD}))
    return "dd"
end

@inline function fs2u(@nospecialize(ids::Type{<:IDS}))
    return fs2u(Base.typename(ids).name)
end

@inline function fs2u(@nospecialize(ids::Type{<:IDSvector}))
    return fs2u(Base.typename(eltype(ids)).name)
end

@inline function fs2u(@nospecialize(ids::Type{<:IDSvectorElement}))
    return string(fs2u(Base.typename(ids).name), "[:]")
end

@inline function fs2u(ids::Symbol)
    return rstrip(replace(string(ids), r"___|__" => s -> s == "___" ? "[:]." : "."), '.')
end

@inline function fs2u(ids::AbstractString)
    if in(':', ids) | in('.', ids)
        error("`$ids` is not a qualified IDS type")
    end
    return fs2u(Symbol(ids))
end

"""
    f2fs(@nospecialize(ids::IDS))

return IDS type as a string
"""
@inline function f2fs(@nospecialize(ids::IDS))
    return string(Base.typename(typeof(ids)).name)
end

@inline function f2fs(@nospecialize(ids::IDSvector))
    return string(Base.typename(eltype(ids)).name)
end

@inline function f2fs(@nospecialize(ids::IDSvectorElement))
    return string(Base.typename(typeof(ids)).name, "___")
end

"""
    f2p(@nospecialize(ids::Union{IDS,IDSvector}))

Return parsed IMAS path of a given IDS
NOTE: indexes of arrays of structures that cannot be determined are set to 0
"""
function f2p(@nospecialize(ids::Union{IDS,IDSvector}))
    # initialize path
    if typeof(ids) <: DD
        name = "dd"
    elseif typeof(ids) <: IDSvectorElement
        name = string(Base.typename(typeof(ids)).name) * "___"
    elseif typeof(ids) <: IDSvector
        name = string(Base.typename(eltype(ids)).name) * "___"
    elseif typeof(ids) <: IDS
        name = string(Base.typename(typeof(ids)).name)
    end
    name = replace(name, "___" => "__:__")
    path_gen = (k == ":" ? "0" : string(k) for k in split(name, "__") if length(k) > 0)

    # collect integers for arrays of structures
    N = count(":", name)
    idx = zeros(Int, N)
    h = ids
    child = nothing
    k = N
    while k > 0 && typeof(h) <: Union{IDS,IDSvector}
        if typeof(h) <: IDSvector
            idx[k] = child === nothing ? 0 : index(child)
            k -= 1
        end
        child = h
        h = parent(h)
    end

    # apply indexes
    output_gen = (
        begin
            if isdigit(p[1]) && !isempty(idx)
                string(popfirst!(idx))
            else
                p
            end
        end for p in path_gen
    )

    return output_gen
end

"""
    f2p_name(ids)

Returns string with name of current IDS
"""
@inline function f2p_name(ids)
    return f2p_name(ids, parent(ids))
end

@inline function f2p_name(ids::DD, ::Nothing)
    return ("dd",)
end

@inline function f2p_name(@nospecialize(ids::IDS), @nospecialize(parent::IDS))
    return (rsplit(string(Base.typename(typeof(ids)).name), "__")[end],)
end

@inline function f2p_name(@nospecialize(ids::IDS), ::Nothing)
    return reverse!(split(replace(ulocation(ids), "[:]" => ".0"), "."))
end

@inline function f2p_name(@nospecialize(ids::IDSvector), ::Nothing)
    return ("",)
end

@inline function f2p_name(@nospecialize(ids::IDSvectorElement), @nospecialize(parent::IDSvector))
    return (string(index(ids)),)
end

@inline function f2p_name(@nospecialize(ids::IDSvector), @nospecialize(parent::IDS))
    return (rsplit(string(Base.typename(eltype(ids)).name), "__")[end],)
end

"""
    f2i(@nospecialize(ids::Union{IDS,IDSvector}))

return IMAS location of a given IDS
"""
@inline function f2i(@nospecialize(ids::Union{IDS,IDSvector}))
    return p2i(f2p(ids))
end

"""
    i2p(imas_location::AbstractString)

return parsed IMAS path (ie. splits IMAS location in its elements)
"""
@inline function i2p(imas_location::AbstractString)
    gen = (
        begin
            if in('[', k)
                s, n = split(k, '[')
                n = strip(n, ']')
                (s, n)  # Yield a tuple of strings
            else
                (k,)  # Yield a single-element tuple
            end
        end for k in split(imas_location, '.') if length(k) > 0
    )

    return collect(Iterators.flatten(gen))
end

"""
    p2i(path::Union{AbstractVector{<:AbstractString},Base.Generator})

Combine list of IMAS location elements into a string
"""
@inline function p2i(path::Union{AbstractVector{<:AbstractString},Base.Generator})
    gen = (
        begin
            if isdigit(p[1]) || p == ":"
                "[$p]"
            elseif k == 1
                p
            else
                ".$p"
            end
        end for (k, p) in enumerate(path)
    )

    return join(gen)
end

"""
    i2u(imas_location::String)

return universal IMAS location from IMAS location
ie. replaces indexes of arrays of structures with [:]
"""
@inline function i2u(imas_location::AbstractString)
    return join(s == "" ? "[:]" : s for s in split(imas_location, r"\[\d+\]"))
end

"""
    u2fs(imas_location::String)

return IDS/IDSvector type as a string starting from a universal IMAS location string
"""
@inline function u2fs(imas_location::AbstractString)
    return replace(imas_location, "[:]." => "___", "[:]" => "___", "." => "__")
end

"""
    u2f(imas_location::String)

return IDS/IDSvector type starting from a universal IMAS location string
"""
@inline function u2f(imas_location::AbstractString)
    return eval(Meta.parse(u2fs(imas_location)))
end