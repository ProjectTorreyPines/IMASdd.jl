const _pattern_intvec = r"\[[0-9]+\]"

"""
    ulocation(@nospecialize(ids::IDS), field::Symbol)

Returns IMAS universal location given IDS and field
"""
function ulocation(@nospecialize(ids::IDS), field::Symbol)
    return "$(f2u(ids)).$(field)"
end

function ulocation(@nospecialize(ids::DD), field::Symbol)
    return "$(field)"
end

function ulocation(@nospecialize(ids::Type{<:IDSvectorElement}), field::Symbol)
    return "$(fs2u(ids))[:].$(field)"
end

function ulocation(@nospecialize(ids::Type{<:IDS}), field::Symbol)
    return "$(fs2u(ids)).$(field)"
end

function ulocation(@nospecialize(ids::Union{IDS,IDSvector}))
    return f2u(ids)
end

"""
    location(@nospecialize(ids::IDS), field::Symbol)

Returns IMAS location of a given IDS and field
"""
function location(@nospecialize(ids::IDS), field::Symbol)
    return "$(f2i(ids)).$(field)"
end

function location(@nospecialize(ids::DD), field::Symbol)
    return "$(field)"
end

"""
    location(@nospecialize(ids::Union{IDS,IDSvector}))

Returns IMAS location of a give IDS
"""
function location(@nospecialize(ids::Union{IDS,IDSvector}))
    return f2i(ids)
end

"""
    f2u(ids)

Returns universal IMAS location of a given IDS
"""
function f2u(@nospecialize(ids::IDS))
    return fs2u(typeof(ids))
end

function f2u(@nospecialize(ids::IDSvector))
    return fs2u(eltype(ids)) * "[:]"
end

function f2u(@nospecialize(ids::IDSvectorElement))
    return fs2u(typeof(ids)) * "[:]"
end

function fs2u(@nospecialize(ids::Type{<:IDS}))
    return fs2u(Base.typename(ids).name)
end

function fs2u(@nospecialize(ids::Type{<:IDSvectorElement}))
    return fs2u(Base.typename(ids).name)
end

function fs2u(ids::Symbol)
    return rstrip(replace(string(ids), "___" => "[:].", "__" => "."), '.')
end

function fs2u(ids::AbstractString)
    if in(':', ids) | in('.', ids)
        error("`$ids` is not a qualified IDS type")
    end
    return fs2u(Symbol(ids))
end

"""
    f2fs(@nospecialize(ids::IDS))

return IDS type as a string
"""
function f2fs(@nospecialize(ids::IDS))
    return d2fs(typeof(ids))
end

function f2fs(@nospecialize(ids::IDSvector))
    return d2fs(eltype(ids))
end

function f2fs(@nospecialize(ids::IDSvectorElement))
    return d2fs(typeof(ids)) * "___"
end

function d2fs(@nospecialize(ids::Type{<:IDS}))
    return string(Base.typename(ids).name)
end

function d2fs(@nospecialize(ids::Type{<:IDSvectorElement}))
    return string(Base.typename(ids).name)
end

"""
    f2p(@nospecialize(ids::Union{IDS,IDSvector}))

Return parsed IMAS path of a given IDS
NOTE: indexes of arrays of structures that cannot be determined are set to 0
"""
function f2p(@nospecialize(ids::Union{IDS,IDSvector}))
    # initialize path
    if typeof(ids) <: IDS
        if typeof(parent(ids)) <: IDSvector
            name = string(Base.typename(typeof(ids)).name) * "___"
        else
            name = string(Base.typename(typeof(ids)).name)
        end
    elseif typeof(ids) <: IDSvector
        name = string(Base.typename(eltype(ids)).name) * "___"
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
    f2i(@nospecialize(ids::Union{IDS,IDSvector}))

return IMAS location of a given IDS
"""
function f2i(@nospecialize(ids::Union{IDS,IDSvector}))
    return p2i(f2p(ids))
end

"""
    i2p(imas_location::AbstractString)

return parsed IMAS path (ie. splits IMAS location in its elements)
"""
function i2p(imas_location::AbstractString)
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
    p2i(path::Union{AbstractVector{<:String},Base.Generator})

Combine list of IMAS location elements into a string
"""
function p2i(path::Union{AbstractVector{<:String},Base.Generator})
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
function i2u(imas_location::AbstractString)
    return replace(imas_location, _pattern_intvec => "[:]") # r"\[[0-9]+\]"
end

"""
    u2fs(imas_location::String)

return IDS/IDSvector type as a string starting from a universal IMAS location string
"""
function u2fs(imas_location::AbstractString)
    return replace(imas_location, "[:]." => "___", "[:]" => "___", "." => "__")
end

"""
    u2f(imas_location::String)

return IDS/IDSvector type starting from a universal IMAS location string
"""
function u2f(imas_location::AbstractString)
    return eval(Meta.parse(u2fs(imas_location)))
end