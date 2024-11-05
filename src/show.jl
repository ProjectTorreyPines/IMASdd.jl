struct IMASnodeRepr
    ids::Union{IDS,IDSvector}
    field::Symbol
    value::Any
    # internal constructor to avoid specialization on IMASnodeRepr
    IMASnodeRepr(@nospecialize(ids::Union{IDS,IDSvector}), field::Symbol, value::Any) = begin
        return new(ids, field, value)
    end
end

function AbstractTrees.children(@nospecialize(ids::IDS))
    ns = NoSpecialize(ids)
    return (IMASnodeRepr(ns.ids, field, getraw(ns.ids, field)) for field in keys_no_missing(ns.ids))
end

function AbstractTrees.children(@nospecialize(ids::IDSvector))
    return ids
end

function AbstractTrees.children(node_value::IMASnodeRepr)
    value = node_value.value
    if typeof(value) <: IDS
        ns = NoSpecialize(value)
        return (IMASnodeRepr(ns.value, field, getraw(ns.value, field)) for field in keys_no_missing(ns.value))
    elseif typeof(value) <: IDSvector && eltype(value) <: IDSvectorRawElement
        n = 5
        if length(value) > n * 3
            return [[value[k] for k in 1:n]; Val(:...); [value[k] for k in length(value)-n:length(value)]]
        else
            return value
        end
    elseif typeof(value) <: IDSvector
        return value
    else
        return []
    end
end

function AbstractTrees.printnode(io::IO, ::Val{:...})
    return printstyled(io, "...\n"; bold=true)
end

function AbstractTrees.printnode(io::IO, @nospecialize(ids::IDS))
    path = collect(f2p(ids))
    return printstyled(io, path[end]; bold=true)
end

function AbstractTrees.printnode(io::IO, @nospecialize(ids::IDSvector))
    path = collect(f2p(ids))
    return printstyled(io, path[end-1]; bold=true)
end

function AbstractTrees.printnode(io::IO, node_value::IMASnodeRepr)
    ids = node_value.ids
    field = node_value.field
    value = node_value.value

    flag_statistics = false

    if typeof(value) <: IDS
        printstyled(io, field; bold=true)
    elseif typeof(value) <: IDSvector
        printstyled(io, field; bold=true)
    else
        printstyled(io, field)
        printstyled(io, " ➡ "; color=:red)
        if typeof(value) <: Function
            color = :blue
            printstyled(io, "Function"; color)
        elseif typeof(value) <: String
            color = :magenta
            printstyled(io, "\"$(value)\""; color)
        elseif typeof(value) <: Integer
            color = :yellow
            printstyled(io, "$(value)"; color)
        elseif typeof(value) <: Union{Float16,Float32,Float64}
            color = :red
            printstyled(io, @sprintf("%g", value); color)
        elseif typeof(value) <: AbstractFloat
            color = :red
            printstyled(io, repr(value); color)
        elseif typeof(value) <: AbstractArray
            color = :green
            if length(value) < 5
                if eltype(value) <: AbstractFloat
                    printstyled(io, "[$(join([@sprintf("%g",v) for v in value],","))]"; color)
                else
                    printstyled(io, "$value"; color)
                end
            else
                printstyled(io, "$(Base.summary(value))"; color)
                flag_statistics = true
            end
        else
            color = :purple
            printstyled(io, "$(Base.summary(value))"; color)
        end
        u = units(ids, field)
        if !(isempty(u) || u == "-")
            printstyled(io, " [$u]"; color, bold=true)
        end
        if typeof(value) <: AbstractArray && length(value) >= 5 && sum(abs, value .- value[1]) == 0.0
            color = :green
            printstyled(io, " (all $(value[1]))"; color)
        end

        if (flag_statistics)
            print(io, "\n")
            print(io, " "^length(string(field) * " ➡ "))

            color = :blue

            printstyled(io, "(min):"; color, bold=true)
            print(io, @sprintf("%.3g, ", minimum(value)))
            printstyled(io, "(avg):"; color, bold=true)
            print(io, @sprintf(":%.3g, ", sum(value) / length(value)))
            printstyled(io, "(max):"; color, bold=true)
            print(io, @sprintf("%.3g ", maximum(value)))
        end
    end
end

function Base.show(io::IO, @nospecialize(ids::Union{IDS,IDSvector}); maxdepth::Int=1000, kwargs...)
    return AbstractTrees.print_tree(io, ids; maxdepth, kwargs...)
end

# show function for the Jupyter notebook
function Base.show(io::IO, ::MIME"text/plain", @nospecialize(ids::Union{IDS,IDSvector}); maxdepth::Int=1000, kwargs...)
    return show(io, ids; maxdepth, kwargs...)
end

# show function for inline prints
function Base.show(io::IO, @nospecialize(ids::IDS))
    content = join((string(k) for k in keys_no_missing(ids)), ", ")
    return print(io, "$(f2i(ids)){$content}")
end

function Base.show(@nospecialize(ids::Union{IDS,IDSvector}), maxdepth; kw...)
    return AbstractTrees.print_tree(ids; maxdepth, kw...)
end

# show function for inline prints
function Base.show(io::IO, @nospecialize(ids::IDSvector))
    if length(ids) < 2
        return println(io, "$(p2i(collect(f2p(ids))[1:end-1]))[$(join(string.(1:length(ids)),", "))]")
    else
        return println(io, "$(p2i(collect(f2p(ids))[1:end-1]))[1...$(length(ids))]")
    end
end

# ================================== #
# to print the data structure itself #
# ================================== #
struct IMASstructRepr
    ids_type::Type
    field::Symbol
    location::String
    field_type::Type
end

const wrap_length::Int = 120

function print_formatted_node(io::IO, nodename::String, nfo::Info; color::Symbol, bold::Bool)
    printstyled(io, nodename; color, bold)
    M = length(nodename)
    if nfo.units != "-"
        printstyled(io, " [$(nfo.units)]"; color, bold=true)
        M += length(" [$(nfo.units)]")
    end
    if M > wrap_length - 10
        print(io, "\n")
        M = 0
    else
        print(io, " ")
        M += 1
    end
    if !isempty(nfo.documentation)
        print(io, " ")
        printstyled(io, word_wrap(nfo.documentation, wrap_length; i=wrap_length - M); color=248)
    end
end

function AbstractTrees.printnode(io::IO, leaf::IMASstructRepr; kwargs...)
    nfo = info(leaf.location)
    print_formatted_node(io, string(leaf.field), nfo; color=:red, bold=false)
    return nothing
end

function AbstractTrees.printnode(io::IO, @nospecialize(ids_type::Type{<:IDS}); kwargs...)
    nfo = info(fs2u(ids_type))
    nodename = replace(split(split("$ids_type", "___")[end], "__")[end], "IMASdd." => "", r"{\w+}" => "")
    print_formatted_node(io, nodename, nfo; color=:black, bold=true)
    return nothing
end

function AbstractTrees.printnode(io::IO, @nospecialize(ids_type::Type{<:IDSvector}); kwargs...)
    nfo = info(fs2u(eltype(ids_type)))
    nodename = replace(split(split("$(eltype(ids_type))", "___")[end], "__")[end] * "[:]", r"\{.*\}" => "")
    print_formatted_node(io, nodename, nfo; color=:black, bold=true)
    return nothing
end

function AbstractTrees.children(@nospecialize(ids_type::Type{<:IDS}); kwargs...)
    tmp = []
    for (field, field_type) in zip(fieldnames(ids_type), fieldtypes(ids_type))
        if field ∈ private_fields || field === :global_time || endswith(string(field), "_σ")
            continue
        elseif field_type <: IDSvector
            if eltype(field_type) == Any
                push!(tmp, field_type{Float64})
            else
                push!(tmp, field_type)
            end
        elseif field_type <: IDS
            push!(tmp, field_type)
        else
            if ids_type <: IDSvectorElement
                uloc = fs2u("$(d2fs(ids_type))___$field")
            else
                uloc = fs2u("$(d2fs(ids_type))__$field")
            end
            push!(tmp, IMASstructRepr(ids_type, field, uloc, field_type))
        end
    end
    return tmp
end

function AbstractTrees.children(@nospecialize(ids_type::Type{T}); kwargs...) where {T<:IDSvector}
    return AbstractTrees.children(eltype(ids_type); kwargs...)
end

function AbstractTrees.Leaves(@nospecialize(ids_type::Type{<:IDS}))
    tmp = IMASstructRepr[]
    for (field, field_type) in zip(fieldnames(ids_type), fieldtypes(ids_type))
        if field ∈ private_fields || field === :global_time
            continue
        elseif field_type <: Union{IDS,IDSvector}
            append!(tmp, AbstractTrees.Leaves(field_type))
        else
            if ids_type <: IDSvectorElement
                uloc = fs2u("$(d2fs(ids_type))___$field")
            else
                uloc = fs2u("$(d2fs(ids_type))__$field")
            end
            push!(tmp, IMASstructRepr(ids_type, field, uloc, field_type))
        end
    end
    return tmp
end

function AbstractTrees.Leaves(@nospecialize(ids_type::Type{T}); kwargs...) where {T<:IDSvector}
    return AbstractTrees.Leaves(eltype(ids_type); kwargs...)
end

function dddoc(@nospecialize(ids_type::Type{T}); maxdepth::Int=1000, kwargs...) where {T<:Union{IDS,IDSvector}}
    return AbstractTrees.print_tree(ids_type; maxdepth, kwargs...)
end

function Base.show(io::IO, ::MIME"text/plain", @nospecialize(ids_type::Type{T}); maxdepth::Int=1000, kwargs...) where {T<:Union{IDS,IDSvector}}
    return AbstractTrees.print_tree(io::IO, ids_type; maxdepth, kwargs...)
end

"""
    word_wrap(s::String, n=92; i=n, p=1, w=1)

Wraps a string at spaces at `n` characters
"""
function word_wrap(s::String, n=92; i=n, p=1, w=1)
    s = deepcopy(s)
    for c in s
        (i -= 1) < -1 && (i = w - p + n; unsafe_store!(pointer(s, w), 10))
        c == ' ' && (w = p)
        p += 1
    end
    return s
end