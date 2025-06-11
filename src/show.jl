struct IMASnodeRepr{T<:Real}
    ids::Union{IDS{T},IDSvector{T}}
    field::Symbol
    value::Any
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
    return printstyled(io, f2p_name(ids)[end]; bold=true)
end

function AbstractTrees.printnode(io::IO, @nospecialize(ids::IDSvector))
    return printstyled(io, f2p_name(ids)[1]; bold=true)
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
            if length(value) < 80
                printstyled(io, "\"$(value)\""; color)
            else
                printstyled(io, "\"$(value[1:30])"; color)
                printstyled(io, " ...$(length(value)) chars... "; color, bold=true)
                printstyled(io, "$(value[end-30:end])\""; color)
            end
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
                    # Use IOBuffer for more efficient array formatting
                    array_io = IOBuffer()
                    print(array_io, "[")
                    for (i, v) in enumerate(value)
                        if i > 1
                            print(array_io, ",")
                        end
                        @printf(array_io, "%g", v)
                    end
                    print(array_io, "]")
                    printstyled(io, String(take!(array_io)); color)
                else
                    printstyled(io, "$value"; color)
                end
            else
                printstyled(io, "$(Base.summary(value))"; color)
                if eltype(value) <: AbstractFloat
                    flag_statistics = true
                end
            end
        else
            color = :purple
            printstyled(io, "$(Base.summary(value))"; color)
        end
        u = units(ids, field)
        if !(isempty(u) || u == "-")
            printstyled(io, " [$u]"; color, bold=true)
        end

        if flag_statistics
            print(io, "\n")
            # More efficient spacing calculation - avoid string() concatenation
            spacing_length = length(string(field)) + 3  # " ➡ " is 3 characters
            print(io, " "^spacing_length)
            if all(isnan.(value))
                printstyled(io, "all:"; color, bold=true)
                print(io, "NaN")
            elseif sum(abs, value .- value[1]) == 0.0
                printstyled(io, "all:"; color, bold=true)
                print(io, @sprintf("%.3g   ", value[1]))
            else
                printstyled(io, "min:"; color, bold=true)
                print(io, @sprintf("%.3g   ", nanminimum(value)))
                printstyled(io, "avg:"; color, bold=true)
                print(io, @sprintf("%.3g   ", sum(x -> isnan(x) ? 0.0 : x, value) / sum(x -> isnan(x) ? 0 : 1, value)))
                printstyled(io, "max:"; color, bold=true)
                print(io, @sprintf("%.3g", nanmaximum(value)))
                if any(isnan.(value))
                    printstyled(io, "   NaNs:"; color, bold=true)
                    print(io, sum(isnan.(value)))
                end
            end
        end
    end
end

function Base.show(io::IO, @nospecialize(ids_arr::AbstractArray{<:IDS}); maxdepth::Int=1000, kw...)
    for (i, ids) in enumerate(ids_arr)
        print(io, "\n" * "="^15 * " Item #$(i) " * "="^15 * "\n")
        AbstractTrees.print_tree(io, ids; maxdepth, kw...)
    end
    return
end

function Base.show(io::IO, @nospecialize(ids_arr::AbstractArray{<:IDSvector}); maxdepth::Int=1000, kw...)
    for (i, ids) in enumerate(ids_arr)
        print(io, "\n" * "="^15 * " Item #$(i) " * "="^15 * "\n")
        AbstractTrees.print_tree(io, ids; maxdepth, kw...)
    end
    return
end

function Base.show(io::IO, @nospecialize(ids::Union{IDS,IDSvector}); maxdepth::Int=1000, kw...)
    return AbstractTrees.print_tree(io, ids; maxdepth, kw...)
end

function Base.show(io::IO, ids::DD; maxdepth::Int=1, kw...) # only depth 1 for dd
    return AbstractTrees.print_tree(io, ids; maxdepth, kw...)
end

# show function for the Jupyter notebook
function Base.show(io::IO, ::MIME"text/plain", @nospecialize(ids::Union{IDS,IDSvector}); maxdepth::Int=1000, kw...)
    return show(io, ids; maxdepth, kw...)
end

function Base.show(io::IO, ::MIME"text/plain", ids::DD; maxdepth::Int=1, kw...) # only depth 1 for dd
    return show(io, ids; maxdepth, kw...)
end

function Base.show(io::IO, ::MIME"text/plain", @nospecialize(ids_arr::AbstractArray{<:IDS}); maxdepth::Int=1000, kw...)
    return show(io, ids_arr; maxdepth, kw...)
end

function Base.show(io::IO, ::MIME"text/plain", @nospecialize(ids_arr::AbstractArray{<:IDSvector}); maxdepth::Int=1000, kw...)
    return show(io, ids_arr; maxdepth, kw...)
end

# show function for inline prints
function Base.show(io::IO, @nospecialize(ids::IDS))
    # Use IOBuffer for more efficient string building
    content_io = IOBuffer()
    keys_iter = keys_no_missing(ids)
    for (i, k) in enumerate(keys_iter)
        if i > 1
            print(content_io, ", ")
        end
        print(content_io, k)
    end
    content = String(take!(content_io))
    return print(io, "$(f2i(ids)){$content}")
end

function Base.show(@nospecialize(ids::Union{IDS,IDSvector}), maxdepth; kw...)
    return AbstractTrees.print_tree(ids; maxdepth, kw...)
end

# show function for inline prints
function Base.show(io::IO, @nospecialize(ids::IDSvector))
    base_path = p2i(f2p(ids)[1:end-1])
    if length(ids) < 2
        # Use IOBuffer for more efficient index range formatting
        indices_io = IOBuffer()
        for i in 1:length(ids)
            if i > 1
                print(indices_io, ", ")
            end
            print(indices_io, i)
        end
        indices_str = String(take!(indices_io))
        return println(io, "$(base_path)[$(indices_str)]")
    else
        return println(io, "$(base_path)[1...$(length(ids))]")
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
    if !contains(nfo.data_type, "STRUCT")
        printstyled(io, "{$(nfo.data_type)}"; color=248)
        M += length("{$(nfo.data_type)}")
    end
    if !isempty(nfo.documentation)
        print(io, " ")
        printstyled(io, word_wrap(nfo.documentation, wrap_length; i=wrap_length - M); color=248)
    end
end

function AbstractTrees.printnode(io::IO, leaf::IMASstructRepr; kw...)
    nfo = info(leaf.location)
    print_formatted_node(io, string(leaf.field), nfo; color=:red, bold=false)
    return nothing
end

function AbstractTrees.printnode(io::IO, @nospecialize(ids_type::Type{<:IDS}); kw...)
    nfo = info(fs2u(ids_type))
    nodename = replace(split(split("$ids_type", "___")[end], "__")[end], "IMASdd." => "", r"{\w+}" => "")
    print_formatted_node(io, nodename, nfo; color=:black, bold=true)
    return nothing
end

function AbstractTrees.printnode(io::IO, @nospecialize(ids_type::Type{<:IDSvector}); kw...)
    nfo = info(fs2u(eltype(ids_type)))
    nodename = replace(split(split("$(eltype(ids_type))", "___")[end], "__")[end] * "[:]", r"\{.*\}" => "")
    print_formatted_node(io, nodename, nfo; color=:black, bold=true)
    return nothing
end

function AbstractTrees.children(@nospecialize(ids_type::Type{<:IDS}); kw...)
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
            uloc = fs2u(Symbol("$(Base.typename(ids_type).name)___$field"), field_type)
            push!(tmp, IMASstructRepr(ids_type, field, uloc, field_type))
        end
    end
    return tmp
end

function AbstractTrees.children(@nospecialize(ids_type::Type{T}); kw...) where {T<:IDSvector}
    return AbstractTrees.children(eltype(ids_type); kw...)
end

function AbstractTrees.Leaves(@nospecialize(ids_type::Type{<:IDS}))
    tmp = IMASstructRepr[]
    for (field, field_type) in zip(fieldnames(ids_type), fieldtypes(ids_type))
        if field ∈ private_fields || field === :global_time
            continue
        elseif field_type <: Union{IDS,IDSvector}
            append!(tmp, AbstractTrees.Leaves(field_type))
        else
            uloc = fs2u(Symbol("$(Base.typename(ids_type).name)___$field"), field_type)
            push!(tmp, IMASstructRepr(ids_type, field, uloc, field_type))
        end
    end
    return tmp
end

function AbstractTrees.Leaves(@nospecialize(ids_type::Type{T}); kw...) where {T<:IDSvector}
    return AbstractTrees.Leaves(eltype(ids_type); kw...)
end

function help(@nospecialize(ids_type::Type{T}); maxdepth::Int=1000, kw...) where {T<:Union{IDS,IDSvector}}
    return AbstractTrees.print_tree(ids_type; maxdepth, kw...)
end

function help(@nospecialize(ids::Union{IDS,IDSvector}); maxdepth::Int=1000, kw...)
    return help(typeof(ids); maxdepth, kw...)
end

export help

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