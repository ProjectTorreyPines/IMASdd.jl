#= ======= =#
#  findall  #
#= ======= =#
function _set_conditions(@nospecialize(ids::IDS), conditions::Pair{String}...)
    for (path, value) in conditions
        h = ids
        for p in i2p(path)
            if isdigit(p[1])
                n = parse(Int, p)
                if n > length(h)
                    resize!(h, n)
                end
                h = h[n]
            else
                p = Symbol(p)
                if ismissing(h, p)
                    setproperty!(h, p, value)
                end
                h = getproperty(h, p)
            end
        end
    end
    return ids
end

function Base.findall(@nospecialize(ids::IDSvector), condition::Pair{String}, conditions::Pair{String}...)
    conditions = vcat(condition, collect(conditions))
    if isempty(ids)
        return eltype(ids)[]
    end
    matches = _match(ids, conditions)
    return values(matches)
end

function _match(@nospecialize(ids::IDSvector), conditions)
    matches = Dict()
    for (k, item) in enumerate(ids)
        match = true
        for (path, value) in conditions
            h = item
            for p in i2p(path)
                if isdigit(p[1])
                    n = parse(Int, p)
                    if n > length(h)
                        match = false
                        break
                    end
                    h = h[n]
                else
                    p = Symbol(p)
                    if ismissing(h, p)
                        match = false
                        break
                    end
                    h = getproperty(h, p)
                end
            end
            if h != value
                match = false
                break
            end
        end
        if match
            matches[k] = item
        end
    end
    return matches
end

Base.@kwdef struct IDS_Field_Finder
    root_ids::Union{IDS,IDSvector} # Start point of the search
    parent_ids::Union{IDS,IDSvector} # Parent IDS of target field
    field::Symbol # Target field symbol
    field_type::Type # Type of the field
    root_name::String # Name of root (default = location(root_ids))
    field_path::String # Full path from root_ids to the field
end

function Base.getproperty(instance::IDS_Field_Finder, prop::Symbol)
    if prop == :value
        return getfield(instance.parent_ids, instance.field)  # Lazily evaluate `value`
    else
        return getfield(instance, prop)  # Default behavior for other fields
    end
end

function Base.show(io::IO, ::MIME"text/plain", IFF_list::AbstractArray{IDS_Field_Finder})
    for (k, IFF) in pairs(IFF_list)
        printstyled(io, "[$k] "; color=:lightblack)
        show(io, MIME"text/plain"(), IFF)
        print(io, "\n")
    end
end

function Base.show(io::IO, ::MIME"text/plain", IFF::IDS_Field_Finder)
    root_name = IFF.root_name
    rest_part = replace(IFF.field_path, root_name => "")
    rest_part = replace(rest_part, String(IFF.field) => "")

    parent_name = location(IFF.parent_ids)

    printstyled(io, root_name; color=:red)

    print(io, rest_part)

    printstyled(io, String(IFF.field); color=:green, bold=true)

    unit = units(IFF.parent_ids, IFF.field)
    if !(isempty(unit) || unit == "-")
        printstyled(io, " [$unit]"; color=:blue, bold=true)
    end
    value = IFF.value
    print(io, " [$(Base.summary(value))]")

    if typeof(value) <: Union{AbstractArray{<:Real},Real} && length(value) > 0
        color = :blue
        print(io, " (")
        if sum(abs, value .- value[1]) == 0.0
            printstyled(io, "all:"; color)
            print(io, @sprintf("%.3g", value[1]))
        else
            printstyled(io, "min:"; color)
            print(io, @sprintf("%.3g, ", minimum(value)))
            printstyled(io, "avg:"; color)
            print(io, @sprintf("%.3g, ", sum(value) / length(value)))
            printstyled(io, "max:"; color)
            print(io, @sprintf("%.3g", maximum(value)))
        end
        print(io, ")")
    elseif value isa String
        print(io, " (")
        max_length = 20
        if length(value) > max_length
            half_len = div(max_length - 5, 2)
            value = value[1:half_len] * " ... " * value[end-half_len+1:end]
        end
        printstyled(io, "\"" * value * "\""; color=:red)
        print(io, ")")
    end
end

"""
    @findall ids :symbol
    @findall ids r"Regular Expression"
    @findall [ids1, ids2] [:sybmol1, :symbol2]
    @findall [ids1, ids2] r"Regular Expression"
Searches for specified fields within single/multiple IDS objects, while capturing their names into IDS_Field_Finder.root_name

See also [`findall(root_ids::Union{IDS,IDSvector}, target::Union{Symbol,AbstractArray{Symbol},Regex}=r""; kwargs)`](@ref) which this macro calls after expansion.


# Arguments
- `root_ids:: Root IDS objects to search.
- `target_fields::Union{Symbol, AbstractArray{Symbol}, Regex}: Fields to search for, specified by a single symbol, array of symbols, or regular expression.

# Returns
- `Vector{IDS_Field_Finder}`: A vector of `IDS_Field_Finder` structures, each containing details on a located field such as parent IDS, root IDS, field type, and full field path.

# Example
```julia-repl
julia> @findall [dd1, dd2] [:psi, :j_tor]
julia> @findall [dd1, dd2] r"psi"

julia> eqt = dd.equilibrium.time_slice[]

julia> @findall eqt :psi
julia> @findall eqt r"global.*psi"
```
"""
macro findall(root_ids, target_fields)
    is_vector_expr(expr) = expr isa Expr && expr.head == :vect

    if is_vector_expr(root_ids)
        # multiple root_ids objects
        elements = root_ids.args

        checks = [:(isa($(esc(elem)), Union{IDS,IDSvector}) || throw(ArgumentError("Invalid type in root_ids: must be IDS or IDSvector"))) for elem in elements]

        return quote
            begin
                $(Expr(:block, checks...))
                IFF_list = Vector{IDS_Field_Finder}()
                for (elm, name) in zip($(esc(root_ids)), $elements)
                    append!(IFF_list, findall(elm, $(esc(target_fields)); root_name=string(name)))
                end
                IFF_list
            end
        end
    else
        # single root_ids object
        check = :(isa($(esc(root_ids)), Union{IDS,IDSvector}) || throw(ArgumentError("Invalid type in root_ids: must be IDS or IDSvector")))

        root_name_str = string(root_ids)

        return quote
            begin
                $check
                findall($(esc(root_ids)), $(esc(target_fields)); root_name=$(esc(root_name_str)))
            end
        end
    end
end

export @findall
push!(document[:Base], Symbol("@findall"))

"""
    findall(ids::Union{IDS, IDSvector}, target_fields::Union{Symbol,AbstractArray{Symbol},Regex}=r""; include_subfields::Bool=true)
Searches for specified fields within IDS objects, supporting nested field exploration and customizable filtering.

# Arguments
- `root_ids::Union{IDS, IDSvector}`: Root IDS objects to search.
- `target_fields::Union{Symbol, AbstractArray{Symbol}, Regex} = r""`: Fields to search for, specified by a single symbol, array of symbols, or regular expression.
- `include_subfields::Bool = true`: If `true`, retrieves nested fields below the target field when found; if `false`, stops at the matching field.

# Returns
- `Vector{IDS_Field_Finder}`: A vector of `IDS_Field_Finder` structures, each containing details on a located field such as parent IDS, root IDS, field type, and full field path.

# Example
```julia-repl
julia> findall(dd.equilibrium.time_slice[].global_quantities) # By default, it searches everything under given IDS
julia> findall(dd.equilibrium.time_slice[].global_quantities, r"") # Same behavior (Default)

# Find fields matching a single symbol within a IDS structure
julia> IFF = findall(dd.equilibrium.time_slice, :psi)

# Search for multiple symbols within multiple root IDS objects
julia> IFF = findall([dd.equilibrium, dd.core_profiles], [:psi, :j_tor])

# Use regular expressions for flexible and powerful search patterns
julia> IFF = findall(dd, r"prof.*1d.*psi")

# Control subfield inclusion using the `include_subfields` keyword
julia> IFF = findall(dd, r"prof.*2d"; include_subfields=false)
julia> IFF = findall(dd, r"prof.*2d"; include_subfields=true) # Default behavior

# Default show for IFF (IDF_Field_Finder) structure
julia> IFF

# Retrieve actual values of found IDS objects (lazy evaluation)
julia> IFF[1].value
julia> IFF[end].value
```
"""
function Base.findall(root_ids::Union{IDS,IDSvector}, target::Union{Symbol,AbstractArray{Symbol},Regex}=r""; include_subfields::Bool=true, root_name::String="")
    root_name = isempty(root_name) ? location(root_ids) : root_name

    IFF_list = Vector{IDS_Field_Finder}()

    flag_found = false
    stack = Vector{Tuple{Union{IDS,IDSvector,Vector{IDS}},String,Bool}}()  # Stack initialization
    sizehint!(stack, 1000)

    if root_ids isa IDSvector
        parent_ids = (root_ids._parent).value
        push!(stack, (parent_ids, location(parent_ids), false))
    else
        push!(stack, (root_ids, root_name, false))
    end

    # helper function
    function is_target_found(ids::Union{IDS,IDSvector}, field::Symbol, path::String, target::Regex)
        return occursin(target, path)
    end
    function is_target_found(ids::Union{IDS,IDSvector}, field::Symbol, path::String, target::Symbol)
        return field == target
    end
    function is_target_found(ids::Union{IDS,IDSvector}, field::Symbol, path::String, target::AbstractArray{Symbol})
        return field in target
    end

    while !isempty(stack)
        ids, path, parent_found = pop!(stack)

        fields = filter(x -> x ∉ IMASdd.private_fields, fieldnames(typeof(ids)))

        for field in fields
            child = getfield(ids, field)

            isempty(child) ? continue : nothing

            if typeof(child) <: Union{IDSvector,Vector{IDS}}
                for (k, grand_child) in pairs(child)
                    new_path = path * "." * String(field) * "[$k]"
                    flag_found = parent_found ? true : is_target_found(ids, field, new_path, target)
                    if include_subfields || !flag_found
                        push!(stack, (grand_child, new_path, flag_found))
                    end
                end
            elseif typeof(child) <: IDS
                new_path = path * "." * String(field)
                flag_found = parent_found ? true : is_target_found(ids, field, new_path, target)
                if include_subfields || !flag_found
                    push!(stack, (child, new_path, flag_found))
                end
            else
                new_path = path * "." * String(field)
                flag_found = parent_found ? true : is_target_found(ids, field, new_path, target)
            end

            if flag_found
                if !ismissing(ids, field)
                    push!(IFF_list,
                        IDS_Field_Finder(;
                            parent_ids=ids,
                            root_ids,
                            field,
                            field_type=fieldtype(typeof(ids), field),
                            root_name,
                            field_path=new_path)
                    )
                end
                flag_found = false
            end
        end
    end

    # Considering that stack is (Last-In, First-Out),
    # reverse the IFF_list to make it is in the order of given input
    if length(IFF_list) == 1
        return IFF_list[1]
    else
        return reverse!(IFF_list)
    end
end

push!(document[:Base], :findall)