import JSON
import HDF5
document[:IO] = Symbol[]

function field_translator_jl2io(field::String)
    if endswith(field, "_σ")
        return "$(field[1:end-2])_error_upper"
    end
    return field
end

function field_translator_jl2io(field::Symbol)
    return Symbol(field_translator_jl2io(string(field)))
end

function field_translator_io2jl(field::String)
    if endswith(field, "_error_upper")
        return "$(field[1:end-12])_σ"
    end
    return field
end

function field_translator_io2jl(field::Symbol)
    return Symbol(field_translator_io2jl(string(field)))
end

#= =============== =#
#  IDS conversions  #
#= =============== =#
# NOTE: for some reason changing `ids_convert` to `Base.convert` (as it should) results in very long compile times
#       It would be nice to know why that's the case and how to avoit it
#function Base.convert(@nospecialize(out_type::Type{<:IDS{T}}), @nospecialize(ids::IDS)) where {T<:Real}
function ids_convert(@nospecialize(out_type::Type{<:IDS{T}}), @nospecialize(ids::IDS)) where {T<:Real}
    in_type = typeof(ids)
    concrete_in_type = Base.typename(in_type).wrapper
    concrete_out_type = Base.typename(out_type).wrapper
    @assert concrete_in_type === concrete_out_type "Cannot convert to a different IDS concrete type"

    S = out_type.parameters[1]

    ids_out = concrete_out_type{S}()
    fill!(ids_out, ids)
    setfield!(ids_out, :_parent, getfield(ids, :_parent))
    return ids_out
end

"""
    convert(el_type::Type{T}, @nospecialize(ids::IDS)) where {T<:Real}

convert an IDS from one eltype to another

eg. convert(Measurements.Measurement{Float64}, dd)
"""
function Base.convert(el_type::Type{T}, @nospecialize(ids::IDS)) where {T<:Real}
    return ids_convert(Base.typename(typeof(ids)).wrapper{el_type}, ids)
end

function Base.convert(el_type::Type{T}, @nospecialize(idsv::IDSvector)) where {T<:Real}
    tmp = [ids_convert(Base.typename(typeof(ids)).wrapper{el_type}, ids) for ids in idsv]
    out = Base.typename(typeof(idsv)).wrapper{eltype(tmp)}()
    append!(out, tmp)
    return out
end

#= ======================= =#
#  dict2imas and imas2dict  #
#= ======================= =#
"""
    get_frozen_strict_property(@nospecialize(ids::IDS), field::Symbol; freeze::Bool, strict::Bool)

getproperty but with handling of `freeze` and `strict` logic
"""
function get_frozen_strict_property(@nospecialize(ids::IDS), field::Symbol; freeze::Bool, strict::Bool)

    if strict && info(ulocation(ids, field)).extra
        return missing
    end

    if freeze
        value = getproperty(ids, field, missing)
    else
        value = getraw(ids, field)
    end

    return value
end

function get_frozen_strict_property(@nospecialize(ids::DD), field::Symbol; freeze::Bool, strict::Bool)
    if strict && info(ulocation(ids, field)).extra
        return missing
    end
    return getraw(ids, field)
end

"""
    dict2imas(dct::AbstractDict, @nospecialize(ids::IDS); verbose::Bool=false)

Populate IMAS data structure `ids` based on data contained in Julia dictionary `dct`.

# Arguments

  - `verbose::Bool=false`: print structure hierarchy as it is filled
"""
function dict2imas(dct::AbstractDict, @nospecialize(ids::T); error_on_missing_coordinates::Bool=true, verbose::Bool=false) where {T<:IDS}
    if error_on_missing_coordinates
        dict2imas(dct, ids, String[]; skip_non_coordinates=true, error_on_missing_coordinates, verbose)
        dict2imas(dct, ids, String[]; skip_non_coordinates=false, error_on_missing_coordinates, verbose)
    else
        dict2imas(dct, ids, String[]; skip_non_coordinates=false, error_on_missing_coordinates, verbose)
    end
    return ids
end

export dict2imas
push!(document[:IO], :dict2imas)

function dict2imas(
    dct::AbstractDict,
    ids::T,
    path::Vector{<:AbstractString};
    skip_non_coordinates::Bool,
    error_on_missing_coordinates::Bool,
    verbose::Bool) where {T<:IDS}

    # Initialize stack with tuples: (current dictionary, IDS structure, path, current depth level)
    stack = Vector{Tuple{AbstractDict,IDS,Vector{<:AbstractString},Int}}()

    sizehint!(stack, 1000)
    push!(stack, (dct, ids, path, 0))

    while !isempty(stack)
        # Pop the top element of the stack
        (current_dct, current_ids, current_path, level) = pop!(stack)

        # Traverse each key-value pair in the current dictionary
        for (_iofield_, value) in current_dct
            # Handle both Dict{Symbol,Any} and Dict{String,Any} keys
            iofield_string = string(_iofield_)
            iofield = Symbol(iofield_string)
            field_string = field_translator_io2jl(iofield_string)
            field = field_translator_io2jl(iofield)

            # If the IDS structure does not contain this field, skip it if needed
            if !hasfield(typeof(current_ids), field)
                if !skip_non_coordinates
                    @warn("$(location(current_ids, field)) was skipped in dict2imas")
                end
                continue
            end

            # Retrieve the target type of the field
            target_type = fieldtype_typeof(current_ids, field)

            if target_type <: IDS
                # Nested structure
                if verbose
                    println(("｜"^level) * iofield_string)
                end
                # Get the target IDS object
                ff = getraw(current_ids, field)

                # Push nested structure onto the stack
                push!(stack, (value, ff, vcat(current_path, [field_string]), level + 1))

            elseif target_type <: IDSvector
                if verbose
                    println(("｜"^level) * iofield_string)
                end

                # Array of IDS structures
                ff = getraw(current_ids, field)

                if length(value) > length(ff)
                    eltype_ff = eltype(ff)
                    len_ori_ff = length(ff)

                    if len_ori_ff == 0
                        ff._value = Vector{eltype_ff}(undef, length(value))
                    else
                        resize!(ff._value, length(value))
                    end

                    for i in (len_ori_ff+1):length(value)
                        ff._value[i] = eltype_ff()
                        setfield!(ff._value[i], :_parent, WeakRef(ff))
                    end
                    add_filled(ff)
                end

                # Push each element of the array onto the stack
                for i in 1:length(value)
                    if verbose
                        println(("｜"^(level + 1)) * string(i))
                    end
                    push!(stack, (value[i], ff[i], vcat(current_path, [field_string, "[$i]"]), level + 1))
                end
            else
                # Leaf node
                if typeof(value) <: Union{Nothing,Missing}
                    continue
                end
                if verbose
                    print(("｜"^level) * iofield_string * " → ")
                end
                try
                    # Convert array data if necessary
                    if target_type <: AbstractArray
                        if tp_ndims(target_type) > 1
                            value = row_col_major_switch(reduce(hcat, value))
                        end
                        if (tp_eltype(target_type) <: Real) && !(tp_eltype(target_type) <: Integer)
                            value = convert(Array{Float64,tp_ndims(target_type)}, value)
                        else
                            value = convert(Array{tp_eltype(target_type),tp_ndims(target_type)}, value)
                        end
                    end
                    # Handle special case for dictionaries saved as strings
                    if typeof(value) <: Dict && target_type <: String
                        value = JSON.sprint(value)
                    end
                    # Set the property on the IDS structure
                    setproperty!(current_ids, field, value; skip_non_coordinates, error_on_missing_coordinates)
                catch e
                    @warn("$(location(current_ids, field)) was skipped in dict2imas_stack: $(e)")
                end
                if verbose
                    println(typeof(value))
                end
            end
        end
    end

    return ids
end

Base.:(==)(a::T1, b::T2) where {T1<:Union{IDS,IDSvector,Vector{IDS}},T2<:Union{IDS,IDSvector,Vector{IDS}}} = isequal(a, b)

function Base.isequal(a::T1, b::T2; verbose::Bool=false) where {T1<:Union{IDS,IDSvector,Vector{IDS}},T2<:Union{IDS,IDSvector,Vector{IDS}}}
    # Check if both objects are of the same type
    if typeof(a) != typeof(b)
        if verbose
            println("Type mismatch: $(typeof(a)) vs $(typeof(b))")
        end
        return false
    end

    # Initialize a stack for iterative comparison with paths for verbose output
    stack = Vector{Tuple{Any,Any,String}}()

    if a isa IDSvector
        a_parent_ids = (a._parent).value
        b_parent_ids = (b._parent).value
        push!(stack, (a_parent_ids, b_parent_ids, location(a_parent_ids)))
    else
        push!(stack, (a, b, location(a)))
    end

    all_equal = true  # Track if all fields are equal

    # Iterate until stack is empty
    while !isempty(stack)
        (obj_a, obj_b, path) = pop!(stack)

        # Check if both objects are of the same type at this level
        if typeof(obj_a) != typeof(obj_b)
            if verbose
                println("Type mismatch at $path: $(typeof(obj_a)) vs $(typeof(obj_b))")
            end
            all_equal = false
            continue
        end

        # Get target_fields in obj_a, excluding private_fields
        target_fields = filter(x -> x ∉ IMASdd.private_fields, fieldnames(typeof(obj_a)))

        # Iterate over target_fields in the object
        for field in target_fields
            field_a = getfield(obj_a, field)
            field_b = getfield(obj_b, field)

            # Skip fields that are of type `WeakRef`
            if field_a isa WeakRef || field_b isa WeakRef
                continue
            end

            # Construct the path to the current field for verbose output
            field_path = path * "." * String(field)

            # Compare IDSvector and nested fields by pushing them onto the stack
            if field_a isa IDSvector || field_a isa Vector{IDS}
                if length(field_a) != length(field_b)
                    if verbose
                        println("Array length mismatch at $field_path: $(length(field_a)) vs $(length(field_b))")
                    end
                    all_equal = false
                    continue
                end
                for i in 1:length(field_a)
                    push!(stack, (field_a[i], field_b[i], field_path * "[$i]"))
                end
            elseif field_a isa IDS
                # Add to stack for nested structures
                push!(stack, (field_a, field_b, field_path))
            else
                # Direct comparison for primitive types
                if !isequal(field_a, field_b)
                    if verbose
                        highlight_differences(field_path, field_a, field_b)
                    end
                    all_equal = false
                end
            end
        end
    end

    if verbose
        print("\n")
    end
    return all_equal  # Return true if all fields matched, false otherwise
end


function highlight_differences(path::String, a::Any, b::Any; color_index::Symbol=:red, color_a::Symbol=:blue, color_b::Symbol=:green)
    print("\n")

    printstyled("$path"; bold=true)

    preceding_chars = " "^2 * "↳" * " "
    print("\n")
    print(preceding_chars)
    print("Type: ")
    printstyled("$(typeof(a))"; color=color_a)
    print(" vs ")
    printstyled("$(typeof(b))"; color=color_b)
    print("\n")

    if isa(a, AbstractArray) && isa(b, AbstractArray)
        # Handle vector differences with length condition
        if length(a) != length(b)
            print(preceding_chars)
            print("Length: ")
            printstyled("($(length(a)))"; color=color_a)
            print(" vs ")
            printstyled("($(length(b)))"; color=color_b)
            print("\n")
            return
        end

        num_mismatches = count(i -> a[i] != b[i], 1:length(a))
        if num_mismatches == length(a)
            print(preceding_chars)
            printstyled("All elements ($num_mismatches)"; color=color_index, bold=true)
            print(" are different\n")
        else
            print(preceding_chars)
            print("Mismatch in ")
            printstyled("$num_mismatches"; color=color_index, bold=true)
            print(" out of ")
            printstyled("$(length(a))"; color=color_index, bold=true)
            print(" elements: \n")
        end

        max_num_mismatches = 20

        if length(a) < max_num_mismatches
            for i in 1:length(a)
                print(" "^length(preceding_chars))
                if a[i] == b[i]
                    print("[$i-th: $(a[i])]")
                else
                    printstyled("[$i-th: "; color=:light_black)
                    printstyled("$(a[i])"; color=color_a, bold=true)
                    printstyled(" vs "; color=:light_black)
                    printstyled("$(b[i])"; color=color_b, bold=true)
                    printstyled("]"; color=:light_black)
                end
                i < length(a) ? print("\n") : nothing
            end
            print("\n")
        else
            if num_mismatches > max_num_mismatches
                print(" "^length(preceding_chars))
                printstyled("Too long to display all differences\n"; color=:light_black)
                print(" "^length(preceding_chars))
                print("The followings are the ")
                printstyled("first $max_num_mismatches differences\n"; color=color_index)
            end
            kk = 0 # count numer of printed mismatch elements
            for i in 1:length(a)
                if a[i] != b[i]
                    print(" "^length(preceding_chars))
                    printstyled("[$i-th: "; color=:light_black)
                    printstyled("$(a[i])"; color=color_a, bold=true)
                    printstyled(" vs "; color=:light_black)
                    printstyled("$(b[i])"; color=color_b, bold=true)
                    printstyled("]"; color=:light_black)
                    kk += 1
                    # kk < num_mismatches ? print("\n") : nothing
                    kk < max_num_mismatches ? print("\n") : break
                end
            end
            print("\n")
        end
    elseif isa(a, Number) && isa(b, Number)
        print(preceding_chars)
        print("Value: ")
        printstyled("$a"; color=color_a)
        print(" vs ")
        printstyled("$b"; color=color_b)
    elseif isa(a, String) && isa(b, String)
        print(preceding_chars)
        print("Value: ")
        printstyled("\"$a\""; color=color_a)
        print(" vs ")
        printstyled("\"$b\""; color=color_b)
    else
        print(preceding_chars)
        printstyled("Unsupported type for difference highlighting\n"; color=:light_black)
    end
    print("\n")
    return
end


"""
    row_col_major_switch(X::AbstractArray)

Reverse the order of array dimensions

This is used for example to switch between row-major (JULIA, FORTRAN, MATLAB) to column-major (C, PYTHON, IMAS) array represnetations
"""
function row_col_major_switch(A::AbstractArray)
    return permutedims(A, reverse(1:ndims(A)))
end

"""
    row_col_major_switch_lazy(X::AbstractArray)

Lazily reverse the order of array dimensions. This operation returns a wrapper structure operating around the original data, thus avoiding extra allocations.

This is used for example to switch between row-major (JULIA, FORTRAN, MATLAB) to column-major (C, PYTHON, IMAS) array represnetations
"""
function row_col_major_switch_lazy(A::AbstractArray)
    return PermutedDimsArray(A, reverse(1:ndims(A)))
end

tp_ndims(::Type{<:AbstractArray{T,N}}) where {T,N} = N
tp_ndims(v::UnionAll) = ndims(v.body)
tp_eltype(::Type{<:AbstractArray{T,N}}) where {T,N} = T
tp_eltype(v::UnionAll) = v.var.ub

"""
    imas2dict(ids::Union{IDS,IDSvector}; freeze::Bool=true, strict::Bool=false)

Populate Julia structure of dictionaries and vectors with data from IMAS data structure `ids`
"""
function imas2dict(@nospecialize(ids::IDS); freeze::Bool=true, strict::Bool=false)
    dct = Dict{Symbol,Any}()
    return imas2dict(ids, dct; freeze, strict)
end

function imas2dict(@nospecialize(ids::IDS), dct::Dict{Symbol,Any}; freeze::Bool, strict::Bool)
    fields = collect(keys_no_missing(ids))
    if typeof(ids) <: DD
        push!(fields, :global_time)
    end
    for field in fields
        value = get_frozen_strict_property(ids, field; freeze, strict)
        iofield = field_translator_jl2io(field)
        if typeof(value) <: Union{Missing,Function}
            continue
        elseif typeof(value) <: Union{IDS,IDSvector} # structures
            if typeof(value) <: IDS
                dct[iofield] = Dict{Symbol,Any}()
            else
                dct[iofield] = Dict{Symbol,Any}[]
            end
            imas2dict(value, dct[iofield]; freeze, strict)
            if isempty(dct[iofield])
                delete!(dct, iofield)
            end
        else # leaf
            if typeof(value) <: AbstractArray
                value = row_col_major_switch(value)
            end
            dct[iofield] = value
        end
    end
    return dct
end

function imas2dict(@nospecialize(ids::IDSvector); freeze::Bool=true, strict::Bool=false)
    dct = Dict{Symbol,Any}[]
    return imas2dict(ids, dct; freeze, strict)
end

function imas2dict(@nospecialize(ids::IDSvector), dct::Vector{Dict{Symbol,Any}}; freeze::Bool, strict::Bool)
    for field in 1:length(ids)
        push!(dct, Dict{Symbol,Any}())
        imas2dict(ids[field], dct[field]; freeze, strict)
    end
    return dct
end

export imas2dict
push!(document[:IO], :imas2dict)

#= ======================= =#
#  json2imas and imas2json  #
#= ======================= =#
"""
    json2imas(filename::AbstractString, @nospecialize(ids::IDS)=dd(); error_on_missing_coordinates::Bool=true, verbose::Bool=false)

Load the IMAS data structure from a JSON file with given `filename`

# Arguments

  - `error_on_missing_coordinates`: boolean to raise an error if coordinate is missing
  - `verbose`: print structure hierarchy as it is filled
"""
function json2imas(filename::AbstractString, @nospecialize(ids::IDS)=dd(); error_on_missing_coordinates::Bool=true, verbose::Bool=false)
    open(filename, "r") do io
        return jstr2imas(read(io, String), ids; error_on_missing_coordinates, verbose)
    end
    return ids
end

export json2imas
push!(document[:IO], :json2imas)

"""
    jstr2imas(json_string::String, @nospecialize(ids::IDS)=dd(); error_on_missing_coordinates::Bool=true, verbose::Bool=false)

Load the IMAS data structure from a JSON string

# Arguments

  - `error_on_missing_coordinates`: boolean to raise an error if coordinate is missing
  - `verbose`: print structure hierarchy as it is filled
"""
function jstr2imas(json_string::String, @nospecialize(ids::IDS)=dd(); error_on_missing_coordinates::Bool=true, verbose::Bool=false)
    json_data = JSON.parse(json_string)
    dict2imas(json_data, ids; error_on_missing_coordinates, verbose)
    if typeof(ids) <: DD
        last_global_time(ids)
    end
    return ids
end

export jstr2imas
push!(document[:IO], :jstr2imas)

"""
    imas2json(@nospecialize(ids::Union{IDS,IDSvector}), filename::AbstractString; freeze::Bool=true, strict::Bool=false, indent::Int=0, kw...)

Save the IMAS data structure to a JSON file with given `filename`.

# Arguments

  - `freeze` evaluates expressions
  - `strict` dumps fields that are strictly in ITER IMAS only
  - `kw...` arguments are passed to the `JSON.print` function
"""
function imas2json(@nospecialize(ids::Union{IDS,IDSvector}), filename::AbstractString; freeze::Bool=true, strict::Bool=false, indent::Int=0, kw...)
    json_string = string(ids; freeze, strict, indent, kw...)
    open(filename, "w") do io
        return write(io, json_string)
    end
    return json_string
end

export imas2json
push!(document[:IO], :imas2json)

"""
    Base.string(@nospecialize(ids::Union{IDS,IDSvector}); freeze::Bool=true, strict::Bool=false, indent::Int=0, kw...)

Returns JSON serialization of an IDS
"""
function Base.string(@nospecialize(ids::Union{IDS,IDSvector}); freeze::Bool=true, strict::Bool=false, indent::Int=0, kw...)
    json_data = imas2dict(ids; freeze, strict)
    return JSON.json(json_data, indent; kw...)
end

"""
    show_json(io::JSON.StructuralContext, s::JSON.CommonSerialization, x::AbstractFloat)

Type piracy for JSON method that writes floats to file to distinguish between NaN, and +/- Infinity

This is not needed for parsing, since JSON already can parse these with the `allownan` agrument, which is set true by default.
"""
function JSON.show_json(io::JSON.StructuralContext, s::JSON.CommonSerialization, x::AbstractFloat)
    if isfinite(x)
        Base.print(io, x)
    elseif isnan(x)
        Base.print(io, "NaN")
    elseif isinf(x)
        if x < 0
            Base.print(io, "-Infinity")
        else
            Base.print(io, "Infinity")
        end
    else
        JSON.show_null(io)
    end
end

"""
    JSON.sprint(data, args...; kw...)

Return JSON represntation of data
"""
function JSON.sprint(data, args...; kw...)
    buf = IOBuffer()
    JSON.print(buf, data, args...; kw...)
    return String(take!(buf))
end

#= ======== =#
#  hdf2imas  #
#= ======== =#
"""
    hdf2imas(filename::AbstractString; error_on_missing_coordinates::Bool=true, kw...)

Load data from a HDF5 file generated by OMAS Python platform (ie. hierarchical HDF5)

# Arguments

  - `kw...` arguments are passed to the `HDF5.h5open` function
"""
function hdf2imas(filename::AbstractString, @nospecialize(ids::IDS)=dd(); error_on_missing_coordinates::Bool=true, kw...)
    HDF5.h5open(filename, "r"; kw...) do fid
        if error_on_missing_coordinates
            hdf2imas(fid, ids; skip_non_coordinates=true, error_on_missing_coordinates)
            hdf2imas(fid, ids; skip_non_coordinates=false, error_on_missing_coordinates)
        else
            hdf2imas(fid, ids; skip_non_coordinates=false, error_on_missing_coordinates)
        end
    end
    if typeof(ids) <: DD
        last_global_time(ids)
    end
    return ids
end

function hdf2imas(gparent::Union{HDF5.File,HDF5.Group}, @nospecialize(ids::IDS); skip_non_coordinates::Bool, error_on_missing_coordinates::Bool)
    for iofield in keys(gparent)
        field = field_translator_io2jl(iofield)
        if !hasfield(typeof(ids), Symbol(field))
            if !skip_non_coordinates
                @debug("$(location(ids, iofield)) was skipped in hdf2imas")
            end
            continue
        end
        if typeof(gparent[iofield]) <: HDF5.Dataset
            value = read(gparent, iofield)
            if typeof(value) <: AbstractArray
                value = row_col_major_switch(value)
            end
            setproperty!(ids, Symbol(field), value; skip_non_coordinates, error_on_missing_coordinates)
        elseif iofield == "parameters"
            tmp = OrderedCollections.OrderedDict{String,Any}()
            hdf2dict!(gparent[iofield], tmp)
            setproperty!(ids, Symbol(field), JSON.sprint(tmp, 1))
        else
            hdf2imas(gparent[iofield], getproperty(ids, Symbol(field)); skip_non_coordinates, error_on_missing_coordinates)
        end
    end
    return ids
end

function hdf2imas(gparent::Union{HDF5.File,HDF5.Group}, @nospecialize(ids::IDSvector); skip_non_coordinates::Bool, error_on_missing_coordinates::Bool)
    indexes = sort!(collect(map(x -> parse(Int64, x), keys(gparent))))
    if isempty(ids)
        resize!(ids, length(indexes))
    end
    for (k, index) in enumerate(indexes)
        hdf2imas(gparent[string(index)], ids[k]; skip_non_coordinates, error_on_missing_coordinates)
    end
    return ids
end

export hdf2imas
push!(document[:IO], :hdf2imas)

#= ========= =#
#  hdf2dict!  #
#= ========= =#
"""
    hdf2dict!(gparent::Union{HDF5.File,HDF5.Group}, ids::AbstractDict)

Load data from a HDF5 file into a dictionary
"""
function hdf2dict!(gparent::Union{HDF5.File,HDF5.Group}, ids::AbstractDict)
    for iofield in keys(gparent)
        field = field_translator_io2jl(iofield)
        if typeof(gparent[iofield]) <: HDF5.Dataset
            value = read(gparent, iofield)
            if typeof(value) <: AbstractArray
                value = row_col_major_switch(value)
            end
            ids[field] = value
        else
            ids[field] = OrderedCollections.OrderedDict{String,Any}()
            hdf2dict!(gparent[iofield], ids[field])
        end
    end
    return ids
end

export hdf2dict!
push!(document[:IO], :hdf2dict!)

#= ======== =#
#  imas2hdf  #
#= ======== =#
"""
    imas2hdf(@nospecialize(ids::Union{IDS,IDSvector}), filename::AbstractString; freeze::Bool=true, strict::Bool=false, kw...)

Save the IMAS data structure to a OMAS HDF5 file with given `filename` (ie. hierarchical HDF5)

# Arguments

  - `kw...` arguments are passed to the `HDF5.h5open` function
  - `freeze` evaluates expressions
  - `strict` dumps fields that are strictly in ITER IMAS only
"""
function imas2hdf(@nospecialize(ids::Union{IDS,IDSvector}), filename::AbstractString; freeze::Bool=true, strict::Bool=false, kw...)
    HDF5.h5open(filename, "w"; kw...) do fid
        return imas2hdf(ids, fid; freeze, strict)
    end
end

function imas2hdf(@nospecialize(ids::IDS), gparent::Union{HDF5.File,HDF5.Group}; freeze::Bool=true, strict::Bool=false)
    fields = collect(keys_no_missing(ids))
    if typeof(ids) <: DD
        push!(fields, :global_time)
    end
    for field in fields
        iofield = field_translator_jl2io(field)
        value = get_frozen_strict_property(ids, field; freeze, strict)
        if typeof(value) <: Union{Missing,Function}
            continue
        elseif typeof(value) <: Union{IDS,IDSvector}
            g = HDF5.create_group(gparent, string(iofield))
            imas2hdf(value, g; freeze, strict)
        elseif typeof(value) <: AbstractString
            HDF5.write(gparent, string(iofield), value)
        else
            if typeof(value) <: AbstractArray
                value = row_col_major_switch(value)
            end
            dset = HDF5.create_dataset(gparent, string(iofield), eltype(value), size(value))
            HDF5.write(dset, value)
        end
    end
end

function imas2hdf(@nospecialize(ids::IDSvector), gparent::Union{HDF5.File,HDF5.Group}; freeze::Bool=true, strict::Bool=false)
    for (index, value) in enumerate(ids)
        if typeof(value) <: Union{IDS,IDSvector}
            g = HDF5.create_group(gparent, string(index - 1)) # -1 to conform to omas HDF5 format
            imas2hdf(value, g; freeze, strict)
        end
    end
end

export imas2hdf
push!(document[:IO], :imas2hdf)

#= ======== =#
#  h5i2imas  #
#= ======== =#
"""
    h5i2imas(filename::AbstractString, @nospecialize(ids::IDS)=dd(); kw...)

Load data from a HDF5 file generated by IMAS platform (ie. tensorized HDF5)

# Arguments

  - `kw...` arguments are passed to the `HDF5.h5open` function
"""
function h5i2imas(filename::AbstractString, @nospecialize(ids::IDS)=dd(); kw...)
    filename = abspath(filename)
    HDF5.h5open(filename, "r"; kw...) do fid
        for iofield in keys(fid)
            field = field_translator_io2jl(iofield)
            if Symbol(field) in fieldnames(typeof(ids))
                h5i2imas(fid[iofield], getproperty(ids, Symbol(field)); skip_non_coordinates=false)
            end
        end
    end
    if typeof(ids) <: DD
        last_global_time(ids)
    end
    return ids
end

function h5i2imas(gparent::Union{HDF5.File,HDF5.Group}, @nospecialize(ids::IDS); skip_non_coordinates::Bool)
    for iofield in keys(gparent)
        if endswith(iofield, "_SHAPE")
            continue
        end

        field = field_translator_io2jl(iofield)

        # get the value and convert int32 to int
        value = try
            read(gparent, iofield)
        catch e
            if !(typeof(e) <: ArgumentError)
                @warn "$iofield: $e"
            end
            continue
        end
        if typeof(value) <: Integer
            value = Int(value)
        elseif typeof(value) <: Array && eltype(value) <: Integer
            value = convert.(Int, value)
        end

        # figure out the shape
        field_shape_name = "$(iofield)_SHAPE"
        struct_shape_tmp = rsplit(iofield, "[]&"; limit=2)
        if field_shape_name in keys(gparent)
            shape = row_col_major_switch(convert.(Int, read(gparent, field_shape_name)))
        elseif length(struct_shape_tmp) > 1
            struct_shape_name = "$(struct_shape_tmp[1])[]&AOS_SHAPE"
            shape = row_col_major_switch(convert.(Int, read(gparent, struct_shape_name)))
        else
            shape = -1
        end

        path = map(Symbol, split(replace(iofield, "[]" => ""), "&"))
        try
            path_tensorized_setfield!(ids, path, value, shape, Int[]; skip_non_coordinates)
        catch e
            if typeof(e) <: ErrorException
                @warn "$iofield: $e"
            else
                rethrow(e)
            end
        end
    end
    return ids
end

export h5i2imas
push!(document[:IO], :h5i2imas)

function path_tensorized_setfield!(@nospecialize(ids::IDS), path::Vector{Symbol}, value::Any, shape::Union{Int,Array{Int}}, known_indices::Array{Int}; skip_non_coordinates::Bool)
    if length(path) > 1
        path_tensorized_setfield!(getfield(ids, path[1]), path[2:end], value, shape, known_indices; skip_non_coordinates)
    elseif shape == -1 || typeof(value) <: String
        #set scalar
        setproperty!(ids, path[1], value; skip_non_coordinates, error_on_missing_coordinates=false)
    else
        if any(shape .== 0)
            return
        end
        indices = (known_indices..., (1:k for k in shape)...)
        val = row_col_major_switch_lazy(value)[indices...]
        if typeof(val) <: String || ndims(val) <= 1
            setproperty!(ids, path[1], val; skip_non_coordinates, error_on_missing_coordinates=false)
        else
            setproperty!(ids, path[1], collect(val'); skip_non_coordinates, error_on_missing_coordinates=false)
        end
    end
end

function path_tensorized_setfield!(
    @nospecialize(ids::IDSvector),
    path::Vector{Symbol},
    value::Any,
    shape::Union{Int,Array{Int}},
    known_indices::Array{Int};
    skip_non_coordinates::Bool)

    if size(shape) == (1,)
        n = shape[1]
    else
        n = size(shape, 1)
    end

    if n == 0
        return
    elseif isempty(ids)
        resize!(ids, n)
    end

    push!(known_indices, 1)
    for k in 1:n
        if k > length(ids)
            break
        end

        known_indices[end] = k
        pass_shape_indices = (k, ntuple(i -> Colon(), ndims(shape) - 1)...)

        if ndims(shape) == 1
            # if everything below is zero sized, then skip it
            if known_indices[end] == 0
                continue
            end
            path_tensorized_setfield!(ids[k], path, value, Int[], known_indices; skip_non_coordinates)
        else
            # if everything below is zero sized, then skip it
            if sum(shape[pass_shape_indices...]) == 0
                continue
            end
            path_tensorized_setfield!(ids[k], path, value, shape[pass_shape_indices...], known_indices; skip_non_coordinates)
        end
    end
    return pop!(known_indices)
end

#= ======== =#
#  imas2h5i  #
#= ======== =#
"""
    imas2h5i(
        @nospecialize(ids::Union{IDS,IDSvector}),
        filename::AbstractString;
        freeze::Bool=true,
        strict::Bool=false,
        run::Int=0,
        shot::Int=0,
        hdf5_backend_version::String="1.0",
        kw...
    )

Save data to a HDF5 file generated by IMAS platform (ie. tensorized HDF5)

# Arguments

  - `kw...` arguments are passed to the `HDF5.h5open` function
  - `freeze` evaluates expressions
  - `strict` dumps fields that are strictly in ITER IMAS only
  - `run`, `shot`, `hdf5_backend_version` arguments are used to set the HDF5 attributes
"""
function imas2h5i(
    @nospecialize(ids::Union{IDS,IDSvector}),
    filename::AbstractString;
    freeze::Bool=true,
    strict::Bool=false,
    run::Int=0,
    shot::Int=0,
    hdf5_backend_version::String="1.0",
    kw...
)
    filename = abspath(filename)
    ret = OrderedCollections.OrderedDict{String,Any}()
    HDF5.h5open(filename, "w"; kw...) do fid
        HDF5.attributes(fid)["SHOT"] = shot
        HDF5.attributes(fid)["RUN"] = run
        HDF5.attributes(fid)["HDF5_BACKEND_VERSION"] = hdf5_backend_version
        return tensorize!(ret, ids, fid; freeze, strict)
    end
    return ret
end

export imas2h5i
push!(document[:IO], :imas2h5i)

# tensorize! entry point for DD
function tensorize!(ret::AbstractDict{String,Any}, @nospecialize(ids::DD), fid::HDF5.File; freeze::Bool, strict::Bool)
    for field in keys(ids)
        iofield = field_translator_jl2io(field)
        subids = get_frozen_strict_property(ids, field; freeze, strict)
        if subids !== missing && !isempty(subids)
            g = HDF5.create_group(fid, string(iofield))
            empty!(ret)
            ret = tensorize!(ret, subids; freeze, strict)
            # always set `homogeneous_time` when saving to h5i
            if "$iofield&ids_properties&homogeneous_time" ∉ keys(ret)
                ret["$iofield&ids_properties&homogeneous_time"] = Dict{Symbol,Any}(:data => 0)
            end
            write_tensor_data(ret, g)
        end
    end
    return ret
end

# tensorize! entry point for DD
function tensorize!(ret::AbstractDict{String,Any}, @nospecialize(ids::IDStop), fid::HDF5.File; freeze::Bool, strict::Bool)
    iofield = location(ids)
    g = HDF5.create_group(fid, string(iofield))
    empty!(ret)
    ret = tensorize!(ret, ids; freeze, strict)
    # always set `homogeneous_time` when saving to h5i
    if "$iofield&ids_properties&homogeneous_time" ∉ keys(ret)
        ret["$iofield&ids_properties&homogeneous_time"] = Dict{Symbol,Any}(:data => 0)
    end
    write_tensor_data(ret, g)
    return ret
end

function tensorize!(ret::AbstractDict{String,Any}, @nospecialize(ids::IDS); freeze::Bool, strict::Bool)
    path = ulocation(ids)

    # figure out the tensor sizes
    # this is where `aos_shape` and `arr_shape` are set
    shape_ids_vectors!(ret, ids, path, Int[]; freeze, strict)

    # initialize the `cshape` and `data` fields for every entry in ret
    for uloc in keys(ret)

        if endswith(uloc, "]")
            # this is for arrays of structures
            tmp = copy(ret[uloc][:aos_shape])
            tmp[end] = 1
            ret[uloc][:cshape] = zeros(Int, tmp...)

        elseif haskey(ret[uloc], :aos_shape) && haskey(ret[uloc], :arr_shape)
            # this is for leaves
            ret[uloc][:cshape] = zeros(Int, ret[uloc][:aos_shape]..., length(ret[uloc][:arr_shape]))
            tp = ret[uloc][:type]
            if tp <: String
                ret[uloc][:data] = Array{String}(undef, ret[uloc][:aos_shape]..., ret[uloc][:arr_shape]...) .= ""
            else
                ret[uloc][:data] = zeros(tp, ret[uloc][:aos_shape]..., ret[uloc][:arr_shape]...)
            end
        end
    end

    # assign the data
    assign_ids_vectors!(ret, ids, path, Int[]; freeze, strict)

    return ret
end

function assign_ids_vectors!(ret::AbstractDict{String,Any}, @nospecialize(ids::IDSvector), ppath::AbstractString, sz::Vector{Int}; freeze::Bool, strict::Bool)
    path = "$ppath[]"

    # sz holds the size of the array of structures in the tensorized representation
    sz = push!(sz, length(ids))

    if length(sz) == ndims(ret[path][:cshape])
        if any((size(ret[path][:cshape])[1:end-1] .- sz[1:end-1]) .< 0)
            return ret
        end

        ret[path][:cshape][sz[1:end-1]..., 1] = length(ids)
    end

    for k in eachindex(ids)
        sz[end] = k
        assign_ids_vectors!(ret, ids[k], path, sz; freeze, strict)
    end

    pop!(sz)
    return ret
end

function assign_ids_vectors!(ret::AbstractDict{String,Any}, @nospecialize(ids::IDS), ppath::AbstractString, sz::Vector{Int}; freeze::Bool, strict::Bool)
    if any(sz .== 0)
        return ret
    end

    for field in keys_no_missing(ids)
        iofield = field_translator_jl2io(field)
        path = "$ppath&$iofield"
        value = getfield(ids, field)

        if typeof(value) <: Union{IDS,IDSvector}
            assign_ids_vectors!(ret, value, path, copy(sz); freeze, strict)

        elseif !isempty(sz)

            if any((ret[path][:aos_shape] .- sz) .< 0)
                continue
            end

            if typeof(value) <: AbstractArray
                value = get_frozen_strict_property(ids, field; freeze, strict)
                if value === missing || typeof(value) <: Function || sum(size(value)) == 0
                    continue
                end
                if ndims(value) > 1
                    value = collect(value')
                end

                ret[path][:cshape][sz..., :] .= size(value)

                indices = sz
                for dim in 1:ndims(value)
                    indices = (indices..., 1:size(value)[dim])
                end
                ret[path][:data][indices...] .= value

            else
                value = get_frozen_strict_property(ids, field; freeze, strict)
                if value === missing || typeof(value) <: Function
                    continue
                end

                indices = (sz..., 1:1)
                ret[path][:cshape][indices...] .= 1
                ret[path][:data][indices...] .= value
            end

        else
            value = get_frozen_strict_property(ids, field; freeze, strict)
            if value === missing || typeof(value) <: Function
                continue
            end
            ret[path] = Dict{Symbol,Any}()
            ret[path][:data] = value
        end
    end
    return ret
end

function shape_ids_vectors!(ret::AbstractDict{String,Any}, @nospecialize(ids::IDSvector), ppath::AbstractString, sz::Vector{Int}; freeze::Bool, strict::Bool)
    path = "$ppath[]"
    if path ∉ keys(ret)
        ret[path] = Dict{Symbol,Any}()
    end
    sz = push!(sz, length(ids))
    ret[path][:aos_shape] = copy(sz)

    for k in eachindex(ids)
        sz[end] = k
        shape_ids_vectors!(ret, ids[k], path, sz; freeze, strict)
    end

    pop!(sz)
    return ret
end

function shape_ids_vectors!(ret::AbstractDict{String,Any}, @nospecialize(ids::IDS), ppath::AbstractString, sz::Vector{Int}; freeze, strict)
    for field in keys_no_missing(ids)
        iofield = field_translator_jl2io(field)
        path = "$ppath&$iofield"
        value = getfield(ids, field)
        if typeof(value) <: Union{IDS,IDSvector}
            shape_ids_vectors!(ret, value, path, copy(sz); freeze, strict)
        elseif !isempty(sz)

            if path ∉ keys(ret)
                ret[path] = Dict{Symbol,Any}()
                ret[path][:aos_shape] = copy(sz)
            else
                ret[path][:aos_shape] = max.(sz, ret[path][:aos_shape])
            end

            if typeof(value) <: AbstractArray
                value = get_frozen_strict_property(ids, field; freeze, strict)
                if value === missing || typeof(value) <: Function || sum(size(value)) == 0
                    continue
                end
                ret[path][:is0D] = false
                ret[path][:type] = eltype(value)
                if !haskey(ret[path], :arr_shape)
                    ret[path][:arr_shape] = size(value)
                else
                    ret[path][:arr_shape] = max.(size(value), ret[path][:arr_shape])
                end

            else
                value = get_frozen_strict_property(ids, field; freeze, strict)
                if value === missing || typeof(value) <: Function
                    continue
                end
                ret[path][:is0D] = true
                ret[path][:type] = typeof(value)
                ret[path][:arr_shape] = 1
            end
        end
    end
    return ret
end

function write_tensor_data(ret::AbstractDict{String,Any}, g::HDF5.Group)
    for uloc in keys(ret)
        path = split(uloc, "&"; limit=2)[end]
        if haskey(ret[uloc], :cshape) && !haskey(ret[uloc], :data) && sum(ret[uloc][:cshape]) > 0
            g["$(path)&AOS_SHAPE"] = Int32.(row_col_major_switch(ret[uloc][:cshape]))
        elseif haskey(ret[uloc], :data)
            data = ret[uloc][:data]
            if length(data) != 0
                if haskey(ret[uloc], :is0D) && ret[uloc][:is0D]
                    data = row_col_major_switch(data[(Colon() for _ in 1:ndims(data)-1)..., 1])
                    if typeof(data) <: Int
                        data = Int32(data)
                    end
                    g[path] = data
                elseif typeof(data) <: AbstractArray
                    data = row_col_major_switch(data)
                    if eltype(data) <: Int
                        data = Int32.(data)
                    end
                    g[path] = data
                    if haskey(ret[uloc], :cshape)
                        g["$(path)_SHAPE"] = Int32.(row_col_major_switch(ret[uloc][:cshape]))
                    end
                else
                    g[path] = data
                    if haskey(ret[uloc], :cshape)
                        g["$(path)_SHAPE"] = Int32.(row_col_major_switch(ret[uloc][:cshape]))
                    end
                end
            end
        end
    end

    return ret
end
