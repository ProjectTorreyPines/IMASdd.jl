import JSON
import HDF5
import Dates
document[:IO] = Symbol[]

#= ================== =#
#  format independent  #
#= ================== =#
"""
    file2imas(filename::AbstractString; kw...)

Load IDS from a file that can be in different formats .json or .h5 both ITER tensorized (h5i) or OMAS hierarchical (hdf)
"""
function file2imas(filename::AbstractString; kw...)
    if endswith(filename, ".json")
        return json2imas(filename; kw...)
    elseif endswith(filename, ".nc")
        error("OMAS `nc` format is not yet supported. Use `.json` or `.h5` formats instead.")
    elseif endswith(filename, ".h5")
        if is_h5i(filename)
            return h5i2imas(filename; kw...)
        else
            return hdf2imas(filename; kw...)
        end
    end
end

export file2imas
push!(document[:IO], :file2imas)

"""
    is_h5i(filename::AbstractString)

Returns true if a file is in ITER tensorized (h5i) format
"""
function is_h5i(filename::AbstractString)
    if endswith(filename, ".h5")
        HDF5.h5open(filename, "r") do file
            for name in keys(file)
                if endswith(name, "&AOS_SHAPE")
                    return true
                end
            end
        end
    end
    return false
end
export is_h5i
push!(document[:IO], :is_h5i)

#= ======== =#
#  IO utils  #
#= ======== =#

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
    else
        return field
    end
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

    ids_out = concrete_out_type{S}(; frozen=getfield(ids, :_frozen))
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
    out = Base.typename(typeof(idsv)).wrapper{eltype(tmp)}(; frozen=getfield(idsv, :_frozen))
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
    dict2imas(dct::AbstractDict, @nospecialize(ids::IDS); show_warnings::Bool=true)

Populate IMAS data structure `ids` based on data contained in Julia dictionary `dct`.
"""
function dict2imas(dct::AbstractDict, @nospecialize(ids::T); error_on_missing_coordinates::Bool=true, show_warnings::Bool=true) where {T<:IDS}
    if error_on_missing_coordinates
        dict2imas(dct, ids, String[]; show_warnings, skip_non_coordinates=true, error_on_missing_coordinates)
        dict2imas(dct, ids, String[]; show_warnings, skip_non_coordinates=false, error_on_missing_coordinates)
    else
        dict2imas(dct, ids, String[]; show_warnings, skip_non_coordinates=false, error_on_missing_coordinates)
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
    show_warnings::Bool) where {T<:IDS}

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
                if show_warnings && !skip_non_coordinates
                    @warn("`$(location(current_ids, field))` was skipped because it not exists in `dd`")
                end
                continue
            end

            # Retrieve the target type of the field
            target_type = concrete_fieldtype_typeof(current_ids, field)

            if target_type <: IDS
                # Get the target IDS object
                ff = getraw(current_ids, field)

                # Push nested structure onto the stack
                push!(stack, (value, ff, vcat(current_path, [field_string]), level + 1))

            elseif target_type <: IDSvector
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
                        ff._value[i] = eltype_ff(; frozen=getfield(ids, :_frozen))
                        setfield!(ff._value[i], :_parent, WeakRef(ff))
                    end
                    add_filled(ff)
                end

                # Push each element of the array onto the stack
                for i in 1:length(value)
                    push!(stack, (value[i], ff[i], vcat(current_path, [field_string, "[$i]"]), level + 1))
                end
            else
                # Leaf node
                if typeof(value) <: Union{Nothing,Missing}
                    continue
                end
                # Convert array data if necessary
                if target_type <: AbstractArray
                    if tp_ndims(target_type) > 1
                        value = row_col_major_switch(reduce(hcat, value))
                    end
                    if eltype(target_type) <: Complex
                        value = eltype(target_type).(getindex.(value, "re"), getindex.(value, "im"))
                    elseif !(value isa target_type)
                        value = convert(target_type, value)
                    end
                elseif target_type <: Complex
                    value = target_type(value["re"], value["im"])
                end
                # Handle special case for dictionaries saved as strings
                if typeof(value) <: Dict && target_type <: String
                    value = JSON.sprint(value)
                end
                # Set the property on the IDS structure
                setproperty!(current_ids, field, value; skip_non_coordinates, error_on_missing_coordinates)
            end
        end
    end

    return ids
end

Base.:(==)(a::T1, b::T2) where {T1<:Union{IDS,IDSvector,Vector{IDS}},T2<:Union{IDS,IDSvector,Vector{IDS}}} = isequal(a, b)

function Base.isequal(a::T1, b::T2; verbose::Bool=false) where {T1<:Union{IDS,IDSvector,Vector{IDS}},T2<:Union{IDS,IDSvector,Vector{IDS}}}

    comparable_fields = _extract_comparable_fields(a, b)

    all_equal = true  # Track if all fields are equal
    while !isempty(comparable_fields)
        item = pop!(comparable_fields)

        if item.already_different
            verbose && highlight_differences(item.path, item.a, item.b)
            all_equal = false
        elseif !isequal(item.a, item.b)
            verbose && highlight_differences(item.path, item.a, item.b)
            all_equal = false
        end
    end

    return all_equal
end

function Base.isapprox(a::T1, b::T2; verbose::Bool=false, kw...) where {T1<:Union{IDS,IDSvector,Vector{IDS}},T2<:Union{IDS,IDSvector,Vector{IDS}}}

    comparable_fields = _extract_comparable_fields(a, b)

    all_approx = true  # Track if all fields are equal
    while !isempty(comparable_fields)
        item = pop!(comparable_fields)

        if item.already_different
            verbose && highlight_differences(item.path, item.a, item.b)
            all_approx = false
        else
            # Direct comparison for primitive types
            if eltype(item.a) <: Number && eltype(item.b) <: Number
                if any(isnan.(item.a)) || any(isnan.(item.b))
                    comparison_result = isequal(item.a, item.b)
                else
                    comparison_result = isapprox(item.a, item.b; kw...)
                end
            else
                comparison_result = isequal(item.a, item.b)
            end

            if !comparison_result
                verbose && highlight_differences(item.path, item.a, item.b)
                all_approx = false
            end
        end
    end

    return all_approx  # Return true if all fields matched, false otherwise
end

function _extract_comparable_fields(a::T1, b::T2) where {T1<:Union{IDS,IDSvector,Vector{IDS}},T2<:Union{IDS,IDSvector,Vector{IDS}}}
    comparable_fields = Vector{NamedTuple{(:a, :b, :path, :already_different),Tuple{Any,Any,String,Bool}}}()

    if typeof(a) != typeof(b)
        push!(comparable_fields, (a=a, b=b, path="Arguments", already_different=true))
        return comparable_fields
    end

    stack = Vector{Tuple{Any,Any,String}}()

    if a isa IDSvector
        if length(a) != length(b)
            push!(comparable_fields, (a=a, b=b, path=ulocation(a), already_different=true))
            return comparable_fields
        end
        for i in eachindex(a)
            push!(stack, (a[i], b[i], location(a[i])))
        end
    else
        push!(stack, (a, b, location(a)))
    end

    while !isempty(stack)
        (obj_a, obj_b, path) = pop!(stack)

        if typeof(obj_a) != typeof(obj_b)
            push!(comparable_fields, (a=obj_a, b=obj_b, path=path, already_different=true))
            continue
        end

        if isempty(obj_a) && isempty(obj_b)
            continue
        end

        target_fields = filter(x -> x ∉ IMASdd.private_fields, fieldnames(typeof(obj_a)))

        for field in target_fields
            field_path = path * "." * String(field)

            if ismissing(obj_a, field) && ismissing(obj_b, field)
                continue
            elseif ismissing(obj_a, field) || ismissing(obj_b, field)
                field_a = ismissing(obj_a, field) ? missing : getproperty(obj_a, field)
                field_b = ismissing(obj_b, field) ? missing : getproperty(obj_b, field)
                push!(comparable_fields, (a=field_a, b=field_b, path=field_path, already_different=true))
                continue
            end

            field_a = getproperty(obj_a, field)
            field_b = getproperty(obj_b, field)

            if field_a isa WeakRef || field_b isa WeakRef
                continue
            end

            if field_a isa IDSvector || field_a isa Vector{IDS}
                if length(field_a) != length(field_b)
                    continue
                end
                for i in 1:length(field_a)
                    push!(stack, (field_a[i], field_b[i], field_path * "[$i]"))
                end
            elseif field_a isa IDS
                push!(stack, (field_a, field_b, field_path))
            else
                push!(comparable_fields, (a=field_a, b=field_b, path=field_path, already_different=false))
            end
        end
    end

    return comparable_fields
end


function highlight_differences(path::String, a::Any, b::Any; color_index::Symbol=:red, color_a::Symbol=:blue, color_b::Symbol=:green)
    # print("\n")

    printstyled("$path"; bold=true)

    preceding_chars = " "^2 * "↳" * " "
    print("\n")
    print(preceding_chars)
    print("Type: ")
    printstyled("$(typeof(a))"; color=color_a)
    print(" vs ")
    printstyled("$(typeof(b))"; color=color_b)
    print("\n")

    if typeof(a) != typeof(b)
        print(preceding_chars)
        printstyled("Type mismatch\n"; color=:light_red)
    elseif isa(a, AbstractArray) && isa(b, AbstractArray)
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
        print("\n")
    elseif isa(a, String) && isa(b, String)
        print(preceding_chars)
        print("Value: ")
        printstyled("\"$a\""; color=color_a)
        print(" vs ")
        printstyled("\"$b\""; color=color_b)
        print("\n")
    elseif isa(a, Missing) || isa(b, Missing)
        print(preceding_chars)
        printstyled("One of fields is Missing\n"; color=:light_black)
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
    imas2dict(ids::Union{IDS,IDSvector}; freeze::Bool=false, strict::Bool=false)

Populate Julia structure of dictionaries and vectors with data from IMAS data structure `ids`
"""
function imas2dict(@nospecialize(ids::IDS); freeze::Bool=false, strict::Bool=false)
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

function imas2dict(@nospecialize(ids::IDSvector); freeze::Bool=false, strict::Bool=false)
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
    json2imas(filename::AbstractString, @nospecialize(ids::IDS)=dd(); error_on_missing_coordinates::Bool=true, show_warnings::Bool=true)

Load the IMAS data structure from a JSON file with given `filename`
"""
function json2imas(filename::AbstractString, @nospecialize(ids::IDS)=dd(); error_on_missing_coordinates::Bool=true, show_warnings::Bool=true)
    open(filename, "r") do io
        return jstr2imas(read(io, String), ids; show_warnings, error_on_missing_coordinates)
    end
    return ids
end

export json2imas
push!(document[:IO], :json2imas)

"""
    jstr2imas(json_string::String, @nospecialize(ids::IDS)=dd(); error_on_missing_coordinates::Bool=true, show_warnings::Bool=true)

Load the IMAS data structure from a JSON string
"""
function jstr2imas(json_string::String, @nospecialize(ids::IDS)=dd(); error_on_missing_coordinates::Bool=true, show_warnings::Bool=true)
    json_data = JSON.parse(json_string)
    dict2imas(json_data, ids; show_warnings, error_on_missing_coordinates)
    if typeof(ids) <: DD
        last_global_time(ids)
    end
    return ids
end

export jstr2imas
push!(document[:IO], :jstr2imas)

"""
    imas2json(@nospecialize(ids::Union{IDS,IDSvector}), filename::AbstractString; freeze::Bool=false, strict::Bool=false, indent::Int=0, kw...)

Save the IMAS data structure to a JSON file with given `filename`.

# Arguments

  - `freeze` evaluates expressions
  - `strict` dumps fields that are strictly in ITER IMAS only
  - `kw...` arguments are passed to the `JSON.print` function
"""
function imas2json(@nospecialize(ids::Union{IDS,IDSvector}), filename::AbstractString; freeze::Bool=false, strict::Bool=false, indent::Int=0, kw...)
    json_string = string(ids; freeze, strict, indent, kw...)
    open(filename, "w") do io
        return write(io, json_string)
    end
    return json_string
end

export imas2json
push!(document[:IO], :imas2json)

"""
    Base.string(@nospecialize(ids::Union{IDS,IDSvector}); freeze::Bool=false, strict::Bool=false, indent::Int=0, kw...)

Returns JSON serialization of an IDS
"""
function Base.string(@nospecialize(ids::Union{IDS,IDSvector}); freeze::Bool=false, strict::Bool=false, indent::Int=0, kw...)
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
    JSON.show_json(io::JSON.StructuralContext, s::JSON.CommonSerialization, ids::IDS)

Type piracy for JSON method that handles IDSs

NOTE: does not freeze expressions
"""
function JSON.show_json(io::JSON.StructuralContext, s::JSON.CommonSerialization, ids::IDS)
    json_data = imas2dict(ids; freeze=false, strict=false)
    return JSON.show_json(io, s, json_data)
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
    hdf2imas(filename::AbstractString, target_path::AbstractString; error_on_missing_coordinates::Bool=true, verbose::Bool=false, kw...)

Load an object from an HDF5 file using a simple entry point. Given a file and an internal
target path (as a string), the function returns either the dataset’s value or an IMAS ids
structure constructed from a group.

If the object at `target_path` is a group and has a "concrete_type" attribute, that type is
evaluated and instantiated; otherwise, a default (`dd()`) is used. Coordinate data is
processed based on `error_on_missing_coordinates`.

# Arguments

  - `filename`: Path to the HDF5 file
  - `target_path`: Internal HDF5 path to the desired dataset or group
  - `error_on_missing_coordinates` (default: `true`): Enforce coordinate checks
  - `verbose` (default: `false`): Enable verbose logging
  - `kw...`: Additional keyword arguments passed to `HDF5.h5open`

# Returns

The value of the dataset or the constructed IMAS ids.
"""
function hdf2imas(filename::AbstractString, target_path::AbstractString; show_warnings::Bool=true, error_on_missing_coordinates::Bool=true, verbose::Bool=false, kw...)
    HDF5.h5open(filename, "r"; kw...) do fid
        @assert haskey(fid, target_path) "hdf2imas: File `$filename` does not have `$target_path`."
        obj = fid[target_path]

        if isa(obj, HDF5.Dataset)
            return obj[]
        elseif isa(obj, HDF5.Group)
            attr = HDF5.attributes(obj)
            if "concrete_type" in keys(attr)
                conc_type = attr["concrete_type"][]
                verbose && @info "Found type of `$(target_path)` => $(attr["abstract_type"][]) [$(conc_type)]"
                ids = eval(Meta.parse(conc_type))()
            else
                verbose && @warn "Assumed type of `$(target_path)` => $(typeof(dd()))"
                ids = dd()
            end

            if error_on_missing_coordinates
                hdf2imas(obj, ids; show_warnings, skip_non_coordinates=true, error_on_missing_coordinates)
                hdf2imas(obj, ids; show_warnings, skip_non_coordinates=false, error_on_missing_coordinates)
            else
                hdf2imas(obj, ids; show_warnings, skip_non_coordinates=false, error_on_missing_coordinates)
            end

            if typeof(ids) <: DD
                last_global_time(ids)
            end

            return ids
        end
    end
end

"""
    hdf2imas(filename::AbstractString; error_on_missing_coordinates::Bool=true, kw...)

Load data from a HDF5 file generated by OMAS Python platform (ie. hierarchical HDF5)

`kw...` arguments are passed to the `HDF5.h5open` function
"""
function hdf2imas(filename::AbstractString, @nospecialize(ids::IDS)=dd(); show_warnings::Bool=true, error_on_missing_coordinates::Bool=true, kw...)
    HDF5.h5open(filename, "r"; kw...) do fid
        if error_on_missing_coordinates
            hdf2imas(fid, ids; show_warnings, error_on_missing_coordinates, skip_non_coordinates=true)
            hdf2imas(fid, ids; show_warnings, error_on_missing_coordinates, skip_non_coordinates=false)
        else
            hdf2imas(fid, ids; show_warnings, error_on_missing_coordinates, skip_non_coordinates=false)
        end
    end
    if typeof(ids) <: DD
        last_global_time(ids)
    end
    return ids
end

function hdf2imas(gparent::Union{HDF5.File,HDF5.Group}, @nospecialize(ids::IDS); show_warnings::Bool, skip_non_coordinates::Bool, error_on_missing_coordinates::Bool)
    for iofield in keys(gparent)
        field = field_translator_io2jl(iofield)
        if !hasfield(typeof(ids), Symbol(field))
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
            hdf2imas(gparent[iofield], getproperty(ids, Symbol(field)); show_warnings, skip_non_coordinates, error_on_missing_coordinates)
        end
    end
    return ids
end

function hdf2imas(gparent::Union{HDF5.File,HDF5.Group}, @nospecialize(ids::IDSvector); show_warnings::Bool, skip_non_coordinates::Bool, error_on_missing_coordinates::Bool)
    indexes = sort!(collect(map(x -> parse(Int64, x), keys(gparent))))
    if isempty(ids)
        resize!(ids, length(indexes))
    end
    for (k, index) in enumerate(indexes)
        hdf2imas(gparent[string(index)], ids[k]; show_warnings, skip_non_coordinates, error_on_missing_coordinates)
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
    variable_length_string_type()

Create an HDF5 datatype for variable-length UTF-8 strings (H5T_VARIABLE).
This ensures compatibility with IMAS-AL which expects variable-length strings.
"""
function variable_length_string_type()
    dt = HDF5.API.h5t_copy(HDF5.API.H5T_C_S1)
    HDF5.API.h5t_set_size(dt, HDF5.API.H5T_VARIABLE)
    HDF5.API.h5t_set_cset(dt, HDF5.API.H5T_CSET_UTF8)
    return HDF5.Datatype(dt)
end

"""
    write_varlen_string(parent::Union{HDF5.File,HDF5.Group}, name::AbstractString, value::String)

Write a scalar variable-length UTF-8 string to an HDF5 file/group.
Creates the dataset and writes the string in one call for IMAS-AL compatibility.
"""
function write_varlen_string(parent::Union{HDF5.File,HDF5.Group}, name::AbstractString, value::String)
    str_type = variable_length_string_type()
    dset = HDF5.create_dataset(parent, name, str_type, ())
    cstr_ptr = Base.unsafe_convert(Ptr{UInt8}, value)
    ref_ptr = Ref(cstr_ptr)
    HDF5.API.h5d_write(dset, str_type, HDF5.API.H5S_ALL, HDF5.API.H5S_ALL, HDF5.API.H5P_DEFAULT, ref_ptr)
    return nothing
end

"""
    write_varlen_string_array(parent::Union{HDF5.File,HDF5.Group}, name::AbstractString, values::AbstractArray{String})

Write an array of variable-length UTF-8 strings to an HDF5 file/group.
Creates the dataset and writes the array in one call for IMAS-AL compatibility.
"""
function write_varlen_string_array(parent::Union{HDF5.File,HDF5.Group}, name::AbstractString, values::AbstractArray{String})
    str_type = variable_length_string_type()
    dset = HDF5.create_dataset(parent, name, str_type, size(values))
    cstr_ptrs = [Base.unsafe_convert(Ptr{UInt8}, v) for v in values]
    HDF5.API.h5d_write(dset, str_type, HDF5.API.H5S_ALL, HDF5.API.H5S_ALL, HDF5.API.H5P_DEFAULT, cstr_ptrs)
    return nothing
end

"""
    imas2hdf(@nospecialize(ids::Union{IDS,IDSvector}), filename::AbstractString;
             mode::String="w", target_group::String="/", overwrite::Bool=false,
             freeze::Bool=false, strict::Bool=false, desc::String="", kw...)

Save an IMAS data structure to an OMAS HDF5 file.

Arguments:

  - `filename`: HDF5 file path
  - `mode`: File open mode ("w", "a", or "r+"); "a" is converted to "r+"
  - `target_group`: Group where data will be stored (default is `"/"`)
  - `overwrite`: If true, overwrite the target group if it exists
  - `show_warnings`: If true, display warn messages
  - `freeze` evaluates expressions
  - `strict` dumps fields that are strictly in ITER IMAS only
  - `desc`: description of additional information (e.g., Shot number)
  - `compress`: compression level, an integer between 0 (no compression) and 9 (highest)
  - `kw...`: Options passed to the internal dispatch

Returns:

The result of `imas2hdf(ids, gparent; freeze, strict, desc)`.
"""
function imas2hdf(@nospecialize(ids::Union{IDS,IDSvector}), filename::AbstractString;
    mode::String="w", target_group::String="/", overwrite::Bool=false, show_warnings::Bool=true,
    freeze::Bool=false, strict::Bool=false, desc::String="", compress::Int=0, kw...)

    @assert mode in ("w", "a", "r+") "mode must be \"w\", \"a\", or \"r+\"."
    mode = (mode == "a") ? "r+" : mode

    # Normalize the target_group path
    target_group = norm_hdf5_path(target_group)

    HDF5.h5open(filename, mode) do fid
        if haskey(fid, target_group)
            if target_group == "/"
                gparent = fid
            else
                if !overwrite
                    error("Target group '$target_group' already exists in file '$filename'. " *
                          "\n       Set `overwrite`=true to replace the existing group.")
                else
                    show_warnings && @warn "Target group '$target_group' already exists. Overwriting it..."
                    HDF5.delete_object(fid, target_group)
                    gparent = HDF5.create_group(fid, target_group)
                end
            end
        else
            gparent = HDF5.create_group(fid, target_group)
        end

        return imas2hdf(ids, gparent; freeze, strict, desc, compress, kw...)
    end
end

function imas2hdf(@nospecialize(ids::IDS), gparent::Union{HDF5.File,HDF5.Group}; freeze::Bool=false, strict::Bool=false, desc::String="", compress::Int=0)

    @assert compress in 0:9 "compress must be between 0 and 9"

    # Add metadata to group's attributes
    attr = HDF5.attrs(gparent)
    attr["abstract_type"] = "IDS"
    attr["concrete_type"] = string(typeof(ids))
    attr["freeze"] = string(freeze)
    attr["strict"] = string(strict)
    attr["description"] = desc
    if typeof(gparent) <: HDF5.File
        update_file_attributes(gparent)
    end

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
            if haskey(gparent, string(iofield))
                g = gparent[string(iofield)]
            else
                g = HDF5.create_group(gparent, string(iofield))
            end
            imas2hdf(value, g; freeze, strict, compress)
        elseif typeof(value) <: AbstractString
            if haskey(gparent, string(iofield))
                HDF5.delete_object(gparent, string(iofield))
            end
            # Use variable-length UTF-8 strings (H5T_VARIABLE) for IMAS-AL compatibility
            write_varlen_string(gparent, string(iofield), value)
        else
            if typeof(value) <: AbstractArray
                value = row_col_major_switch(value)
            end
            if haskey(gparent, string(iofield))
                HDF5.delete_object(gparent, string(iofield))
            end
            if eltype(value) == String
                # Use variable-length UTF-8 strings (H5T_VARIABLE) for IMAS-AL compatibility
                write_varlen_string_array(gparent, string(iofield), value)
            else
                gparent[string(iofield), compress=compress] = value
            end
        end
    end
end

function imas2hdf(@nospecialize(ids::IDSvector), gparent::Union{HDF5.File,HDF5.Group}; freeze::Bool=false, strict::Bool=false, desc::String="", compress::Int=0)

    @assert compress in 0:9 "compress must be between 0 and 9"

    # Add metadata
    attr = HDF5.attrs(gparent)
    attr["abstract_type"] = "IDSvector"
    attr["concrete_type"] = string(typeof(ids))
    attr["freeze"] = string(freeze)
    attr["strict"] = string(strict)
    attr["description"] = desc

    for (index, value) in enumerate(ids)
        if typeof(value) <: Union{IDS,IDSvector}
            if haskey(gparent, string(index - 1))
                g = gparent[string(index - 1)] # -1 to conform to omas HDF5 format
            else
                g = HDF5.create_group(gparent, string(index - 1)) # -1 to conform to omas HDF5 format
            end
            imas2hdf(value, g; freeze, strict, compress)
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

`kw...` arguments are passed to the `HDF5.h5open` function
"""
function h5i2imas(filename::AbstractString, @nospecialize(ids::IDS)=dd(); show_warnings::Bool=true, kw...)
    filename = abspath(filename)
    HDF5.h5open(filename, "r"; kw...) do fid
        for iofield in keys(fid)
            field = field_translator_io2jl(iofield)
            if !hasfield(typeof(ids), Symbol(field))
                show_warnings && @warn("`$(location(ids, field))` was skipped because it not exists in `dd`")
                continue
            end
            h5i2imas(fid[iofield], getproperty(ids, Symbol(field)); show_warnings, skip_non_coordinates=false)
        end
    end
    if typeof(ids) <: DD
        last_global_time(ids)
    end
    return ids
end

function h5i2imas(gparent::Union{HDF5.File,HDF5.Group}, @nospecialize(ids::IDS); show_warnings::Bool, skip_non_coordinates::Bool)
    for iofield in keys(gparent)
        if endswith(iofield, "_SHAPE")
            continue
        end

        # get the value and convert int32 to int
        value = try
            read(gparent, iofield)
        catch e
            if !(typeof(e) <: ArgumentError)
                show_warnings && @warn "$iofield: $e"
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
            shape = -1 # scalar
        end

        path = map(Symbol, eachsplit(replace(iofield, "[]" => ""), "&"))
        try
            path_tensorized_setfield!(ids, path, value, shape, Int[]; show_warnings, skip_non_coordinates)
        catch e
            if typeof(e) <: ErrorException
                show_warnings && @warn "$iofield: $e"
            else
                rethrow(e)
            end
        end
    end
    return ids
end

export h5i2imas
push!(document[:IO], :h5i2imas)

function path_tensorized_setfield!(
    @nospecialize(ids::IDS),
    path::Vector{Symbol},
    value::Any,
    shape::Union{Int,Array{Int}},
    known_indices::Array{Int};
    show_warnings::Bool,
    skip_non_coordinates::Bool
)
    if length(path) > 1
        path_tensorized_setfield!(getfield(ids, path[1]), path[2:end], value, shape, known_indices; show_warnings, skip_non_coordinates)
    else
        field = field_translator_io2jl(path[1])
        if show_warnings && !hasfield(typeof(ids), field)
            @warn("`$(location(ids, field))` was skipped because it not exists in `dd`")
        elseif shape == -1 || typeof(value) <: String
            #set scalar
            setproperty!(ids, field, value; skip_non_coordinates, error_on_missing_coordinates=false)
        else
            if any(shape .== 0)
                return
            end
            indices = (known_indices..., (1:k for k in shape)...)
            # # OM: I have seen some weird indexing errors happening from GGD data generated by WEST
            # # For now I assume the issue is theirs.
            # # The loop below checks that the actual data array size matches the expected size from shape metadata.
            # # If the actual data is smaller than expected, we silently skip this field to avoid BoundsError.
            # size_value = size(value)
            # for (k,i) in enumerate(indices[end:-1:1])
            #     if typeof(i) <: Int
            #         if size_value[k] < i
            #             #println(location(ids, field))
            #             return
            #         end
            #     else
            #         if size_value[k] < i[end]
            #             #println(location(ids, field))
            #             return
            #         end
            #     end
            # end
            val = @views value[indices[end:-1:1]...]
            if typeof(val) <: String || ndims(val) <= 1
                setproperty!(ids, field, val; skip_non_coordinates, error_on_missing_coordinates=false)
            else
                # NOTE: indices[end:-1:1] already handles the row/column major switch
                setproperty!(ids, field, collect(val); skip_non_coordinates, error_on_missing_coordinates=false)
            end
        end
    end
end

function path_tensorized_setfield!(
    @nospecialize(ids::IDSvector),
    path::Vector{Symbol},
    value::Any,
    shape::Union{Int,Array{Int}},
    known_indices::Array{Int};
    show_warnings::Bool,
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
            path_tensorized_setfield!(ids[k], path, value, Int[], known_indices; show_warnings, skip_non_coordinates)
        else
            # if everything below is zero sized, then skip it
            if sum(shape[pass_shape_indices...]) == 0
                continue
            end
            path_tensorized_setfield!(ids[k], path, value, shape[pass_shape_indices...], known_indices; show_warnings, skip_non_coordinates)
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
        freeze::Bool=false,
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
    freeze::Bool=false,
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

        else
            # Get the frozen/strict value once
            value = get_frozen_strict_property(ids, field; freeze, strict)
            if value === missing || typeof(value) <: Function
                continue
            end

            if !isempty(sz)

                if any((ret[path][:aos_shape] .- sz) .< 0)
                    continue
                end

                if typeof(value) <: AbstractArray
                    if sum(size(value)) == 0
                        continue
                    end
                    if ndims(value) > 1
                        value = row_col_major_switch(value)
                    end

                    ret[path][:cshape][sz..., :] .= size(value)

                    indices = sz
                    for dim in 1:ndims(value)
                        indices = (indices..., 1:size(value)[dim])
                    end
                    ret[path][:data][indices...] .= value

                else
                    indices = (sz..., 1:1)
                    ret[path][:cshape][indices...] .= 1
                    ret[path][:data][indices...] .= value
                end

            else
                ret[path] = Dict{Symbol,Any}()
                ret[path][:data] = value
            end
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

            # Get the frozen/strict value once
            value = get_frozen_strict_property(ids, field; freeze, strict)
            if value === missing || typeof(value) <: Function
                continue
            end

            if typeof(value) <: AbstractArray
                if sum(size(value)) == 0
                    continue
                end
                ret[path][:is0D] = false
                ret[path][:type] = eltype(value)
                size_value = reverse(size(value))
                if !haskey(ret[path], :arr_shape)
                    ret[path][:arr_shape] = size_value
                else
                    ret[path][:arr_shape] = max.(size_value, ret[path][:arr_shape])
                end

            else
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
                    # Use variable-length UTF-8 strings (H5T_VARIABLE) for IMAS-AL compatibility
                    if typeof(data) <: String
                        write_varlen_string(g, path, data)
                    else
                        g[path] = data
                    end
                elseif typeof(data) <: AbstractArray
                    data = row_col_major_switch(data)
                    if eltype(data) <: Int
                        data = Int32.(data)
                    end
                    # Use variable-length UTF-8 strings (H5T_VARIABLE) for IMAS-AL compatibility
                    if eltype(data) <: String
                        write_varlen_string_array(g, path, data)
                    else
                        g[path] = data
                    end
                    if haskey(ret[uloc], :cshape)
                        g["$(path)_SHAPE"] = Int32.(row_col_major_switch(ret[uloc][:cshape]))
                    end
                else
                    # Use variable-length UTF-8 strings (H5T_VARIABLE) for IMAS-AL compatibility
                    if typeof(data) <: String
                        write_varlen_string(g, path, data)
                    else
                        g[path] = data
                    end
                    if haskey(ret[uloc], :cshape)
                        g["$(path)_SHAPE"] = Int32.(row_col_major_switch(ret[uloc][:cshape]))
                    end
                end
            end
        end
    end

    return ret
end

#= ======= =#
#  h5merge  #
#= ======= =#
"""
    h5merge(output_file::AbstractString, keys_files::Union{AbstractDict{<:AbstractString,<:AbstractString},AbstractVector{<:Pair{<:AbstractString,<:AbstractString}};
          mode::AbstractString="a", skip_existing_entries::Bool=false,
          h5_group_search_depth::Integer=0, h5_strip_group_prefix::Bool=false,
          verbose::Bool=false)

Merges multiple files into a single HDF5 output file.

# Arguments

  - `output_file`: Path to the HDF5 file where data will be merged.

  - `keys_files`: A vector or dictionary mapping target group names to input filenames.
  - `mode`: `"w"` to create a new file or `"a"` to append to an existing one.
  - `skip_existing_entries`: If `true`, groups already present in the output file are not overwritten.
  - `h5_group_search_depth`: For input HDF5 files, the depth at which to collect group paths.

      + `0` means use the root (`"/"`).
      + `1` means collect immediate children of the root.
      + Higher values collect groups deeper in the hierarchy.
  - `h5_strip_group_prefix`: If `true`, the target group name (the key from `keys_files`) is omitted from the output HDF5 path.
    For example, if an input file contains a group path `"/level1/level2"` and the key is `"parent"`, then:

      + With `h5_strip_group_prefix = false`, the output path becomes `"/parent/level1/level2"`.
      + With `h5_strip_group_prefix = true`, the output path becomes `"/level1/level2"`.
  - `verbose`: If `true`, additional logging information is printed.

# Behavior

  - For input files with the `.h5` extension, the function opens the file and collects group paths up to `h5_group_search_depth`.
    Each collected path is modified by stripping the first N components (using "/" as the delimiter) according to the flag `h5_strip_group_prefix` (if `true`, the parent key is omitted).
    Then, the corresponding objects are copied into the output file.
  - For other file types (e.g., JSON, YAML, text, markdown), the file is read and stored as text or raw binary data.
  - The function records attributes for each copied group that indicate the original file paths.

Returns a vector of group names (as strings) that were processed.
"""
function h5merge(
    output_file::AbstractString,
    keys_files::Union{AbstractDict{<:AbstractString,<:AbstractString},AbstractVector{<:Pair{<:AbstractString,<:AbstractString}}};
    mode::AbstractString="a",
    skip_existing_entries::Bool=false,
    h5_group_search_depth::Integer=0,
    h5_strip_group_prefix::Bool=false,
    verbose::Bool=false
)
    @assert mode in ("w", "a")
    @assert h5_group_search_depth >= 0 "h5_group_search_depth cannot be negative"

    if h5_strip_group_prefix
        # Ensure that at least level 1 is searched if the parent prefix is to be omitted.
        h5_group_search_depth = max(h5_group_search_depth, 1)
    end

    for file in values(keys_files)
        @assert !isdir(file) "h5merge: File `$file` is a directory, not a file."
        @assert isfile(file) "h5merge: File `$file` does not exist."
    end

    if !isfile(output_file)
        mode = "w"
    end
    if mode == "a"
        mode = "r+"
    end

    check_group_list = String[]

    HDF5.h5open(output_file, mode) do output_h5

        update_file_attributes(output_h5)

        for (gparent_name, input_file) in keys_files
            if haskey(output_h5, gparent_name)
                if skip_existing_entries
                    continue
                end
                HDF5.delete_object(output_h5, gparent_name)
            end

            if isempty(input_file) || filesize(input_file) == 0
                file_type = "EMPTY"
                HDF5.create_dataset(output_h5, gparent_name, UInt8, 0)

            elseif endswith(input_file, ".h5")
                file_type = "HDF5"

                HDF5.h5open(input_file, "r") do input_h5

                    target_h5_paths = h5_collect_group_paths(input_h5, h5_group_search_depth)
                    # Use gparent_name only if h5_strip_group_prefix is false.
                    prefix = h5_strip_group_prefix ? "" : gparent_name

                    for ori_h5_path in target_h5_paths
                        new_h5_path = norm_hdf5_path("/" * prefix * "/" * ori_h5_path)

                        if haskey(output_h5, new_h5_path)
                            if skip_existing_entries
                                verbose && @info "Skipping [$(relpath(input_h5.filename))$ori_h5_path]"
                                continue
                            else
                                @warn "Overwriting: [$(relpath(input_h5.filename))$ori_h5_path] --> [$(output_h5.filename)$new_h5_path]"
                                HDF5.delete_object(output_h5, new_h5_path)
                            end
                        end

                        HDF5.copy_object(input_h5, ori_h5_path, output_h5, new_h5_path)

                        attr = HDF5.attrs(output_h5[new_h5_path])
                        attr["original_file_abs_path"] = abspath(input_file)
                        attr["original_file_rel_path"] = relpath(input_file)
                        push!(check_group_list, new_h5_path)
                    end
                end
            elseif split(input_file, ".")[end] in ("json", "yaml", "txt", "md") || is_text_file(input_file)
                file_type = "TEXT"
                open(input_file, "r") do io
                    text = read(io, String)
                    return HDF5.write(output_h5, gparent_name, text)
                end
            else
                file_type = "BINARY"
                open(input_file, "r") do io
                    data = read(io) # Read the file as bytes
                    dset = HDF5.create_dataset(output_h5, gparent_name, UInt8, length(data))
                    return dset[:] = data
                end
            end
            if verbose
                @info "$(gparent_name) --> [$(file_type)] @ $(input_file)"
            end

            if file_type != "HDF5"
                attr = HDF5.attrs(output_h5[gparent_name])
                attr["original_file_abs_path"] = abspath(input_file)
                attr["original_file_rel_path"] = relpath(input_file)
                push!(check_group_list, gparent_name)
            end
        end
    end
    return check_group_list
end

"""
    h5merge(
        output_file::AbstractString,
        directories::AbstractVector{<:AbstractString};
        include_base_dir::Bool=true,
        cleanup::Bool=false,
        kwargs...)

Add all files in multiple directories (and their subdirectories) to an HDF5 `output_file`
"""
function h5merge(output_file::AbstractString, directories::AbstractVector{<:AbstractString}; mode::AbstractString="a", cleanup::Bool=false, kwargs...)
    @assert mode in ("w", "a")

    if mode == "w"
        h5merge(output_file, directories[1]; include_base_dir=true, mode="w", cleanup, kwargs...)
        for this_directory in directories[2:end]
            h5merge(output_file, this_directory; include_base_dir=true, mode="a", cleanup, kwargs...)
        end
    else
        for this_directory in directories
            h5merge(output_file, this_directory; include_base_dir=true, mode, cleanup, kwargs...)
        end
    end
end

"""
    h5merge(
        output_file::AbstractString,
        directory::AbstractString;
        mode::AbstractString="a",
        skip_existing_entries::Bool=false,
        follow_symlinks::Bool=false,
        verbose::Bool=false,
        include_base_dir::Bool=false,
        pattern::Union{Regex,Nothing}=nothing,
        kwargs...
        )

Add all files in a directory (and subdirectories) to an HDF5 `output_file`
"""
function h5merge(
    output_file::AbstractString,
    directory::AbstractString;
    mode::AbstractString="a",
    skip_existing_entries::Bool=false,
    follow_symlinks::Bool=false,
    verbose::Bool=false,
    include_base_dir::Bool=false,
    pattern::Union{Regex,Nothing}=nothing,
    cleanup::Bool=false,
    kwargs...
)
    @assert isdir(directory) "h5merge: `$directory` is not a valid directory."
    (verbose && isfile(output_file)) ? (@warn "h5merge: `$output_file` already exists.") : nothing

    directory = normpath(directory)
    directory = joinpath(splitpath(directory)...)
    base_dir = basename(directory)

    # Collect all files in the directory recursively
    keys_files = Dict{String,String}()
    for (root, dirs, files) in walkdir(directory; follow_symlinks)
        for file in files
            if !startswith(file, ".")
                if pattern === nothing || occursin(pattern, file)

                    root_components = splitpath(root)
                    base_idx = findlast(x -> x == base_dir, root_components)

                    if include_base_dir
                        group_name = join([root_components[base_idx:end]..., file], "/")
                    else
                        group_name = join([root_components[base_idx+1:end]..., file], "/")
                    end
                    keys_files[group_name] = abspath(root, file)
                end
            end
        end
    end

    check_group_list = h5merge(output_file, keys_files; mode, skip_existing_entries, verbose, kwargs...)

    if cleanup
        cleanup_files_and_directory(output_file, directory, keys_files; check_group_list, verbose, pattern)
    end

    return
end

"""
    h5_collect_group_paths(root::HDF5.Group, search_depth::Integer)

Recursively collects group paths in an HDF5 file up to the specified target depth using an iterative, stack-based approach.

  - If `search_depth == 0`, returns ["/"].
  - If `search_depth == 1`, returns paths of all objects directly under the root (e.g., "/group1", "/group2").
  - For `search_depth > 1`, returns paths at the specified depth (e.g., for `search_depth == 2`, returns "/group1/subgroup1", etc.).

Returns an array of strings containing the collected group paths.
"""
function h5_collect_group_paths(H5_file::HDF5.File, search_depth::Integer)
    paths = String[]
    # Initialize the stack with a tuple: (current group, current path, current depth)
    root_group = H5_file["/"]
    stack = [(root_group, "/", 0)]

    while !isempty(stack)
        grp, current_path, depth = pop!(stack)

        if depth == search_depth
            # If we've reached the target depth, add the current path to the results.
            push!(paths, current_path)
        else
            # Otherwise, iterate over the children of the current group.
            for child in keys(grp)
                new_path = current_path * "/" * child
                child_obj = grp[child]
                if child_obj isa HDF5.Group
                    # Push the child group onto the stack with incremented depth.
                    push!(stack, (child_obj, new_path, depth + 1))
                else
                    # If the child is not a group but would reach the target depth,
                    # add its path to the results.
                    if depth + 1 == search_depth
                        push!(paths, new_path)
                    end
                end
            end
        end
    end

    return paths
end

"""
    read_combined_h5(filename::AbstractString; show_warnings::Bool=true, error_on_missing_coordinates::Bool=true, pattern::Regex=r"", kw...)

Iteratively traverse an HDF5 file from the root ("/") using a stack.

# Arguments

  - `filename`: Path to the combined HDF5 file
  - `error_on_missing_coordinates` (default `true`): Enforce coordinate checks during dispatch
  - `pattern` (default `r""`): A regex used to filter which paths are processed
  - `kw...`: Additional keyword arguments passed to `hdf2imas`

# Returns

  - `Dict{String,Any}`: loaded data with keys (path as string)
"""
function read_combined_h5(filename::AbstractString; show_warnings::Bool=true, error_on_missing_coordinates::Bool=true, pattern::Regex=r"", kw...)
    results = Dict{String,Any}()

    HDF5.h5open(filename, "r") do fid
        stack = ["/"]  # start at the root
        while !isempty(stack)
            current_path = pop!(stack)
            obj = fid[current_path]

            if isa(obj, HDF5.Dataset)
                # Only store datasets matching the filter.
                if occursin(pattern, current_path)
                    results[current_path] = obj[]
                end

            elseif isa(obj, HDF5.Group)
                attrs = HDF5.attributes(obj)
                dispatch = false
                if haskey(attrs, "abstract_type")
                    abs_type = attrs["abstract_type"][]
                    if abs_type in ("IDS", "IDSvector")
                        dispatch = true
                    end
                end
                # Determine group name from current path (empty for root).
                group_name = current_path == "/" ? "" : basename(current_path)
                if !dispatch && !isempty(group_name) && endswith(group_name, ".h5")
                    dispatch = true
                end

                if dispatch
                    if occursin(pattern, current_path)
                        # Call the dispatch function and store the result.
                        results[current_path] = hdf2imas(filename, current_path; show_warnings, error_on_missing_coordinates, kw...)
                    end
                else
                    # Always traverse children even if the current group doesn't match.
                    for key in keys(obj)
                        new_path = (current_path == "/") ? ("/" * key) : (current_path * "/" * key)
                        push!(stack, new_path)
                    end
                end
            end
        end
    end
    return results
end

"""
    is_text_file(file::AbstractString)

Checks is all characters are printable or whitespace
"""
function is_text_file(file::AbstractString)
    open(file, "r") do io
        data = read(io, String) # Read the first num_bytes
        return all(c -> isprint(c) || c == '\n' || c == '\t' || c == '\r', data)
    end
end

function update_file_attributes(file::HDF5.File)
    attr = HDF5.attrs(file)
    attr["IMASdd_version"] = string(pkgversion(IMASdd))
    attr["date_time"] = Dates.format(Dates.now(), "yyyy-mm-ddTHH:MM:SS")
    return
end

function norm_hdf5_path(path::AbstractString)
    # Replace multiple slashes with a single slash
    normalized = replace(path, r"/+" => "/")
    # Optionally ensure the normalized path starts with a slash
    if !startswith(normalized, "/")
        normalized = "/" * normalized
    end
    return normalized
end

function cleanup_files_and_directory(
    combined_file_name::AbstractString,
    base_directory::AbstractString,
    keys_files::Union{AbstractDict{<:AbstractString,<:AbstractString},AbstractVector{<:Pair{<:AbstractString,<:AbstractString}}};
    check_group_list::AbstractArray{<:AbstractString}=String[],
    verbose::Bool=false,
    pattern::Union{Regex,Nothing}=nothing
)

    if isempty(check_group_list)
        for (group_name, input_file) in keys_files
            push!(check_group_list, group_name)
        end
    end

    HDF5.h5open(combined_file_name, "r") do merged_h5
        all_exist = true
        for group_name in check_group_list
            if !haskey(merged_h5, group_name)
                all_exist = false
                @warn "Group '$group_name' is missing in $combined_file_name; cleanup aborted."
                break
            end
        end

        if all_exist
            # Conditionally print the appropriate INFO message based on `pattern`
            if pattern === nothing || pattern == r""
                @info "All files in `$(base_directory)` are successfully merged into $(combined_file_name)"
            else
                @info "Files containing $(pattern) in `$(base_directory)` are successfully merged into $(combined_file_name)"
            end

            @info "Cleaning up merged files from disk..."
            for (group_name, input_file) in keys_files
                try
                    rm(input_file; force=true)
                    if verbose
                        @info "Removed file: $(input_file)"
                    end
                catch e
                    @warn "Failed to remove file $(input_file): $e"
                end
            end

            # Check recursively if base_directory has any files.
            if !directory_contains_files(base_directory)
                try
                    rm(base_directory; recursive=true)
                    @info "Base directory '$(base_directory)' and all its subdirectories were empty and have been removed."
                catch e
                    @warn "Failed to remove base directory $(base_directory): $e"
                end
            else
                @info "Base directory '$(base_directory)' contains files; not removing it."
            end
        else
            @warn "Not all input files were successfully merged; skipping cleanup."
        end
    end
end

function directory_contains_files(dir::AbstractString)
    for (root, dirs, files) in walkdir(dir)
        # If there's any file in this level, return true
        if !isempty(files)
            return true
        end
    end
    return false
end

export h5merge
push!(document[:IO], :h5merge)
export read_combined_h5
push!(document[:IO], :read_combined_h5)