import JSON
import HDF5
document[:IO] = Symbol[]

#= =============== =#
#  IDS conversions  #
#= =============== =#
function _inner_converter(S::DataType, T::DataType, fieldname::Symbol, value::Any)
    if typeof(value) <: T
        return convert(S, value)
    else
        return value
    end
end

function _inner_converter(S::DataType, T::DataType, fieldname::Symbol, value::Vector)
    if eltype(value) <: T
        in_type = typeof(value)
        concrete_in_type = Base.typename(in_type).wrapper
        return Base.convert(concrete_in_type{S}, value)
    else
        return value
    end
end

function _inner_converter(S::DataType, T::DataType, fieldname::Symbol, value::IDSvector)
    in_type = eltype(value)
    concrete_in_type = Base.typename(in_type).wrapper
    return ids_convert(IDSvector{concrete_in_type{S}}, value)
end

function _inner_converter(S::DataType, T::DataType, fieldname::Symbol, @nospecialize(ids::IDS))
    in_type = typeof(ids)
    concrete_in_type = Base.typename(in_type).wrapper
    converted_value = ids_convert(concrete_in_type{S}, ids)
    return converted_value
end

function ids_convert(out_type::Type{Z}, @nospecialize(ids::IDSvector)) where {Z<:IDSvector}
    in_type = eltype(ids)
    out_type = out_type.parameters[1]
    concrete_in_type = Base.typename(in_type).wrapper
    concrete_out_type = Base.typename(out_type).wrapper
    @assert concrete_in_type === concrete_out_type "Cannot convert to a different IDS concrete type: $concrete_in_type to $concrete_out_type"

    T = in_type.parameters[1]
    S = out_type.parameters[1]

    ids_out = IDSvector{concrete_out_type{S}}()
    converted_values = (_inner_converter(S, T, Symbol(k), ids[k]) for k in eachindex(ids))
    append!(getfield(ids_out, :_value), converted_values)
    for field in eachindex(ids_out)
        setfield!(ids_out._value[field], :_parent, WeakRef(ids_out))
    end
    return ids_out
end

function ids_convert(out_type::Type{Z}, @nospecialize(ids::IDS)) where {Z<:IDS{<:Real}}
    in_type = typeof(ids)
    concrete_in_type = Base.typename(in_type).wrapper
    concrete_out_type = Base.typename(out_type).wrapper
    @assert concrete_in_type === concrete_out_type "Cannot convert to a different IDS concrete type"

    T = in_type.parameters[1]
    S = out_type.parameters[1]

    converted_values = (_inner_converter(S, T, fieldname, getfield(ids, fieldname)) for fieldname in fieldnames(concrete_in_type))

    ids_out = concrete_out_type{S}(converted_values...)
    for field in keys(ids_out)
        value = getfield(ids_out, field)
        if typeof(value) <: Union{IDS,IDSvector}
            setfield!(value, :_parent, WeakRef(ids_out))
        end
    end
    return ids_out
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

function dict2imas(dct::AbstractDict, @nospecialize(ids::T), path::Vector{String}; skip_non_coordinates::Bool, error_on_missing_coordinates::Bool, verbose::Bool) where {T<:IDS}
    # recursively traverse `dct` structure
    level = length(path)
    for (_field_, value) in dct

        # handle both Dict{Symbol,Any} or Dict{String,Any}
        field_string = string(_field_)
        field = Symbol(field_string)

        if !hasfield(typeof(ids), field)
            if !skip_non_coordinates
                @warn("$(location(ids, field)) was skipped in dict2imas")
            end
            continue
        end

        target_type = typeof(getfield(ids, field))

        if target_type <: IDS
            # Structure
            if verbose
                println(("｜"^level) * field_string)
            end
            ff = getraw(ids, field)
            dict2imas(value, ff, vcat(path, [field_string]); skip_non_coordinates, error_on_missing_coordinates, verbose)

        elseif target_type <: IDSvector
            # Array of structures
            ff = getraw(ids, field)
            if verbose
                println(("｜"^level) * field_string)
            end
            if length(ff) < length(value)
                resize!(ff, length(value))
            end
            for i in 1:length(value)
                if verbose
                    println(("｜"^(level + 1)) * string(i))
                end
                dict2imas(value[i], ff[i], vcat(path, [field_string, "[$i]"]); skip_non_coordinates, error_on_missing_coordinates, verbose)
            end

        else
            # Leaf
            if typeof(value) <: Union{Nothing,Missing}
                continue
            end
            if verbose
                print(("｜"^level) * field_string * " → ")
            end
            try
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
                # this is to handle OMAS saving of code.parameters as nested dictionaries and not as strings
                if typeof(value) <: Dict && target_type <: String
                    value = JSON.sprint(value)
                end
                setproperty!(ids, field, value; skip_non_coordinates, error_on_missing_coordinates)
            catch e
                @warn("$(location(ids, field)) was skipped in dict2imas: $(e)")
            end
            if verbose
                println(typeof(value))
            end
        end
    end

    return ids
end

export dict2imas
push!(document[:IO], :dict2imas)

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
        if typeof(value) <: Union{Missing,Function}
            continue
        elseif typeof(value) <: Union{IDS,IDSvector} # structures
            if typeof(value) <: IDS
                dct[field] = Dict{Symbol,Any}()
            else
                dct[field] = Dict{Symbol,Any}[]
            end
            imas2dict(value, dct[field]; freeze, strict)
            if isempty(dct[field])
                delete!(dct, field)
            end
        else # field
            if typeof(value) <: AbstractArray
                value = row_col_major_switch(value)
            end
            dct[field] = value
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
    for field in keys(gparent)
        if !hasfield(typeof(ids), Symbol(field))
            if !skip_non_coordinates
                @debug("$(location(ids, field)) was skipped in hdf2imas")
            end
            continue
        end
        if typeof(gparent[field]) <: HDF5.Dataset
            value = read(gparent, field)
            if typeof(value) <: AbstractArray
                value = row_col_major_switch(value)
            end
            setproperty!(ids, Symbol(field), value; skip_non_coordinates, error_on_missing_coordinates)
        elseif field == "parameters"
            tmp = OrderedCollections.OrderedDict{String,Any}()
            hdf2dict!(gparent[field], tmp)
            setproperty!(ids, Symbol(field), JSON.sprint(tmp, 1))
        else
            hdf2imas(gparent[field], getproperty(ids, Symbol(field)); skip_non_coordinates, error_on_missing_coordinates)
        end
    end
    return ids
end

function hdf2imas(gparent::Union{HDF5.File,HDF5.Group}, @nospecialize(ids::IDSvector); skip_non_coordinates::Bool, error_on_missing_coordinates::Bool)
    indexes = sort!(collect(map(x -> parse(Int64, x), keys(gparent))))
    if isempty(ids)
        resize!(ids, length(indexes))
    end
    for (field, index) in enumerate(indexes)
        hdf2imas(gparent[string(index)], ids[field]; skip_non_coordinates, error_on_missing_coordinates)
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
    for field in keys(gparent)
        if typeof(gparent[field]) <: HDF5.Dataset
            value = read(gparent, field)
            if typeof(value) <: AbstractArray
                value = row_col_major_switch(value)
            end
            ids[field] = value
        else
            ids[field] = OrderedCollections.OrderedDict{String,Any}()
            hdf2dict!(gparent[field], ids[field])
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
        value = get_frozen_strict_property(ids, field; freeze, strict)
        if typeof(value) <: Union{Missing,Function}
            continue
        elseif typeof(value) <: Union{IDS,IDSvector}
            g = HDF5.create_group(gparent, string(field))
            imas2hdf(value, g; freeze, strict)
        elseif typeof(value) <: AbstractString
            HDF5.write(gparent, string(field), value)
        else
            if typeof(value) <: AbstractArray
                value = row_col_major_switch(value)
            end
            dset = HDF5.create_dataset(gparent, string(field), eltype(value), size(value))
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
        for field in keys(fid)
            if Symbol(field) in fieldnames(typeof(ids))
                h5i2imas(fid[field], getproperty(ids, Symbol(field)); skip_non_coordinates=false)
            end
        end
    end
    return ids
end

function h5i2imas(gparent::Union{HDF5.File,HDF5.Group}, @nospecialize(ids::IDS); skip_non_coordinates::Bool)
    for field in keys(gparent)
        if endswith(field, "_SHAPE")
            continue
        end

        # get the value and convert int32 to int
        value = try
            read(gparent, field)
        catch e
            if !(typeof(e) <: ArgumentError)
                @warn "$field: $e"
            end
            continue
        end
        if typeof(value) <: Integer
            value = Int(value)
        elseif typeof(value) <: Array && eltype(value) <: Integer
            value = convert.(Int, value)
        end

        # figure out the shape
        field_shape_name = "$(field)_SHAPE"
        struct_shape_tmp = rsplit(field, "[]&"; limit=2)
        if field_shape_name in keys(gparent)
            shape = row_col_major_switch(convert.(Int, read(gparent, field_shape_name)))
        elseif length(struct_shape_tmp) > 1
            struct_shape_name = "$(struct_shape_tmp[1])[]&AOS_SHAPE"
            shape = row_col_major_switch(convert.(Int, read(gparent, struct_shape_name)))
        else
            shape = -1
        end

        path = map(Symbol, split(replace(field, "[]" => ""), "&"))
        try
            path_tensorized_setfield!(ids, path, value, shape, Int[]; skip_non_coordinates)
        catch e
            @warn "$field: $e"
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
        setproperty!(ids, path[1], val; skip_non_coordinates, error_on_missing_coordinates=false)
    end
end

function path_tensorized_setfield!(
    @nospecialize(ids::IDSvector),
    path::Vector{Symbol},
    value::Any,
    shape::Union{Int,Array{Int}},
    known_indices::Array{Int};
    skip_non_coordinates::Bool
)
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

        if size(shape) == (1,)
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
        subids = get_frozen_strict_property(ids, field; freeze, strict)
        if subids !== missing && !isempty(subids)
            g = HDF5.create_group(fid, string(field))
            empty!(ret)
            ret = tensorize!(ret, subids; freeze, strict)
            # always set `homogeneous_time` when saving to h5i
            if "$field&ids_properties&homogeneous_time" ∉ keys(ret)
                ret["$field&ids_properties&homogeneous_time"] = Dict{Symbol,Any}(:data => 0)
            end
            write_tensor_data(ret, g)
        end
    end
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

function assign_ids_vectors!(ret::AbstractDict{String,Any}, @nospecialize(ids::IDSvector), ppath::String, sz::Vector{Int}; freeze::Bool, strict::Bool)
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

function assign_ids_vectors!(ret::AbstractDict{String,Any}, @nospecialize(ids::IDS), ppath::String, sz::Vector{Int}; freeze::Bool, strict::Bool)
    if any(sz .== 0)
        return ret
    end

    for field in keys_no_missing(ids)
        path = "$ppath&$field"
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

function shape_ids_vectors!(ret::AbstractDict{String,Any}, @nospecialize(ids::IDSvector), ppath::String, sz::Vector{Int}; freeze::Bool, strict::Bool)
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

function shape_ids_vectors!(ret::AbstractDict{String,Any}, @nospecialize(ids::IDS), ppath::String, sz::Vector{Int}; freeze, strict)
    for field in keys_no_missing(ids)
        path = "$ppath&$field"
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
