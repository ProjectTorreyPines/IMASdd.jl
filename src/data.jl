using Printf
import AbstractTrees
import OrderedCollections
import StaticArraysCore
document[:Base] = Symbol[]

function rsplit2(str::AbstractString, splitter::AbstractChar)
    N = length(str)
    idx = findlast(splitter, str)
    @assert !isnothing(idx) "\"$splitter\" not found in \"$str\""
    return SubString(str, 1:(idx-1)), SubString(str, idx+1:N)
end

#= ============================ =#
#  IDS and IDSvector structures  #
#= ============================ =#
# this structure is used when returning generators to avoid specialization
# of the generator on the many concrete IDS types that are in IMASdd
struct NoSpecialize
    specialized_data_structure::Any
end

function Base.getproperty(ns::NoSpecialize, field::Symbol)
    return getfield(ns, :specialized_data_structure)
end

#= ==== =#
#  info  #
#= ==== =#
"""
    info(uloc::AbstractString, extras::Bool=true)

Return information of a node in the IMAS data structure, possibly including extra structures
"""
function info(ulocation::AbstractString, extras::Bool=true)
    tp, field = ulocation_2_tp_field(ulocation)
    nfo = _all_info[getfield(@__MODULE__, Symbol(tp)), Symbol(field)]
    if !extras && nfo.extras
        error("$ulocation is an extra structure")
    end
    return nfo
end

function ulocation_2_tp_field(ulocation::AbstractString)
    if ulocation == "dd"
        tp = "dd"
        field = "_"
    elseif !occursin('.', ulocation)
        tp = "dd"
        field = ulocation
    else
        tp, field = rsplit2(ulocation, '.')
    end
    field = replace(field, "[:]" => "")
    tp = replace(tp, r"\[:\]$" => "", "[:]" => "_", "." => "__")
    return tp, field
end

"""
    function info(@nospecialize(ids_type::Type), field::Symbol)

Return information of a filed of an IDS
"""
@inline function info(@nospecialize(ids_type::Type), field::Symbol)
    return _all_info[(ids_type.name.wrapper, field)]::Info
end

function info(@nospecialize(ids::UnionAll), field::Symbol)
    return _all_info[(ids, field)]::Info
end

function info(@nospecialize(ids::IDSvector), field::Symbol)
    return info(eltype(ids), field)
end

function info(@nospecialize(ids::IDS), field::Symbol)
    return info(typeof(ids), field)
end

export info
push!(document[:Base], :info)

"""
    units(uloc::String)
"""
function units(uloc::String)
    return info(uloc).units
end

"""
    units(@nospecialize(ids::IDS), field::Symbol)

Return string with units for a given IDS field
"""
function units(@nospecialize(ids::IDS), field::Symbol)
    return units(ulocation(ids, field))
end

export units
push!(document[:Base], :units)

struct Coordinate{T<:Real}
    ids::Union{IDS{T},IDSvector{T}}
    field::Symbol
end

function Base.show(io::IO, @nospecialize(coord::Coordinate{<:Real}))
    return println(io, "$(location(coord.ids, coord.field))")
end

"""
    coordinates(@nospecialize(ids::IDS), field::Symbol; override_coord_leaves::Union{Nothing,Vector{<:Union{Nothing,Symbol}}}=nothing)

Return a vector of Coordinate with the .ids and .field filled to point at the coordinate entries in the dd

if field === :- then there's no coordinate

Use `override_coord_leaves` to override fetching coordinates of a given field

NOTE:
getproperty(coords[X]) value is `nothing` when the data does not have a coordinate

    getproperty(coords[X]) Coordinate value is `missing` if the coordinate is missing in the data structure
"""
function coordinates(@nospecialize(ids::IDS), field::Symbol; override_coord_leaves::Union{Nothing,Vector{<:Union{Nothing,Symbol}}}=nothing)
    T = eltype(ids)

    coord_locs = info(ids, field).coordinates
    coords = Vector{Coordinate{T}}(undef, length(coord_locs))

    for (k, coord) in enumerate(coord_locs)
        if occursin("...", coord)
            if (override_coord_leaves === nothing) || (override_coord_leaves[k] === nothing)
                coords[k] = Coordinate{T}(ids, Symbol(coord))
            else
                coords[k] = Coordinate{T}(ids, override_coord_leaves[k])
            end
        else
            coord_path, coord_leaf_string = rsplit2(coord, '.')
            if (override_coord_leaves === nothing) || (override_coord_leaves[k] === nothing)
                coord_leaf = Symbol(coord_leaf_string)
            else
                coord_leaf = override_coord_leaves[k]
            end
            h = goto(ids, coord_path)
            coords[k] = Coordinate{T}(h, coord_leaf)
        end
    end
    return coords
end

@inline function Base.getproperty(coord::Coordinate; return_missing_time::Bool=false)
    if occursin("...", string(coord.field))
        return nothing
    end
    value = getproperty(coord.ids, coord.field, missing)
    if coord.field == :time && value === missing
        tmp = time_array_from_parent_ids(coord.ids, Val(:get))
        if return_missing_time && isempty(tmp)
            return missing
        else
            return tmp
        end
    else
        return value
    end
end

@inline function location(coord::Coordinate)
    if occursin("...", string(coord.field))
        return coord.field
    end
    return location(coord.ids, coord.field)
end

export coordinates
push!(document[:Base], :coordinates)

"""
    time_coordinate_index(@nospecialize(ids::IDS), field::Symbol; error_if_not_time_dependent::Bool)

Return index of time coordinate

If `error_if_not_time_dependent == false` it will return `0` for arrays that are not time dependent
"""
function time_coordinate_index(@nospecialize(ids::IDS), field::Symbol; error_if_not_time_dependent::Bool)
    coordinates = info(ids, field).coordinates
    if field == :time && length(coordinates) == 1 && coordinates[1] == "1...N"
        return 1
    end
    for (k, coord) in enumerate(coordinates)
        if rsplit2(coord, '.')[end] == "time"
            return k
        end
    end
    if error_if_not_time_dependent
        if fieldtype_typeof(ids, field) <: Array
            error("$(location(ids)).$(field) does not have a time coordinate. It depends on $(coordinates)")
        else
            error("$(location(ids)).$(field) does not have a time coordinate. It's of type $(fieldtype_typeof(ids, field))")
        end
    end
    return 0
end

export time_coordinate_index
push!(document[:Base], :time_coordinate_index)

"""
    time_coordinate(@nospecialize(ids::IDS), field::Symbol)

Return a vector of Coordinate with the .ids and .field filled to point at the time coordinate of the field
"""
function time_coordinate(@nospecialize(ids::IDS), field::Symbol)
    return coordinates(ids, field)[time_coordinate_index(ids, field; error_if_not_time_dependent=true)]
end
export time_coordinate
push!(document[:Base], :time_coordinate)

#= ========== =#
#  access log  #
#= ========== =#
mutable struct AccessLog
    enabled::Bool
    read::Set{String}
    expr::Set{String}
    write::Set{String}
end

"""
    IMASdd.access_log

    IMASdd.access_log.enable = true / false

    @show IMASdd.access_log

    empty!(IMASdd.access_log) # to reset

Track access to the data dictionary
"""
const access_log = AccessLog(false, Set(String[]), Set(String[]), Set(String[]))

function Base.empty!(access_log::AccessLog)
    empty!(access_log.read)
    empty!(access_log.expr)
    empty!(access_log.write)
    return nothing
end

function Base.show(io::IO, access_log::AccessLog)
    for field in (:read, :expr, :write)
        log = getfield(access_log, field)
        for k in sort!(collect(log))
            println(io, "$field: $k")
        end
        println(io, "")
    end
end

export access_log
push!(document[:Base], :access_log)

#= === =#
#  IDS  #
#= === =#
"""
    fieldtype_typeof(ids, field)

Returns typeof the field in an IDS

Please note that in the DD array types are defined as Array{<:D,N} and not Array{D,N}
"""
function fieldtype_typeof(ids, field)
    return fieldtype(typeof(ids), field)
end

"""
    concrete_fieldtype_typeof(ids, field)

Returns the concrete typeof of a field in a given ids object, ensuring that Array{<:D, N} is converted to Array{D, N}.
"""
function concrete_fieldtype_typeof(ids, field)
    return concrete_array_type(fieldtype_typeof(ids, field))
end

function concrete_fieldtype_typeof(ids::IDS{Float64}, field)
    return fieldtype_typeof(ids, field)
end

"""
    concrete_array_type(T)

Converts Array{<:D,N} to Array{D,N}
"""
function concrete_array_type(T)
    if !(T <: Array)
        return T
    end
    S, N = Base.unwrap_unionall(T).parameters
    D = S isa TypeVar ? S.ub : S  # 'ub' gives the upper bound of TypeVar
    return Array{D,N}
end

function eltype_concrete_fieldtype_typeof(ids, field)
    return eltype(concrete_array_type(fieldtype(typeof(ids), field)))
end

function eltype_concrete_fieldtype_typeof(ids::IDS{Float64}, field)
    return eltype(fieldtype(typeof(ids), field))
end

"""
    Base.getproperty(ids::Union{IDSraw, IDSvectorRawElement}, field::Symbol)

No processing for IDSraw and IDSvectorRawElement
"""
@inline function Base.getproperty(ids::Union{DD,IDSraw,IDSvectorRawElement}, field::Symbol)
    return getfield(ids, field)
end

"""
    getproperty(ids::IDS, field::Symbol; to_cocos::Int=user_cocos)

Return IDS value for requested field
"""
Base.@constprop :aggressive function Base.getproperty(ids::IDS, field::Symbol; to_cocos::Int=user_cocos)
    if fieldtype_typeof(ids, field) <: Union{IDS,IDSvector}
        # is an IDS or IDSvector

    elseif hasdata(ids, field)
        # has data

    elseif field == :time && !(typeof(ids) <: IDStop)
        # if missing time, set time from parent vector that has time information
        # this is necessary to work with IDSs that were generated with homogeneous_time=1
        # Effectively this behaves like one-time expressions for time
        time_array = time_array_from_parent_ids(ids, Val(:get))
        if typeof(ids) <: IDSvectorTimeElement
            ids.time = time_array[index(ids)]
        else
            ids.time = time_array
        end

    elseif !isfrozen(ids)
        exec_expression_with_ancestor_args(ids, field; throw_on_missing=true)
    end

    value = getfield(ids, field)
    cocos_out(ids, field, value, to_cocos)
    return value
end

"""
    getproperty(ids::IDS, field::Symbol, @nospecialize(default::Any); to_cocos::Int=user_cocos)

Return IDS value for requested field or `default` if field is missing

NOTE: This is useful because accessing a `missing` field in an IDS would raise an error
"""
Base.@constprop :aggressive function Base.getproperty(ids::IDS, field::Symbol, @nospecialize(default::Any); to_cocos::Int=user_cocos)
    valid = false

    if fieldtype_typeof(ids, field) <: Union{IDS,IDSvector}
        # is an IDS or IDSvector
        valid = true

    elseif hasdata(ids, field)
        # has data
        valid = true

    elseif !isfrozen(ids)
        # check
        valid = exec_expression_with_ancestor_args(ids, field; throw_on_missing=false)
    end

    if valid
        value = getfield(ids, field)
        return cocos_out(ids, field, value, to_cocos)
    else
        return default
    end
end

export getproperty
push!(document[:Base], :getproperty)

"""
    getraw(@nospecialize(ids::IDS), field::Symbol)

Returns data, expression function, or missing

  - Does not raise an error on missing data, returns missing
  - Does not evaluate expressions
"""
function getraw(@nospecialize(ids::IDS), field::Symbol)
    value = getfield(ids, field)

    if typeof(value) <: Union{IDS,IDSvector}
        # nothing to do for data structures
        return value

    elseif field == :global_time
        # global time
        return value

    elseif hasdata(ids, field)
        # has data
        return value

    elseif hasexpr(ids, field)
        # has an expression
        return getexpr(ids, field)

    else
        # missing data
        return missing
    end
end

"""
    isempty(@nospecialize(ids::IDSvector))

returns true if IDSvector is empty
"""
@inline function Base.isempty(@nospecialize(ids::IDSvector))
    return isempty(ids._value)
end

"""
    isempty(@nospecialize(ids::IDS); include_expr::Bool=false, eval_expr::Bool=false)

Returns true if none of the IDS fields downstream have data (or expressions)

NOTE: By default it does not include nor evaluate expressions
"""
function Base.isempty(@nospecialize(ids::IDS); include_expr::Bool=false, eval_expr::Bool=false)
    if hasdata(ids)
        return false
    end
    if include_expr
        if eval_expr
            np = NoSpecialize(ids)
            return !any(!isempty(np.ids, field; include_expr, eval_expr) for field in keys(np.ids))
        else
            return !hasexpr(ids)
        end
    else
        return true
    end
end

"""
    isempty(@nospecialize(ids::IDS), field::Symbol; include_expr::Bool=false, eval_expr::Bool=false)

Returns true if the ids field has no data (or expression)

NOTE: By default it does not include nor evaluate expressions
"""
function Base.isempty(@nospecialize(ids::IDS), field::Symbol; include_expr::Bool=false, eval_expr::Bool=false)
    value = getfield(ids, field)
    if typeof(value) <: IDSvector # filled arrays of structures
        return isempty(value)
    elseif typeof(value) <: IDS # filled structures
        return isempty(value; include_expr, eval_expr)
    elseif eval_expr
        return getproperty(ids, field, missing) === missing
    elseif include_expr
        return !(hasdata(ids, field) || hasexpr(ids, field))
    else
        return !hasdata(ids, field)
    end
end

export isempty
push!(document[:Base], :isempty)

"""
    isfrozen(@nospecialize(ids::IDS))

Returns if the ids has been frozen
"""
@inline function isfrozen(@nospecialize(ids::IDS))
    return getfield(ids, :_frozen)
end

export isfrozen
push!(document[:Base], :isfrozen)

"""
    _setproperty!(@nospecialize(ids::IDS{<:Real}), field::Symbol, @nospecialize(value::Any); from_cocos::Int)

Like setfield! but also add to list of filled fields
"""
function _setproperty!(@nospecialize(ids::IDS{<:Real}), field::Symbol, @nospecialize(value::Any); from_cocos::Int)

    if typeof(value) <: Union{AbstractRange,StaticArraysCore.SVector,StaticArraysCore.MVector,SubArray}
        value = collect(value)
    end

    # convert cocos and types
    __convert!(ids, field, value, eltype_concrete_fieldtype_typeof(ids, field), from_cocos)

    # add to list of filled fields
    add_filled(ids, field)

    # log write access
    if access_log.enabled
        push!(access_log.write, ulocation(ids, field))
    end

    return value
end

function _setproperty!(@nospecialize(ids::IDS{<:Real}), field::Symbol, @nospecialize(value::Union{IDS,IDSvector}); from_cocos::Int)
    setfield!(value, :_parent, WeakRef(ids))
    add_filled(ids, field)
    return setfield!(ids, field, value)
end

# Real to Real
function __convert!(@nospecialize(ids::IDS{<:Real}), field::Symbol, @nospecialize(value::Union{Real,Array{<:Real}}), eltype_in_ids::Type{<:Real}, from_cocos::Int)
    # may need cocos conversion
    if from_cocos != internal_cocos
        cocos_multiplier = transform_cocos_coming_in(ids, field, from_cocos)
        if cocos_multiplier != 1.0
            value = cocos_multiplier .* value
        end
    end

    # setfield
    return setfield!(ids, field, value)
end

# Any to Float64 # we're strict when IDSs are of type Float64
function __convert!(@nospecialize(ids::IDS{Float64}), field::Symbol, @nospecialize(value::Any), eltype_in_ids::Type{Float64}, from_cocos::Int)
    error("`$(typeof(value))` is the wrong type for `$(ulocation(ids, field))`, it should be `$(T)`")
    return nothing
end

# Float64 to Float64 # nothing todo, breeze through
function __convert!(@nospecialize(ids::IDS{Float64}), field::Symbol, value::Union{Float64,Array{Float64}}, eltype_in_ids::Type{Float64}, from_cocos::Int)
    return setfield!(ids, field, value)
end

# Any to Real # allow conversions of reals when IDSs are not of type Float64
function __convert!(@nospecialize(ids::IDS{<:Real}), field::Symbol, @nospecialize(value::Any), eltype_in_ids::Type{<:Real}, from_cocos::Int)
    target_type = concrete_fieldtype_typeof(ids, field)
    return setfield!(ids, field, convert(target_type, value))
end

# Any to Any # nothing todo, breeze through, we'll get an error if type is not right
function __convert!(@nospecialize(ids::IDS{<:Real}), field::Symbol, @nospecialize(value::Any), eltype_in_ids::Type, from_cocos::Int)
    return setfield!(ids, field, value)
end

"""
    add_filled(@nospecialize(ids::IDS), field::Symbol)

Utility function to set the _filled field of an IDS and the upstream parents
"""
function add_filled(@nospecialize(ids::IDS), field::Symbol)
    if field !== :global_time
        setfield!(getfield(ids, :_filled), field, true)
    end
    return add_filled(ids)
end

"""
    add_filled(@nospecialize(ids::Union{IDS,IDSvector}))

Utility function to set the _filled field of the upstream parents
"""
function add_filled(@nospecialize(ids::Union{IDS,IDSvector}))
    pids = parent(ids)
    if typeof(pids) <: IDS
        pfield = name(ids)
        pfilled = getfield(pids, :_filled)
        if !getfield(pfilled, pfield)
            add_filled(pids, pfield)
        end
    end
    return nothing
end

"""
    del_filled(@nospecialize(ids::IDS), field::Symbol)

Utility function to unset the _filled field of an IDS

NOTE: this function does not call set_parent_filled()
"""
function del_filled(@nospecialize(ids::IDS), field::Symbol)
    setfield!(getfield(ids, :_filled), field, false)
    return ids
end

"""
    set_parent_filled(@nospecialize(ids::IDS), field::Symbol)

Utility function to set the _filled field of the IDSs upstream
"""
function set_parent_filled(@nospecialize(ids::IDS))
    if isempty(ids)
        pids = parent(ids)
        if typeof(pids) <: IDS
            setfield!(getfield(pids, :_filled), name(ids), false)
        end
        set_parent_filled(pids)
    end
    return nothing
end

function set_parent_filled(@nospecialize(ids::IDSvector))
    if isempty(ids)
        pids = parent(ids)
        if typeof(pids) <: IDS
            setfield!(getfield(pids, :_filled), name(ids), false)
        end
        set_parent_filled(pids)
    end
    return nothing
end

function set_parent_filled(::Nothing)
    # Handle the case when we reach the top of the hierarchy
    return nothing
end

"""
    Base.setproperty!(@nospecialize(ids::IDS), field::Symbol, value; skip_non_coordinates::Bool=false, error_on_missing_coordinates::Bool=true)
"""
function Base.setproperty!(
    @nospecialize(ids::IDS),
    field::Symbol,
    @nospecialize(value::Any);
    skip_non_coordinates::Bool=false,
    error_on_missing_coordinates::Bool=true,
    from_cocos::Int=user_cocos
)
    return _setproperty!(ids, field, value; from_cocos)
end

"""
    Base.setproperty!(@nospecialize(ids::IDS), field::Symbol, value::AbstractArray{<:IDS}; skip_non_coordinates::Bool=false, error_on_missing_coordinates::Bool=true)

Handle setproperty of entire vectors of IDS structures at once (ids.field is of type IDSvector)
"""
function Base.setproperty!(
    @nospecialize(ids::IDS),
    field::Symbol,
    value::AbstractArray{<:IDS};
    skip_non_coordinates::Bool=false,
    error_on_missing_coordinates::Bool=true,
    from_cocos::Int=user_cocos
)
    orig = getfield(ids, field)
    empty!(orig)
    append!(orig, value)
    add_filled(ids, field)
    return orig
end

"""
    Base.setproperty!(@nospecialize(ids::IDS), field::Symbol, value::AbstractArray; skip_non_coordinates::Bool=false, error_on_missing_coordinates::Bool=true)

Ensures coordinates are set before the data that depends on those coordinates.

If `skip_non_coordinates` is set, then fields that are not coordinates will be silently skipped.
"""
function Base.setproperty!(
    @nospecialize(ids::IDS),
    field::Symbol,
    value::AbstractArray;
    skip_non_coordinates::Bool=false,
    error_on_missing_coordinates::Bool=true,
    from_cocos::Int=user_cocos
)
    if !isfrozen(ids) && !hasdata(ids, field) && error_on_missing_coordinates
        # figure out the coordinates
        coords = coordinates(ids, field)

        # skip non coordinates
        if skip_non_coordinates && any(!occursin("...", string(coord.field)) for coord in coords)
            return nothing
        end

        # don't allow assigning data before coordinates
        coords_values = (getproperty(coord; return_missing_time=true) for coord in coordinates(ids, field))
        if any(ismissing, coords_values)
            coords_names = [location(coord) for coord in coordinates(ids, field)]
            error("Can't assign data to `$(location(ids, field))` before `$(coords_names)`")
        end
    end
    return _setproperty!(ids, field, value; from_cocos)
end

function Base.setproperty!(
    @nospecialize(ids::IDS),
    field::Symbol,
    value::AbstractDict;
    skip_non_coordinates::Bool=false,
    error_on_missing_coordinates::Bool=true,
    from_cocos::Int=user_cocos
)
    return _setproperty!(ids, field, string(value); from_cocos)
end

export setproperty!
push!(document[:Base], :setproperty!)

#= ======== =#
#  deepcopy  #
#= ======== =#
@inline function Base.deepcopy(@nospecialize(ids::Union{IDS,IDSvector}))
    # using fill! is much more efficient than going via Base.deepcopy_internal()
    ids_new = typeof(ids)(;frozen=getfield(ids, :_frozen))
    fill!(ids_new, ids)
    return ids_new
end

@inline function Base.deepcopy(ids::DD)
    ids_new = typeof(ids)(;frozen=getfield(ids, :_frozen))
    fill!(ids_new, ids)
    setfield!(ids_new, :global_time, getfield(ids, :global_time))
    setfield!(ids_new, :_aux, deepcopy(getfield(ids, :_aux)))
    return ids_new
end

"""
    Base.fill!(@nospecialize(IDS_new::Union{IDS,IDSvector}), @nospecialize(IDS_ori::Union{IDS,IDSvector}))

fills `IDS_new` from `IDS_ori` using a stack-based approach, instead of recursion

### Notes

  - `IDS_new` and `IDS_ori` must have matching wrapper types but can have different parametric types
  - In other words, this can be used to copy data from a IDS{Float64} to a IDS{Real} or similar
  - For this to work one must define a function:
    `Base.fill!(@nospecialize(IDS_new::IDS{T1}), @nospecialize(IDS_ori::IDS{T2}), field::Symbol) where {T1<:???, T2<:???}`
"""
function Base.fill!(@nospecialize(IDS_new::Union{IDS,IDSvector}), @nospecialize(IDS_ori::Union{IDS,IDSvector}))
    # Check type structure (comparing only wrapper, not full type)
    if !(typeof(IDS_new).name.wrapper == typeof(IDS_ori).name.wrapper)
        error("Type structures don't match: $(typeof(IDS_new).name.wrapper) vs $(typeof(IDS_ori).name.wrapper)")
    end

    stack = Tuple{Any,Any}[]

    if IDS_ori isa IDSvector
        if length(IDS_new) != length(IDS_ori)
            resize!(IDS_new, length(IDS_ori))
        end
        for i in eachindex(IDS_ori)
            push!(stack, (IDS_new[i], IDS_ori[i]))
        end
    else
        push!(stack, (IDS_new, IDS_ori))
    end

    # Process while stack is not empty
    while !isempty(stack)
        ids_new, ids = pop!(stack)

        # Get filled fields from current ids
        filled = getfield(ids, :_filled)

        for field in fieldnames(typeof(ids))
            if hasfield(typeof(filled), field) && getfield(filled, field)
                field_type = fieldtype_typeof(ids, field)

                if field_type <: IDS
                    push!(stack, (getfield(ids_new, field), getfield(ids, field)))
                    add_filled(ids_new, field)
                elseif field_type <: IDSvector
                    field_ori = getfield(ids, field)
                    if !isempty(field_ori)
                        field_new = getfield(ids_new, field)
                        if length(field_new) != length(field_ori)
                            resize!(field_new, length(field_ori))
                        end

                        for i in eachindex(field_ori)
                            push!(stack, (field_new[i], field_ori[i]))
                        end
                    end
                else
                    # call appropriate dispatch of fill!
                    fill!(ids_new, ids, field)
                end
            end
        end
    end

    return IDS_new
end

# fill - handle both same and different types
function Base.fill!(@nospecialize(ids_new::IDS{<:Real}), @nospecialize(ids::IDS{<:Real}), field::Symbol)
    value = getfield(ids, field)
    T1 = eltype(ids_new)
    T2 = eltype(ids)
    if T1 === T2 || field == :time || !(eltype(value) <: T2)
        _setproperty!(ids_new, field, deepcopy(value); from_cocos=internal_cocos)
    else
        _setproperty!(ids_new, field, T1.(value); from_cocos=internal_cocos)
    end
    return nothing
end

#= ========= =#
#  IDSvector  #
#= ========= =#
function Base.size(@nospecialize(ids::IDSvector))
    return size(ids._value)
end

function Base.length(@nospecialize(ids::IDSvector{<:IDSvectorElement}))
    return length(ids._value)
end

function Base.getindex(@nospecialize(ids::IDSvector{<:IDSvectorElement}), i::Int)
    if 1 <= i <= length(ids._value)
        return ids._value[i]
    elseif i < 1
        error("Attempt to access $(length(ids))-element $(typeof(ids)) at index [$i]. Need start indexing at 1.")
    else
        error("Attempt to access $(length(ids))-element $(typeof(ids)) at index [$i]. Need to `resize!(ids, $i)`.")
    end
end

function Base.setindex!(@nospecialize(ids::IDSvector{<:IDSvectorElement}), @nospecialize(value::IDSvectorElement), i::Integer)
    ids._value[i] = value
    setfield!(value, :_parent, WeakRef(ids))
    add_filled(ids)
    return value
end

function Base.push!(@nospecialize(ids::IDSvector{<:IDSvectorElement}), @nospecialize(value::IDSvectorElement))
    setfield!(value, :_parent, WeakRef(ids))
    push!(ids._value, value)
    add_filled(ids)
    return ids
end

function Base.append!(@nospecialize(ids::IDSvector{<:IDSvectorElement}), @nospecialize(values::AbstractVector{<:IDSvectorElement}))
    for value in values
        push!(ids, value)
    end
    return ids
end

function Base.push!(@nospecialize(ids::IDSvector{<:IDSvectorElement}), @nospecialize(value::Any))
    return error("`push!` on $(location(ids)) must be of type $(T) and is instead of type $(typeof(value))")
end

function Base.pushfirst!(@nospecialize(ids::IDSvector{<:IDSvectorElement}), @nospecialize(value::IDSvectorElement))
    setfield!(value, :_parent, WeakRef(ids))
    pushfirst!(ids._value, value)
    add_filled(ids)
    return ids
end

function Base.insert!(@nospecialize(ids::IDSvector{<:IDSvectorElement}), i, @nospecialize(value::IDSvectorElement))
    setfield!(value, :_parent, WeakRef(ids))
    insert!(ids._value, i, value)
    add_filled(ids)
    return value
end

function Base.pop!(@nospecialize(ids::IDSvector{<:IDSvectorElement}))
    tmp = pop!(ids._value)
    set_parent_filled(ids)
    return tmp
end

function Base.popfirst!(@nospecialize(ids::IDSvector{<:IDSvectorElement}))
    tmp = popfirst!(ids._value)
    set_parent_filled(ids)
    return tmp
end

function Base.popat!(@nospecialize(ids::IDSvector{<:IDSvectorElement}), index::Int)
    tmp = popat!(ids._value, index)
    set_parent_filled(ids)
    return tmp
end

"""
    merge!(@nospecialize(target_ids::IDS), @nospecialize(source_ids::IDS))
"""
function Base.merge!(@nospecialize(target_ids::IDS), @nospecialize(source_ids::IDS))
    @assert typeof(target_ids) === typeof(source_ids) "Cannot merge different IDS types: $(typeof(target_ids)) != $(typeof(source_ids))"
    for field in keys_no_missing(source_ids; include_expr=false, eval_expr=false)
        value = getproperty(source_ids, field)
        _setproperty!(target_ids, field, value; from_cocos=internal_cocos)
    end
    return target_ids
end

function Base.merge!(@nospecialize(target_ids::IDSvector), @nospecialize(source_ids::IDSvector))
    @assert eltype(target_ids) === eltype(source_ids) "Cannot merge IDSvectors with different element types: $(eltype(target_ids)) != $(eltype(source_ids))"
    for (k, value) in enumerate(source_ids)
        if k <= length(target_ids)
            target_ids[k] = value
        else
            push!(target_ids, value)
        end
    end
    return target_ids
end

"""
    index(@nospecialize(ids::IDSvectorElement))

Returns index of the IDSvectorElement in the parent IDSvector
"""
@inline function index(@nospecialize(ids::IDSvectorElement))
    return index(parent(ids), ids)
end

@inline function index(@nospecialize(idss::IDSvector{<:IDSvectorElement}), @nospecialize(ids::IDSvectorElement))
    if isempty(idss)
        return 0
    end
    n = findfirst(k === ids for k in idss._value)
    if n === nothing
        # this happens when doing freeze(ids)
        return 0
    else
        return n
    end
end

@inline function index(::Nothing, @nospecialize(ids::IDSvectorElement))
    return 0
end

@inline function index(@nospecialize(ids::IDS))
    # this function does not make sense per se
    # but it solves an issue with type stability
    return 0
end

@inline function index(::Nothing, @nospecialize(ids::IDS))
    # this function does not make sense per se
    # but it solves an issue with type stability
    return 0
end

export index
push!(document[:Base], :index)

#= ==== =#
#  keys  #
#= ==== =#
"""
    _common_base_string(s1::String, s2::String)

Given two strings, returns a tuple of 3 strings:

  - the common initial part,
  - the remaining part of `s1`,
  - the remaining part of `s2`.
"""
@inline function _common_base_string(s1::AbstractString, s2::AbstractString)
    n = min(ncodeunits(s1), ncodeunits(s2))
    i = 0
    while i < n && s1[i+1] == s2[i+1]
        i += 1
    end
    return SubString(s1, 1, i), SubString(s1, i + 1), SubString(s2, i + 1)
end

"""
    keys(@nospecialize(ids::IDS))

Returns generator of fields in a IDS whether they are filled with data or not
"""
@inline function Base.keys(@nospecialize(ids::IDS))
    return fieldnames(typeof(getfield(ids, :_filled)))
end

"""
    keys(ids::IDSvector)

Returns range 1:length(ids)
"""
@inline function Base.keys(@nospecialize(ids::IDSvector))
    return 1:length(ids)
end

"""
    keys_no_missing(@nospecialize(ids::IDS); include_expr::Bool=true, eval_expr::Bool=false)

Returns generator of fields with data in a IDS

NOTE: By default it includes expressions, but does not evaluate them.
It assumes that a IDStop without data will also have no valid expressions.
"""
function keys_no_missing(@nospecialize(ids::IDS); include_expr::Bool=true, eval_expr::Bool=false)
    ns = NoSpecialize(ids)
    return (field for field in keys(ns.ids) if !isempty(ns.ids, field; include_expr, eval_expr))
end

function keys_no_missing(ids::DD; include_expr::Bool=false, eval_expr::Bool=false)
    ns = NoSpecialize(ids)
    return (field for field in keys(ns.ids) if !isempty(ns.ids, field; include_expr, eval_expr))
end

export keys_no_missing
push!(document[:Base], :keys_no_missing)

function Base.pairs(::IDS)
    return error("`pairs(ids)` is purposely not defined since with expressions it's unclear what one would want to iterate on.\\Use `keys()` or `IMAS.keys_no_missing()` instead.")
end

function Base.values(::IDS)
    return error("`values(ids)` is purposely not defined since with expressions it's unclear what one would want to iterate on.\\Use `keys()` or `IMAS.keys_no_missing()` instead.")
end

function Base.iterate(::IDS)
    return error(
        "`iterate(ids)` is purposely not defined since with expressions it's unclear what one would want to iterate on.\\Use `keys()` or `IMAS.keys_no_missing()` instead."
    )
end

#= ====== =#
#  empty!  #
#= ====== =#
function Base.empty!(@nospecialize(ids::IDS))
    @assert isempty(in_expression(ids))
    for field in fieldnames(typeof(ids))
        if field ∈ private_fields || field == :global_time
            # pass
        else
            _empty!(ids, field)
        end
    end
    set_parent_filled(ids)
    return ids
end

function Base.empty!(@nospecialize(ids::IDS), field::Symbol)
    tmp = _empty!(ids, field)
    set_parent_filled(ids)
    return tmp
end

function _empty!(@nospecialize(ids::IDS), field::Symbol)
    value = getfield(ids, field)
    if typeof(value) <: Union{IDS,IDSvector} && !isempty(value)
        empty!(value)
    elseif typeof(value) <: Vector
        setfield!(ids, field, typeof(value)())
    end
    del_filled(ids, field)
    return value
end

function Base.empty!(@nospecialize(ids::IDSvector))
    empty!(ids._value)
    set_parent_filled(ids)
    return ids
end

#= ======= =#
#  resize!  #
#= ======= =#
function Base.resize!(@nospecialize(ids::IDSvector{<:IDSvectorElement}), n::Int; wipe::Bool=true)
    if n > length(ids)
        for k in length(ids):n-1
            push!(ids, eltype(ids)(;frozen=getfield(ids,:_frozen)))
        end
    elseif n < length(ids)
        for k in n:length(ids)-1
            pop!(ids)
        end
    end
    if wipe && !isempty(ids)
        empty!(ids[end])
    end
    return ids
end

"""
    Base.resize!(@nospecialize(ids::IDSvector{T}), condition::Pair{String}, conditions::Pair{String}...; wipe::Bool=true, error_multiple_matches::Bool=true) where {T<:IDSvectorElement}

Resize if a set of conditions are not met.

If wipe=true and an entry matching the condition is found, then the content of the matching IDS is emptied.

Either way, the IDS is populated with the conditions.

NOTE: `error_multiple_matches` will delete all extra entries matching the conditions.

Returns the selected IDS
"""
function Base.resize!(
    @nospecialize(ids::IDSvector{<:IDSvectorElement}),
    condition::Pair{String},
    conditions::Pair{String}...;
    wipe::Bool=true,
    error_multiple_matches::Bool=true)

    conditions = vcat(condition, collect(conditions))
    if isempty(ids)
        return _set_conditions(resize!(ids, 1; wipe)[1], conditions...)
    end
    matches = _match(ids, conditions)
    if length(matches) == 1
        match = first(values(matches))
        if wipe
            empty!(match)
        end
        return _set_conditions(match, conditions...)
    elseif length(matches) > 1
        if error_multiple_matches
            error("Multiple entries $([k for k in keys(matches)]) of $(location(ids)) match resize!() conditions: $conditions")
        else
            for (kk, k) in reverse!(collect(enumerate(sort!(collect(keys(matches))))))
                if kk == 1
                    if wipe
                        empty!(matches[k])
                    end
                    return _set_conditions(matches[k], conditions...)
                else
                    deleteat!(ids, k)
                end
            end
        end
    else
        return _set_conditions(resize!(ids, length(ids) + 1; wipe)[length(ids)], conditions...)
    end
end

export resize!
push!(document[:Base], :resize!)

#= ========= =#
#  deleteat!  #
#= ========= =#
function Base.deleteat!(@nospecialize(ids::IDSvector), i::Int)
    deleteat!(ids._value, i)
    set_parent_filled(ids)
    return ids
end

"""
    Base.deleteat!(@nospecialize(ids::IDSvector), condition::Pair{String}, conditions::Pair{String}...)

If an entry matching the condition is found, then the content of the matching IDS is emptied
"""
function Base.deleteat!(@nospecialize(ids::IDSvector), condition::Pair{String}, conditions::Pair{String}...)
    conditions = vcat(condition, collect(conditions))
    if isempty(ids)
        return ids
    end
    matches = _match(ids, conditions)
    for k in reverse!(sort!(collect(keys(matches))))
        deleteat!(ids, k)
    end
    return ids
end

export deleteat!
push!(document[:Base], :deleteat!)

#= ========= =#
#  ismissing  #
#= ========= =#
"""
    Base.ismissing(@nospecialize(ids::IDS), field::Symbol)

returns true/false if field is missing in IDS
"""
function Base.ismissing(@nospecialize(ids::IDS), field::Symbol)
    if typeof(ids) <: DD && field === :global_time
        # nothing to do for global_time
        return false

    elseif hasdata(ids, field)
        # has data
        return false

    elseif fieldtype_typeof(ids, field) <: Union{IDS,IDSvector}
        # is an IDS or IDSvector
        return false

    else
        # check if the expression works out
        valid = exec_expression_with_ancestor_args(ids, field; throw_on_missing=false)
        return !valid
    end
    return true
end

function Base.ismissing(@nospecialize(ids::IDSvector), field::Int)
    return length(ids) < field
end

function Base.ismissing(@nospecialize(ids::IDS), path::Union{AbstractVector,Tuple})
    if length(path) == 1
        return ismissing(ids, Symbol(path[1]))
    end
    return ismissing(getfield(ids, Symbol(path[1])), path[2:end])
end

function Base.ismissing(@nospecialize(ids::IDSvector), path::Vector)
    if length(path) == 1
        return ismissing(ids, path[1])
    end
    if typeof(path[1]) <: Integer
        n = path[1]
    else
        n = parse(Int, path[1])
    end
    if n <= length(ids)
        return ismissing(ids[n], path[2:end])
    else
        return true
    end
end

export ismissing
push!(document[:Base], :ismissing)

#= ==== =#
#  diff  #
#= ==== =#
function _diff_function(v1::T, v2::T, tol::Float64) where {T}
    if v1 in (Inf, -Inf, NaN) && v2 !== v1
        return tol * 2
    else
        return maximum(abs.(v1 .- v2) ./ (tol .+ sum(abs.(v1) .+ abs.(v2)) ./ 2.0 ./ length(v1)))
    end
end

"""
    Base.diff(
        @nospecialize(ids1::T),
        @nospecialize(ids2::T);
        tol::Float64=1E-2,
        recursive::Bool=true,
        verbose::Bool=false) where {T<:IDS}

Compares two IDSs and returns dictionary with differences

NOTE: This function does not evaluate expressions (use `freeze()` on the IDSs to compare values instead of functions)
"""
function Base.diff(
    @nospecialize(ids1::IDS),
    @nospecialize(ids2::IDS);
    tol::Float64=1E-2,
    recursive::Bool=true,
    verbose::Bool=false) 

    # check type first
    if !isequal(typeof(ids1), typeof(ids2))
        return Dict{String, String}( "type_mismatch" => "$(typeof(ids1)) != $(typeof(ids2))")
    end

    return diff(ids1, ids2, String[], Dict{String,String}(); tol, recursive, verbose)
end

function Base.diff(
    @nospecialize(ids1::IDS),
    @nospecialize(ids2::IDS),
    path::Vector{<:AbstractString},
    differences::Dict{String,String};
    tol::Float64=1E-2,
    recursive::Bool=true,
    verbose::Bool=true)

    # check type first
    if !isequal(typeof(ids1), typeof(ids2))
        return Dict{String, String}( "type_mismatch" => "$(typeof(ids1)) != $(typeof(ids2))")
    end

    for field in keys(ids2)
        v1 = getraw(ids1, field)
        v2 = getraw(ids2, field)

        pathname = p2i(String[path; "$field"])
        if typeof(v1) != typeof(v2)
            differences[pathname] = "types:  $(typeof(v1)) --  $(typeof(v2))"
        elseif typeof(v1) <: Missing
            continue
        elseif typeof(v1) <: Function
            continue # we do not compare anonymous functions
        elseif typeof(v1) <: IDS
            if recursive
                diff(v1, v2, String[path; "$field"], differences; tol, recursive, verbose)
            end
        elseif typeof(v1) <: IDSvector
            if recursive
                if length(v1) != length(v2)
                    differences[pathname] = "length:  $(length(v1)) --  $(length(v2))"
                else
                    for k in 1:length(v1)
                        diff(v1[k], v2[k], String[path; "$field"; "$k"], differences; tol, recursive, verbose)
                    end
                end
            end
        elseif typeof(v1) <: AbstractArray
            if isempty(v1) && isempty(v2)
                continue
            elseif length(v1) != length(v2)
                differences[pathname] = "length:  $(length(v1)) --  $(length(v2))"
            elseif _diff_function(v1, v2, tol) > tol
                differences[pathname] = @sprintf("value: %g", _diff_function(v1, v2, tol))
            end
        elseif typeof(v1) <: Number
            if _diff_function(v1, v2, tol) > tol
                differences[pathname] = "value:  $v1 --  $v2"
            end
        elseif typeof(v1) <: Union{String,Symbol}
            if v1 != v2
                differences[pathname] = "value:  $v1 --  $v2"
            end
        else
            # raise error to force use to handle this explicitly
            error("Unhandled difference: $((pathname, typeof(v1), typeof(v2)))")
        end
        if verbose && (pathname in keys(differences))
            printstyled(pathname; bold=true)
            printstyled(" ➡ "; color=:red)
            println(differences[pathname])
        end
    end
    return differences
end

export diff
push!(document[:Base], :diff)

#= ========== =#
#  navigation  #
#= ========== =#
"""
    top_ids(@nospecialize(ids::Union{IDS,IDSvector}))

Return top-level IDS in the hierarchy and `nothing` if top level is not a top-level IDS
"""
function top_ids(@nospecialize(ids::Union{IDS,IDSvector}))
    if typeof(ids) <: DD
        error("No ids is above dd")
    end
    if typeof(ids) <: IDStop
        return ids
    end
    pids = parent(ids)
    if pids === nothing
        return nothing
    else
        return top_ids(pids)
    end
end

export top_ids
push!(document[:Base], :top_ids)

"""
    top_dd(@nospecialize(ids::Union{IDS,IDSvector}))

Return top-level `dd` in the hierarchy, and `nothing` if top level is not `dd`
"""
function top_dd(@nospecialize(ids::Union{IDS,IDSvector}))
    if typeof(ids) <: DD
        return ids
    end
    pids = parent(ids)
    if pids === nothing
        return nothing
    else
        return top_dd(pids)
    end
end

export top_dd
push!(document[:Base], :top_dd)

"""
    parent(@nospecialize(ids::Union{IDS,IDSvector}); error_parent_of_nothing::Bool=true)

Return parent IDS/IDSvector in the hierarchy

If `error_parent_of_nothing=true` then asking `parent(nothing)` will just return nothing
"""
function Base.parent(ids::Union{IDS,IDSvector}; error_parent_of_nothing::Bool=true)
    return getfield(ids, :_parent).value
end

function Base.parent(ids::Nothing; error_parent_of_nothing::Bool=true)
    if error_parent_of_nothing
        error("Asking parent of Nothing")
    else
        return nothing
    end
end

export parent
push!(document[:Base], :parent)

"""
    name(ids::Union{IDS,IDSvector})

Return name of the IDS
"""
@inline function name(ids::Union{IDS,IDSvector})
    return getfield(ids, :_name)
end

export name
push!(document[:Base], :name)

"""
    goto(@nospecialize(ids::Union{IDS,IDSvector}), loc_fs::String)

Reach location in a given IDS

NOTE: loc_fs is the path expressed in fs format
"""
function goto(ids::Union{IDS,IDSvector}, loc_fs::AbstractString)
    # find common ancestor
    cs, s1, s2 = _common_base_string(ulocation(ids), loc_fs)
    s2 = lstrip(s2, '_')
    cs0 = cs
    if endswith(cs0, "__") && !endswith(cs0, "___")
        cs0 = cs0[1:end-2]
    end
    cs0 = rstrip(cs0, '.')

    # go upstream until common acestor
    h = ids
    while ulocation(h) != cs0
        parent_value = parent(h)
        if parent_value === nothing
            break
        end
        h = parent_value
    end

    # then dive into the location branch
    for p in i2p(s2)
        if isdigit(p[1])
            n = parse(Int, p)
            if n <= length(h)
                h = h[n]
            else
                error(IMASdetachedHead("$(location(ids))", loc_fs))
            end
        else
            if hasfield(typeof(h), Symbol(p))
                h = getfield(h, Symbol(p))
            else
                error(IMASdetachedHead("$(location(ids))", loc_fs))
            end
        end
    end

    return h
end

"""
    goto(@nospecialize(ids::Union{IDS,IDSvector}), path::Union{AbstractVector,Tuple})

Reach location in a given IDS
"""
function goto(@nospecialize(ids::Union{IDS,IDSvector}), path::Union{AbstractVector,Tuple})
    if isempty(path)
        return ids
    elseif typeof(path[1]) <: Symbol
        return goto(getproperty(ids, path[1]), path[2:end])
    elseif typeof(path[1]) <: Int
        return goto(ids[path[1]], path[2:end])
    else
        error("goto cannot be of type `$(typeof(path[1]))")
    end
end

export goto
push!(document[:Base], :goto)

"""
    leaves(@nospecialize(ids::IDS))

Returns iterator with (filled) leaves in the IDS
"""
function leaves(@nospecialize(ids::IDS))
    return AbstractTrees.Leaves(ids)
end

export leaves
push!(document[:Base], :leaves)

#= ===== =#
#  paths  #
#= ===== =#
"""
    filled_ids_fields(@nospecialize(ids::IDS); eval_expr::Bool=false)

Returns a vector with tuples pointing to all the (ids, field) that have data downstream
"""
function filled_ids_fields(@nospecialize(ids::IDS); eval_expr::Bool=false)
    ret = OrderedCollections.OrderedDict{String,Tuple{<:IDS,Symbol}}()
    path = location(ids)
    filled_ids_fields!(ret, ids, path; eval_expr)
    return ret
end

function filled_ids_fields!(ret::AbstractDict{String,Tuple{<:IDS,Symbol}}, @nospecialize(ids::IDS), ppath::String; eval_expr::Bool=false)
    for field in keys_no_missing(ids; eval_expr=false)
        path = "$ppath.$field"
        if fieldtype_typeof(ids, field) <: Union{IDS,IDSvector}
            filled_ids_fields!(ret, getfield(ids, field), path; eval_expr)
        elseif eval_expr
            value = getproperty(ids, field, missing)
            if value !== missing
                ret[path] = (ids, field)
            end
        else
            ret[path] = (ids, field)
        end
    end
end

function filled_ids_fields!(ret::AbstractDict{String,Tuple{<:IDS,Symbol}}, @nospecialize(ids::IDSvector), ppath::String; eval_expr::Bool=false)
    for k in eachindex(ids)
        path = "$ppath[$k]"
        filled_ids_fields!(ret, ids[k], path; eval_expr)
    end
end

export filled_ids_fields
push!(document[:Base], :filled_ids_fields)

"""
    paths(@nospecialize(ids::IDS); eval_expr::Bool=false)

Returns the locations in the IDS that have data downstream
"""
function paths(@nospecialize(ids::IDS); eval_expr::Bool=false)
    return keys(filled_ids_fields(ids; eval_expr))
end

export paths
push!(document[:Base], :paths)

#= ============== =#
#  selective_copy  #
#= ============== =#
"""
    selective_copy!(@nospecialize(h_in::IDS), @nospecialize(h_out::IDS), path::Vector{<:AbstractString}, time0::Float64)

Copies the content of a path from one IDS to another (if the path exists) at a given time0

NOTE:

  - the path is a i2p(ulocation)
  - if time0 is NaN then all times are retained
"""
function selective_copy!(@nospecialize(h_in::IDS), @nospecialize(h_out::IDS), path::Vector{<:AbstractString}, time0::Float64)
    field = Symbol(path[1])
    if length(path) == 1
        raw_value = getraw(h_in, field)
        if !ismissing(h_in, field) # at the leaf
            if !isnan(time0) && typeof(raw_value) <: Vector && (field == :time || any(coord.field == :time for coord in coordinates(h_in, field)))
                value = get_time_array(h_in, field, [time0])
            else
                value = getproperty(h_in, field)
            end
            _setproperty!(h_out, Symbol(path[end]), value; from_cocos=internal_cocos)
        end
    else # plain IDS
        selective_copy!(getfield(h_in, field), getfield(h_out, field), path[2:end], time0)
    end
    if typeof(h_out) <: IMASdd.DD
        if time0 != NaN
            h_out.global_time = time0
        else
            h_out.global_time = h_in.global_time
        end
    end
    return nothing
end

function selective_copy!(@nospecialize(h_in::IDSvector), @nospecialize(h_out::IDSvector), path::Vector{<:AbstractString}, time0::Float64)
    if isempty(h_in)
        #pass
    elseif eltype(h_in) <: IDSvectorTimeElement && !isnan(time0)
        h_in = getindex(h_in, time0)
        if isempty(h_out)
            resize!(h_out, time0)
        end
        h_out = getindex(h_out, time0)
        selective_copy!(h_in, h_out, path[2:end], time0)
    elseif length(path) > 1
        if isempty(h_out)
            resize!(h_out, length(h_in))
        end
        for k in 1:length(h_in)
            selective_copy!(getindex(h_in, k), getindex(h_out, k), path[2:end], time0)
        end
    end
    return nothing
end

export selective_copy!
push!(document[:Base], :selective_copy!)

#= ================ =#
#  selective_delete  #
#= ================ =#
"""
    selective_delete!(@nospecialize(h_in::IDS), path::Vector{<:AbstractString})

Deletes a path from one IDS

NOTE:

  - the path is a i2p(ulocation)
"""
function selective_delete!(@nospecialize(h_in::IDS), path::Vector{<:AbstractString})
    field = Symbol(path[1])
    if length(path) == 1
        if hasdata(h_in, field)
            empty!(h_in, field)
            return true
        end
    else # plain IDS
        return selective_delete!(getfield(h_in, field), path[2:end])
    end
    return false
end

function selective_delete!(@nospecialize(h_in::IDSvector), path::Vector{<:AbstractString})
    if isempty(h_in)
        #pass
        return false
    elseif length(path) > 1
        for k in 1:length(h_in)
            return selective_delete!(getindex(h_in, k), path[2:end])
        end
    end
end

export selective_delete!
push!(document[:Base], :selective_delete!)