document[:COCOS] = Symbol[]

"""
    internal_cocos = 11

Native COCOS used by the data structure
"""
const internal_cocos::Int = 11
const user_cocos::Int = 11

export internal_cocos
push!(document[:COCOS], :internal_cocos)

"""
    cocos_out(@nospecialize(ids::IDS{T}), field::Symbol, value::Union{T,AbstractArray{T}}, to_cocos::Int) where {T<:Real}

converts output from internal cocos to output cocos
"""
function cocos_out(@nospecialize(ids::IDS{T}), field::Symbol, value::Union{T,AbstractArray{T}}, to_cocos::Int) where {T<:Real}
    cocos_multiplier = transform_cocos_going_out(ids, field, to_cocos)
    if cocos_multiplier != 1.0
        return cocos_multiplier .* value
    else
        return value
    end
end

@inline function cocos_out(@nospecialize(ids::IDS), field::Symbol, @nospecialize(value::Any), to_cocos::Int)
    return value
end

"""
    cocos_transform(uloc::String)
"""
function cocos_transform(uloc::String)
    return info(uloc).cocos_transform
end

"""
    cocos_transform(@nospecialize(ids::IDS), field::Symbol)

Return Vector of strings with cocos_transform for a given IDS location

- empty vector for COCOS transforms that have not been manually assigned
- one element vector filled with
  - `""` for no COCOS transforms
  - `"?"` for COCOS transforms that should have been manually assigned but were not
  - other strings, defining the cocos transformations as per the `CoordinateConventions.jl` package
"""
function cocos_transform(@nospecialize(ids::IDS), field::Symbol)
    cocos_transform(ulocation(ids, field))
end

export cocos_transform
push!(document[:COCOS], :cocos_transform)

"""
    transform_cocos(@nospecialize(ids::IDS), field::Symbol, cc_in::Int, cc_out::Int)

Return multiplier of coordinate transformation from CoordinateConventions.jl package
"""
function CoordinateConventions.transform_cocos(@nospecialize(ids::IDS), field::Symbol, from_cocos::Int, to_cocos::Int)
    transform = cocos_transform(ids, field)
    if isempty(transform)
        return 1.0
    else
        transform_dictionary = CoordinateConventions.transform_cocos(from_cocos, to_cocos)
        if length(transform) == 1
            return transform_dictionary[transform[1]]
        else
            return Float64[transform_dictionary[tr] for tr in transform]
        end
    end
end

function transform_cocos_going_out(@nospecialize(ids::IDS), field::Symbol, to_cocos::Int)
    if internal_cocos == to_cocos
        return 1.0
    else
        if !isempty(in_expression(ids))
            # expressions are executed in internal COCOS
            return 1.0
        else
            return CoordinateConventions.transform_cocos(ids, field, internal_cocos, to_cocos)
        end
    end
end

function transform_cocos_coming_in(@nospecialize(ids::IDS), field::Symbol, from_cocos::Int)
    if internal_cocos == from_cocos
        return 1.0
    else
        if !isempty(in_expression(ids))
            # expressions are executed in internal COCOS
            return 1.0
        else
            CoordinateConventions.transform_cocos(ids, field, from_cocos, internal_cocos)
        end
    end
end

function _cocos(ex, cocos_number)
    value = gensym()
    ids = gensym()
    field = gensym()
    if ex.head == :(=)
        quote
            $value = $(esc(ex.args[2]))
            $ids = $(esc(ex.args[1].args[1]))
            $field = $(esc(ex.args[1].args[2]))
            setproperty!($ids, $field, $value, $(cocos_number))
        end
    else
        quote
            $ids = $(esc(ex.args[1]))
            $field = $(esc(ex.args[2]))
            getproperty($ids, $field; to_cocos=$(cocos_number))
        end
    end
end

macro cocos1(ex)
    return _cocos(ex, 1)
end
export @cocos1
push!(document[:COCOS], Symbol("@cocos1"))

macro cocos2(ex)
    return _cocos(ex, 2)
end
export @cocos2
push!(document[:COCOS], Symbol("@cocos2"))

macro cocos3(ex)
    return _cocos(ex, 3)
end
export @cocos3
push!(document[:COCOS], Symbol("@cocos3"))

macro cocos4(ex)
    return _cocos(ex, 4)
end
export @cocos4
push!(document[:COCOS], Symbol("@cocos4"))

macro cocos5(ex)
    return _cocos(ex, 5)
end
export @cocos5
push!(document[:COCOS], Symbol("@cocos5"))

macro cocos6(ex)
    return _cocos(ex, 6)
end
export @cocos6
push!(document[:COCOS], Symbol("@cocos6"))

macro cocos7(ex)
    return _cocos(ex, 7)
end
export @cocos7
push!(document[:COCOS], Symbol("@cocos7"))

macro cocos8(ex)
    return _cocos(ex, 8)
end
export @cocos8
push!(document[:COCOS], Symbol("@cocos8"))



macro cocos11(ex)
    return _cocos(ex, 11)
end
export @cocos11
push!(document[:COCOS], Symbol("@cocos11"))

macro cocos12(ex)
    return _cocos(ex, 12)
end
export @cocos12
push!(document[:COCOS], Symbol("@cocos12"))

macro cocos13(ex)
    return _cocos(ex, 13)
end
export @cocos13
push!(document[:COCOS], Symbol("@cocos13"))

macro cocos14(ex)
    return _cocos(ex, 14)
end
export @cocos14
push!(document[:COCOS], Symbol("@cocos14"))

macro cocos15(ex)
    return _cocos(ex, 15)
end
export @cocos15
push!(document[:COCOS], Symbol("@cocos15"))

macro cocos16(ex)
    return _cocos(ex, 16)
end
export @cocos16
push!(document[:COCOS], Symbol("@cocos16"))

macro cocos17(ex)
    return _cocos(ex, 17)
end
export @cocos17
push!(document[:COCOS], Symbol("@cocos17"))

macro cocos18(ex)
    return _cocos(ex, 18)
end
export @cocos18
push!(document[:COCOS], Symbol("@cocos18"))
