document[:COCOS] = Symbol[]

"""
    internal_cocos = 11

Native COCOS used by the data structure
"""
internal_cocos = 11
user_cocos = 11

export internal_cocos
push!(document[:COCOS], :internal_cocos)

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
    if !isempty(getfield(ids, :_in_expression))
        # expressions are executed in internal COCOS
        return 1.0
    else
        return CoordinateConventions.transform_cocos(ids, field, internal_cocos, to_cocos)
    end
end

function transform_cocos_coming_in(@nospecialize(ids::IDS), field::Symbol, from_cocos::Int)
    CoordinateConventions.transform_cocos(ids, field, from_cocos, internal_cocos)
end

macro cocos11(ex)
    return _cocos11(ex)
end

function _cocos11(ex)
    value = gensym()
    ids = gensym()
    field = gensym()
    if ex.head == :(=)
        quote
            $value = $(esc(ex.args[2]))
            $ids = $(esc(ex.args[1].args[1]))
            $field = $(esc(ex.args[1].args[2]))
            _setproperty!($ids, $field, $value; from_cocos=11)
        end
    else
        quote
            $ids = $(esc(ex.args[1]))
            $field = $(esc(ex.args[2]))
            _getproperty($ids, $field; to_cocos=11)
        end
    end
end

export @cocos11
push!(document[:COCOS], Symbol("@cocos11"))

macro cocos17(ex)
    return _cocos17(ex)
end

function _cocos17(ex)
    value = gensym()
    ids = gensym()
    field = gensym()
    if ex.head == :(=)
        quote
            $value = $(esc(ex.args[2]))
            $ids = $(esc(ex.args[1].args[1]))
            $field = $(esc(ex.args[1].args[2]))
            _setproperty!($ids, $field, $value; from_cocos=17)
        end
    else
        quote
            $ids = $(esc(ex.args[1]))
            $field = $(esc(ex.args[2]))
            _getproperty($ids, $field; to_cocos=17)
        end
    end
end

export @cocos17
push!(document[:COCOS], Symbol("@cocos17"))