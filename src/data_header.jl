#= ============================ =#
#  IDS and IDSvector structures  #
#= ============================ =#
abstract type IDS{T} end

abstract type DD{T} <: IDS{T} end

abstract type IDStop{T} <: IDS{T} end

abstract type IDSraw{T} <: IDS{T} end

abstract type IDSvectorElement{T} <: IDS{T} end

abstract type IDSvectorRawElement{T} <: IDSvectorElement{T} end

abstract type IDSvectorIonElement{T} <: IDSvectorElement{T} end

abstract type IDSvectorStaticElement{T} <: IDSvectorElement{T} end

abstract type IDSvectorTimeElement{T} <: IDSvectorElement{T} end

mutable struct IDSvector{T} <: AbstractVector{T}
    _value::Vector{T}
    _parent::WeakRef
    function IDSvector(ids::Vector{T}) where {T<:IDSvectorElement}
        return new{T}(ids, WeakRef(nothing))
    end
end

struct Info{T<:Tuple{Vararg{String}}}
    coordinates::T
    units::String
    data_type::String
    documentation::String
    extra::Bool
    cocos_transform::Vector{String}
end

IDSvector{T}() where {T} = IDSvector(T[])

@inline function Base.eltype(@nospecialize(ids::IDS{T})) where {T}
    return T
end

"""
    typed_nan(value)

Returns an equivalent of "NaN" for the same eltype of input value
"""
function typed_nan(value)
    if eltype(value) <: Int
        nan = 0
    else
        nan = NaN
    end
end

const private_fields = (:_filled, :_frozen, :_threads_lock, :_in_expression, :_parent, :_aux)
