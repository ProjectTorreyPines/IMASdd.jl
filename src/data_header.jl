#= ============================ =#
#  IDS and IDSvector structures  #
#= ============================ =#
abstract type FilledFields end

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

struct Info
    coordinates::Vector{String}
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
function typed_nan(value::Float64)
    return NaN
end

function typed_nan(value::Int)
    return 0
end

function typed_nan(::Type{Int})
    return 0
end

function typed_nan(::Type{T}) where {T}
    return T(NaN)
end

function typed_nan(value::T) where {T}
    return T(NaN)
end

const private_fields = (:_filled, :_frozen, :_threads_lock, :_in_expression, :_parent, :_aux)
