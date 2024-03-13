#= ============================ =#
#  IDS and IDSvector structures  #
#= ============================ =#
abstract type IDS{T} end

abstract type DD{T} <: IDS{T} end

abstract type IDStop{T} <: IDS{T} end

abstract type IDSvectorElement{T} <: IDS{T} end

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
end

IDSvector{T}() where {T} = IDSvector(T[])

@inline function Base.eltype(@nospecialize(ids::IDS))
    return typeof(ids).parameters[1]
end

private_fields = (:_filled, :_frozen, :_in_expression, :_ref, :_parent, :_aux)
