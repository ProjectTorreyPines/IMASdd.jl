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
    _threads_lock::ReentrantLock
end

function IDSvector(ids::Vector{T}) where {T<:IDSvectorElement}
    return IDSvector{T}(ids, WeakRef(nothing), ReentrantLock())
end

function IDSvector{T}() where {T}
    return IDSvector(T[])
end

struct Info{T<:Tuple{Vararg{String}}}
    coordinates::T
    units::String
    data_type::String
    documentation::String
    extra::Bool
    cocos_transform::Vector{String}
end

@inline function Base.eltype(@nospecialize(ids::IDS{T})) where {T}
    return T
end

const private_fields = (:_filled, :_frozen, :_threads_lock, :_in_expression, :_ref, :_parent, :_aux, :_global_time)
