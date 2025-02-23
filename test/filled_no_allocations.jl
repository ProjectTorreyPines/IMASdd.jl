mutable struct FilledFlags
    a::Bool
    b::Bool
end

mutable struct IDS{T}
    a::T
    b::Int
    _filled::FilledFlags
end

IDS{T}() where {T} = IDS{T}(0.0, 0, FilledFlags(false, false))
IDS() = IDS{Float64}()

function Base.setproperty!(ids::IDS, field::Symbol, value::Any)
    setfield!(ids, field, value)
    setfield!(getfield(ids, :_filled), field, true)
    return value
end

Base.@constprop :aggressive function Base.getproperty(ids::IDS, field::Symbol)
    if getfield(getfield(ids, :_filled), field)
    end
    return getfield(ids, field)
end

function testme(ids)
    ids.a
    return nothing
end

ids = IDS()
ids.a = 1.0
testme(ids)

@time testme(ids)
