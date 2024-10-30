
using Test
import Base

mutable struct IDS{T}
    a::T
    b::Int
    c::T
    d::String
    _filled::Set{Symbol}
    function IDS{T}() where {T}
        return new{T}(0.0, 0, 0.0, "", Set{Symbol}())
    end
    IDS() = IDS{Float64}()
end

const funcreg = Dict{Symbol,Function}()
funcreg[:b] = (ids) -> 2
funcreg[:c] = (ids) -> ids.a + ids.b
funcreg[:d] = (ids) -> Error("Bad expression")

function Base.setproperty!(ids::IDS, field::Symbol, value::Any)
    setfield!(ids, field, value)
    push!(getfield(ids, :_filled), field)
    return value
end

function Base.getproperty(ids::IDS, field::Symbol)
    tp = fieldtype(typeof(ids), field)
    value = _getproperty(ids, field)
    if typeof(value) <: Exception
        return throw(value)
    else
        return value::tp
    end
end

struct ErrorExpression <: Exception
    ids::IDS
    field::Symbol
    message::String
end

function Base.showerror(io::IO, e::ErrorExpression)
    return print(io, "Expression `$(e.field)` has an error: $(e.message)")
end

function _getproperty(ids::IDS, field::Symbol)
    if field ∉ getfield(ids, :_filled)
        if field ∈ keys(funcreg)
            try
                return funcreg[field](ids)
            catch e
                return ErrorExpression(ids, field, sprint(showerror, e, catch_backtrace()))
            end
        else
            return ErrorException("Missing data for field `$field`")
        end
    else
        return getfield(ids, field)
    end
end

function Base.ismissing(ids::IDS, field::Symbol)
    value = _getproperty(ids::IDS, field::Symbol)
    if typeof(value) <: Exception
        return true
    else
        return false
    end
end

function test_me(ids)
    ids.a = 1.0
    x = ids.a
    y = ids.b
    z = ids.c
end

ids = IDS()

@test_throws Exception ids.a
@test ids.b == 2
@test_throws Exception ids.d

@test ismissing(ids, :a)
@test !ismissing(ids, :b)
@test ismissing(ids, :c)
@test ismissing(ids, :d)

ids.a = 1.0

@test ids.a == 1.0
@test ids.b == 2
@test ids.c == 3.0
@test_throws Exception ids.d

@test !ismissing(ids, :a)
@test !ismissing(ids, :b)
@test !ismissing(ids, :c)
@test ismissing(ids, :d)

@code_warntype test_me(ids)
