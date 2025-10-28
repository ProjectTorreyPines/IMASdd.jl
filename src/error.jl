#= ========== =#
#  Exceptions  #
#= ========== =#

struct IMASdetachedHead <: Exception
    source::String
    destination::String
end

Base.showerror(io::IO, e::IMASdetachedHead) = print(io, "Could not reach `$(e.destination)` from `$(e.source)`")

struct IMASmissingDataException <: Exception
    ids::IDS
    field::Symbol
end

Base.showerror(io::IO, e::IMASmissingDataException) = print(io, "$(f2i(e.ids)).$(e.field) is missing")

abstract type IMASexpressionError <: Exception end

struct IMASexpressionRecursion <: IMASexpressionError
    ids::IDS
    field::Symbol
end

function Base.showerror(io::IO, e::IMASexpressionRecursion)
    loc = location(e.ids, e.field)
    return print(io, "This expression is stuck in a loop: $loc")
end

struct IMASbadExpression <: IMASexpressionError
    ids::IDS
    field::Symbol
    exception::Exception
    backtrace::Vector{Union{Ptr{Nothing}, Base.InterpreterIP}}
end

function Base.showerror(io::IO, ex::IMASbadExpression)
    print(io, "Bad expression ", location(ex.ids, ex.field), "\n")
    showerror(io, ex.exception, ex.backtrace)  # formatting happens here, only if printed
end

struct IMASbadTime <: Exception
    reason::String
end

Base.showerror(io::IO, e::IMASbadTime) = print(io, e.reason)
