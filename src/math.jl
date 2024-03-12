import DataInterpolations
import PCHIPInterpolation

function interp1d(@nospecialize(ids::IDS), field::Symbol, scheme::Symbol=:linear)
    coord = coordinates(ids, field)
    if length(coord.values) > 1
        error("Cannot interpolate multi-dimensional $(location(ids, field)) that has coordinates $([k for k in coord.names])")
    end
    return interp1d(coord.values[1], getproperty(ids, field), scheme)
end

"""
    interp1d(x, y, scheme::Symbol=:linear)

One dimensional curve interpolations with sheme :constant, :linear, :quadratic, :cubic, :pchip, :lagrange

NOTE: this interpolation method will extrapolate
"""
function interp1d(x::AbstractVector{<:Real}, y::AbstractVector{T}, scheme::Symbol=:linear) where {T<:Real}
    # NOTE: doing simply `itp = interp1d_itp(x, y, scheme)` breaks the type inference scheme.
    @assert length(x) == length(y) "Different lengths in interp1d(x,y):  $(length(x)) and $(length(y))"
    if length(x) == 1 || scheme == :constant
        itp = DataInterpolations.ConstantInterpolation(y, x; extrapolate=true)
    elseif scheme == :pchip
        itp = PCHIPInterpolation.Interpolator(x, y)
    elseif length(x) == 2 || scheme == :linear
        itp = DataInterpolations.LinearInterpolation(y, x; extrapolate=true)
    elseif length(x) == 3 || scheme == :quadratic
        itp = DataInterpolations.QuadraticSpline(y, x; extrapolate=true)
    elseif length(x) == 4 || scheme == :cubic
        itp = DataInterpolations.CubicSpline(y, x; extrapolate=true)
    elseif scheme == :lagrange
        n = length(y) - 1
        itp = DataInterpolations.LagrangeInterpolation(y, x, n; extrapolate=true)
    else
        error("interp1d scheme can only be :constant, :linear, :quadratic, :cubic, :pchip, :lagrange ")
    end
    # NOTE: This trick is used to force a know return type. Not doing this leads to --a lot-- of type instabilities
    return x -> itp(x)::T
end

function interp1d_itp(x::AbstractVector{<:Real}, y::AbstractVector{T}, scheme::Symbol=:linear) where {T<:Real}
    # NOTE: doing simply `itp = interp1d_itp(x, y, scheme)` breaks the type inference scheme.
    @assert length(x) == length(y) "Different lengths in interp1d(x,y):  $(length(x)) and $(length(y))"
    if length(x) == 1 || scheme == :constant
        itp = DataInterpolations.ConstantInterpolation(y, x; extrapolate=true)
    elseif scheme == :pchip
        itp = PCHIPInterpolation.Interpolator(x, y)
    elseif length(x) == 2 || scheme == :linear
        itp = DataInterpolations.LinearInterpolation(y, x; extrapolate=true)
    elseif length(x) == 3 || scheme == :quadratic
        itp = DataInterpolations.QuadraticSpline(y, x; extrapolate=true)
    elseif length(x) == 4 || scheme == :cubic
        itp = DataInterpolations.CubicSpline(y, x; extrapolate=true)
    elseif scheme == :lagrange
        n = length(y) - 1
        itp = DataInterpolations.LagrangeInterpolation(y, x, n; extrapolate=true)
    else
        error("interp1d scheme can only be :constant, :linear, :quadratic, :cubic, :pchip, :lagrange ")
    end
    return itp
end

"""
    extrap1d(itp::DataInterpolations.AbstractInterpolation; first=:extrapolate, last=:extrapolate) where {T<:Real}

`first` and `last` can be [:extrapolate, :flat, Floating] affect how the extrapolation is done at the either end of the array
"""
function extrap1d(itp::DataInterpolations.AbstractInterpolation; first=:extrapolate, last=:extrapolate)
    x = itp.t
    y = itp.u
    T = eltype(y)

    # handle extrapolation
    @assert first ∈ (:extrapolate, :flat) || typeof(first)
    @assert last ∈ (:extrapolate, :flat) || typeof(last)
    if first != :extrapolate && last != :extrapolate
        if first == :flat
            x0 = x[1]
            y0 = y[1]
        else
            x0 = x[1]
            y0 = first
        end
        if last == :flat
            x1 = x[end]
            y1 = y[end]
        else
            x1 = x[end]
            y1 = last
        end
        func = xx -> clip_01(itp, xx, x0, y0, x1, y1)::T

    elseif first != :extrapolate
        if first == :flat
            func = xx -> clip_0(itp, xx, x[1], y[1])::T
        else
            func = xx -> clip_0(itp, xx, x[1], first)::T
        end

    elseif last != :extrapolate
        if last == :flat
            func = xx -> clip_1(itp, xx, x[end], y[end])::T
        else
            func = xx -> clip_1(itp, xx, x[end], last)::T
        end
    else
        func = xx -> itp(xx)::T
    end

    return func
end

function clip_01(f, x::Real, x0::Real, y0::T, x1::Real, y1::T) where {T<:Real}
    if x < x0
        return y0
    elseif x > x1
        return y1
    else
        return f(x)::T
    end
end

function clip_0(f, x::Real, x0::Real, y0::T) where {T<:Real}
    if x < x0
        return y0
    else
        return f(x)::T
    end
end

function clip_1(f, x::Real, x1::Real, y1::T) where {T<:Real}
    if x > x1
        return y1
    else
        return f(x)::T
    end
end