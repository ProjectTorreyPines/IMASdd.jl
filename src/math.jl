document[:Math] = Symbol[]
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

One dimensional curve interpolations with scheme `[:constant, :linear, :quadratic, :cubic, :pchip, :lagrange]`

NOTE: this interpolation method will extrapolate
"""
function interp1d(x::AbstractVector{<:Real}, y::AbstractVector{T}, scheme::Symbol=:linear) where {T<:Real}
    # NOTE: doing simply `itp = interp1d_itp(x, y, scheme)` breaks the type inference scheme.
    @assert length(x) == length(y) "Different lengths in interp1d(x,y):  $(length(x)) and $(length(y))"
    @assert scheme in (:constant, :linear, :quadratic, :cubic, :lagrange)
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

export interp1d
push!(document[:Math], :interp1d)

function interp1d_itp(x::AbstractVector{<:Real}, y::AbstractVector{T}, scheme::Symbol=:linear) where {T<:Real}
    # NOTE: doing simply `itp = interp1d_itp(x, y, scheme)` breaks the type inference scheme.
    @assert length(x) == length(y) "Different lengths in interp1d(x,y):  $(length(x)) and $(length(y))"
    @assert scheme in (:constant, :linear, :quadratic, :cubic, :lagrange)
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

`first` and `last` can be `[:extrapolate, :flat, --value--]` affect how the extrapolation is done at the either end of the array
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

export extrap1d
push!(document[:Math], :extrap1d)

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

function gradient(arr::AbstractVector; method::Symbol=:second_order)
    return gradient(1:length(arr), arr; method)
end

"""
    gradient(coord::AbstractVector{C}, arr::AbstractVector{A}; method::Symbol=:second_order) where {C<:Real, A<:Real}

The finite difference gradient. The returned gradient has the same shape as the input array.

`method` of the gradient can be one of [:backward, :central, :forward, :second_order, :third_order]

For `:central` the gradient is computed using second order accurate central differences in the interior points and first order accurate one-sides (forward or backward) differences at the boundaries.

For `:second_order` the gradient is computed using second order accurate central differences in the interior points, and 2nd order differences at the boundaries.

For `:third_order` the gradient is computed from the cubic spline passing through the points
"""
function gradient(coord::AbstractVector{C}, arr::AbstractVector{A}; method::Symbol=:second_order) where {C<:Real,A<:Real}
    grad = Array{promote_type(A, C)}(undef, length(arr))
    return gradient!(grad, coord, arr; method)
end

"""
    gradient!(grad::AbstractVector, coord::AbstractVector, arr::AbstractVector; method::Symbol=:second_order)

In place version of gradient(coord::AbstractVector, arr::AbstractVector; method::Symbol=:second_order)
"""
function gradient!(grad::Union{AbstractVector,SubArray{<:Real,1}}, coord::AbstractVector, arr::Union{AbstractVector,SubArray{<:Real,1}}; method::Symbol=:second_order)
    np = length(arr)
    @assert length(grad) == np "The length of your grad vector (length = $(length(grad))) is not equal to the length of your arr (length = $np)"
    @assert length(coord) == np "The length of your coord (length = $(length(coord))) is not equal to the length of your arr (length = $np)"

    if np < 3 && method == :second_order
        method = :central
    end

    if method ∈ [:central, :backward, :forward]
        # Forward difference at the beginning
        grad[1] = (arr[2] - arr[1]) / (coord[2] - coord[1])
        # backward difference at the end
        grad[end] = (arr[end] - arr[end-1]) / (coord[end] - coord[end-1])
    end

    if method == :third_order
        itp = DataInterpolations.CubicSpline(arr, coord)
        for k in eachindex(arr)
            grad[k] = DataInterpolations.derivative(itp, coord[k])
        end

    elseif method ∈ [:central, :second_order]
        # Central difference in interior using numpy method
        for p in 2:np-1
            hs = coord[p] - coord[p-1]
            fs = arr[p-1]
            hd = coord[p+1] - coord[p]
            fd = arr[p+1]
            grad[p] = (hs^2 * fd + (hd^2 - hs^2) * arr[p] - hd^2 * fs) / (hs * hd * (hd + hs))
        end
        if method == :second_order
            # Derived using Numerical Mathematics, section 10.10 and lecture notes by A. Yew
            # and checked against formula from A. Yew for the case of equal spacing.
            # Numerical Mathematics: A. Quarteroni, R. Sacco, F. Saleri, Springer (2007)
            #   https://sites.math.washington.edu/~morrow/464_17/sacco%20saleri%20numerical.pdf
            # A. Yew: Lecture notes for APMA 0160 at Brown University
            #    https://www.dam.brown.edu/people/alcyew/handouts/numdiff.pdf
            c = coord[2] - coord[1]
            d = coord[3] - coord[2]
            f = arr[1]
            g = arr[2]
            h = arr[3]
            ccd = c / (c + d)
            ccd2 = c^2 / (c + d)^2
            grad[1] = (-f * (1 - ccd2) + g - h * ccd2) / (c * (1 - ccd))
            c = coord[end-1] - coord[end]
            d = coord[end-2] - coord[end-1]
            f = arr[end]
            g = arr[end-1]
            h = arr[end-2]
            ccd = c / (c + d)
            ccd2 = c^2 / (c + d)^2
            grad[end] = (-f * (1 - ccd2) + g - h * ccd2) / (c * (1 - ccd))
        end

    elseif method == :backward
        for p in 2:np-1
            grad[p] = (arr[p] - arr[p-1]) / (coord[p] - coord[p-1])
        end

    elseif method == :forward
        for p in 2:np-1
            grad[p] = (arr[p+1] - arr[p]) / (coord[p+1] - coord[p])
        end

    else
        error("difference method $(method) doesn't exist in gradient function. Can use one of [:backward, :central, :forward, :second_order, :third_order]")
    end

    return grad
end

function gradient(mat::Matrix; method::Symbol=:second_order)
    return gradient(1:size(mat)[1], 1:size(mat)[2], mat; method)
end

function gradient(mat::Matrix, dim::Int; method::Symbol=:second_order)
    return gradient(1:size(mat)[1], 1:size(mat)[2], mat, dim; method)
end

"""
    gradient(coord1::AbstractVector, coord2::AbstractVector, mat::Matrix, dim::Int; method::Symbol=:second_order)

Finite difference method of the gradient: [:backward, :central, :forward, :second_order, :third_order]

Can be applied to either the first (dim=1) or second (dim=2) dimension
"""
function gradient(coord1::AbstractVector, coord2::AbstractVector, mat::Matrix, dim::Int; method::Symbol=:second_order)
    nrows, ncols = size(mat)
    d = Matrix{eltype(mat)}(undef, nrows, ncols)
    if dim == 1
        for i in 1:ncols
            gradient!(@views(d[:, i]), coord1, @views(mat[:, i]); method)
        end
        return d
    elseif dim == 2
        for i in 1:nrows
            gradient!(@views(d[i, :]), coord2, @views(mat[i, :]); method)
        end
        return d
    else
        throw(ArgumentError("dim should be either 1 or 2"))
    end
end

"""
    gradient(coord1::AbstractVector, coord2::AbstractVector, mat::Matrix; method::Symbol=:second_order)

Finite difference method of the gradient: [:backward, :central, :forward, :second_order, :third_order]

Computes the gradient in both dimensions
"""
function gradient(coord1::AbstractVector, coord2::AbstractVector, mat::Matrix; method::Symbol=:second_order)
    d1 = gradient(coord1, coord2, mat, 1; method)
    d2 = gradient(coord1, coord2, mat, 2; method)
    return d1, d2
end

export gradient
push!(document[:Math], :gradient)