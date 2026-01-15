document[:Math] = Symbol[]
import DataInterpolations: DataInterpolations, ExtrapolationType

"""
    TypedInterp{T,I}

Type-stable zero-overhead interpolation wrapper using @generated functions.
Eliminates closure allocation and maintains type stability for optimal performance.

# Type Parameters
- `T<:Real`: Element type of interpolated values
- `I<:DataInterpolations.AbstractInterpolation`: Underlying interpolation object

# Call Signatures
- `(ti::TypedInterp)(x)`: Evaluate at scalar or vector `x`
- `(ti::TypedInterp)(out, x)`: In-place evaluation into pre-allocated `out`

# Example
```julia
itp = interp1d(0.0:10.0, (0.0:10.0).^2, :cubic)
itp(5.5)              # Scalar evaluation
itp([2.5, 5.5])       # Direct vector evaluation (recommended)
itp.([2.5, 5.5])      # Broadcast evaluation (also works)
itp(out, [2.5, 5.5])  # In-place evaluation (zero allocations)
```
"""
struct TypedInterp{T<:Real,I<:DataInterpolations.AbstractInterpolation}
    itp::I
end

# Scalar/vector evaluation - compiles to specialized code per type combination
@generated function (ti::TypedInterp{T,I})(x) where {T,I}
    quote
        @inbounds ti.itp(x)
    end
end

# In-place evaluation - write results directly to pre-allocated output (zero allocations)
@generated function (ti::TypedInterp{T,I})(out::AbstractVector{T}, x::AbstractVector{T}) where {T<:Real,I}
    quote
        @inbounds ti.itp(out, x)
    end
end

# Helper constructor for type inference
TypedInterp{T}(itp::I) where {T,I} = TypedInterp{T,I}(itp)

# Make TypedInterp broadcastable as a scalar (enables itp.([1,2,3]) syntax)
Base.broadcastable(ti::TypedInterp) = Ref(ti)


"""
    interp1d(x, y, scheme::Symbol=:linear)

One dimensional curve interpolations with scheme `[:constant, :linear, :quadratic, :cubic, :pchip, :lagrange]`
For Integer and Rational data types, only the :constant scheme is supported.
NOTE: this interpolation method will extrapolate
"""
function interp1d(x::AbstractVector{<:Real}, y::AbstractVector{T}, scheme::Symbol=:linear) where {T<:Real}
    # NOTE: doing simply `itp = interp1d_itp(x, y, scheme)` breaks the type inference scheme.
    @assert length(x) == length(y) "Different lengths in interp1d(x,y):  $(length(x)) and $(length(y))"
    @assert scheme in (:constant, :linear, :quadratic, :cubic, :pchip, :lagrange)

    if T <: Union{Integer, Rational} && scheme != :constant
        throw(ArgumentError("Interpolation of Integer or Rational types is only supported for :constant scheme"))
    end

    # avoid infinity
    if any(isinf, x)
        x = noninf.(x)
    end

    # DataInterpolations assumes x is monotonically increasing
    if issorted(x; rev=true)
        x = reverse(x)
        y = reverse(y)
    elseif !issorted(x)
        throw(ArgumentError("x must be sorted"))
    end

    if length(x) == 1 || scheme == :constant || T <: Integer || T <: Rational
        itp = DataInterpolations.ConstantInterpolation(y, x; extrapolation=ExtrapolationType.Extension)
    elseif scheme == :pchip
        itp = DataInterpolations.PCHIPInterpolation(y, x; extrapolation=ExtrapolationType.Extension)
    elseif length(x) == 2 || scheme == :linear
        itp = DataInterpolations.LinearInterpolation(y, x; extrapolation=ExtrapolationType.Extension)
    elseif length(x) == 3 || scheme == :quadratic
        itp = DataInterpolations.QuadraticSpline(y, x; extrapolation=ExtrapolationType.Extension)
    elseif length(x) == 4 || scheme == :cubic
        itp = DataInterpolations.CubicSpline(y, x; extrapolation=ExtrapolationType.Extension)
    elseif scheme == :lagrange
        n = length(y) - 1
        itp = DataInterpolations.LagrangeInterpolation(y, x, n; extrapolation=ExtrapolationType.Extension)
    end

    # Returns the typed interpolation wrapper, which is type stable and zero-overhead
    return TypedInterp{T}(itp)
end

export interp1d
push!(document[:Math], :interp1d)

function noninf(xx)
    if isinf(xx)
        return xx < 0 ? nextfloat(xx) : prevfloat(xx)
    end
    return xx
end

function interp1d_itp(x::AbstractVector{<:Real}, y::AbstractVector{T}, scheme::Symbol=:linear) where {T<:Real}
    # NOTE: doing simply `itp = interp1d_itp(x, y, scheme)` breaks the type inference scheme.
    @assert length(x) == length(y) "Different lengths in interp1d(x,y):  $(length(x)) and $(length(y))"
    @assert scheme in (:constant, :linear, :quadratic, :cubic, :pchip, :lagrange)

    # avoid infinity
    if any(isinf, x)
        x = noninf.(x)
    end

    # DataInterpolations assumes x is monotonically increasing
    if issorted(x; rev=true)
        x = reverse(x)
        y = reverse(y)
    elseif !issorted(x)
        throw(ArgumentError("x must be sorted"))
    end

    if length(x) == 1 || scheme == :constant || T <: Integer
        itp = DataInterpolations.ConstantInterpolation(y, x; extrapolation=ExtrapolationType.Extension)
    elseif scheme == :pchip
        itp = DataInterpolations.PCHIPInterpolation(y, x; extrapolation=ExtrapolationType.Extension)
    elseif length(x) == 2 || scheme == :linear
        itp = DataInterpolations.LinearInterpolation(y, x; extrapolation=ExtrapolationType.Extension)
    elseif length(x) == 3 || scheme == :quadratic
        itp = DataInterpolations.QuadraticSpline(y, x; extrapolation=ExtrapolationType.Extension)
    elseif length(x) == 4 || scheme == :cubic
        itp = DataInterpolations.CubicSpline(y, x; extrapolation=ExtrapolationType.Extension)
    elseif scheme == :lagrange
        n = length(y) - 1
        itp = DataInterpolations.LagrangeInterpolation(y, x, n; extrapolation=ExtrapolationType.Extension)
    end

    return itp
end

"""
    extrap1d(itp::DataInterpolations.AbstractInterpolation; first=:extrapolate, last=:extrapolate) where {T<:Real}

`first` and `last` can be `[:extrapolate, :constant, :nan, --value--]` affect how the extrapolation is done at the either end of the array
"""
function extrap1d(itp::DataInterpolations.AbstractInterpolation; first=:extrapolate, last=:extrapolate)
    x = itp.t
    y = itp.u
    T = eltype(y)

    @assert first ∈ (:extrapolate, :constant, :error, :nan) || typeof(first) <: T
    @assert last ∈ (:extrapolate, :constant, :error, :nan) || typeof(last) <: T

    if first == :nan
        first = typed_nan(T)
    end
    if last == :nan
        last = typed_nan(T)
    end

    if typeof(first) <: T
        x0 = x[1]
        y0 = first
    else
        x0 = x[1]
        y0 = y[1]::T
    end
    if typeof(last) <: T
        x1 = x[end]
        y1 = last
    else
        x1 = x[end]
        y1 = y[end]::T
    end
    func = xx -> clip_01(itp, xx, x0, y0, x1, y1; first, last)::T

    return func
end

function clip_01(f, x::Real, x0::Real, y0::T, x1::Real, y1::T; first, last) where {T<:Real}
    x = noninf(x)
    if x < x0 && first == :extrapolate
        return f(x)::T
    elseif x < x0 && first == :error
        return error("Extrapolation not allowed at $(x) < $(x0)")
    elseif x < x0
        return y0
    elseif x > x1 && last == :extrapolate
        return y1
    elseif x > x1 && last == :error
        return error("Extrapolation not allowed at $(x) > $(x1)")
    elseif x > x1
        return y1
    else
        return f(x)::T
    end
end

export extrap1d
push!(document[:Math], :extrap1d)

function constant_time_interp(time::AbstractVector{Float64}, vector::AbstractVector{T}, time0::AbstractVector{Float64}; first::Symbol=:error, last::Symbol=:constant) where {T<:Any}
    return constant_time_interp!(Vector{T}(undef, length(time0)), time, vector, time0; first, last)
end

function constant_time_interp!(output::AbstractVector, time::AbstractVector, vector::AbstractVector, time0::AbstractVector; first::Symbol=:error, last::Symbol=:constant)
    @inbounds for i in eachindex(time0)
        output[i] = constant_time_interp(time, vector, time0[i]; first, last)
    end
    return output
end

# Scalar version
function constant_time_interp(time::AbstractVector{Float64}, vector::AbstractVector{T}, t::Float64; first::Symbol=:error, last::Symbol=:constant) where {T}
    n = length(time)
    @assert first in (:error, :nan, :constant)
    @assert last in (:error, :nan, :constant)
    if t < time[1]
        if first == :error
            error("Extrapolation not allowed at $(t) < $(time[1])")
        elseif first == :nan
            return typed_nan(T)
        elseif first == :constant
            return vector[1]
        end
    elseif t == time[1]
        return vector[1]
    elseif t == time[n]
        return vector[n]
    elseif t > time[n]
        if last == :error
            error("Extrapolation not allowed at $(t) > $(time[n])")
        elseif last == :nan
            return typed_nan(T)
        elseif last == :constant
            return vector[n]
        end
    else
        lo, hi = 1, n
        while hi - lo > 1
            mid = (lo + hi) >> 1
            if @inbounds(time[mid]) <= t
                lo = mid
            else
                hi = mid
            end
        end
        return vector[lo]
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

    if method ∈ (:central, :backward, :forward)
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

    elseif method ∈ (:central, :second_order)
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

"""
    nanmaximum(a::AbstractArray)

Maximum ignoring NaNs in an array
"""
function nanmaximum(a::AbstractArray)
    m = maximum(x -> isnan(x) ? -Inf : x, a)
    if m == -Inf && all(isnan.(a))
        return NaN
    else
        return m
    end
end

export nanmaximum
push!(document[:Math], :nanmaximum)

"""
    nanminimum(a::AbstractArray)

Minimum ignoring NaNs in an array
"""
function nanminimum(a::AbstractArray)
    m = minimum(x -> isnan(x) ? Inf : x, a)
    if m == Inf && all(isnan.(a))
        return NaN
    else
        return m
    end
end

export nanminimum
push!(document[:Math], :nanminimum)
