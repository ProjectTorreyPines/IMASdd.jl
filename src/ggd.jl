#= =========================================================================== =#
#  General Grid Description (GGD) helpers
#
#  Migrated from IMASggd.jl: these methods dispatch on Unions of IMASdd types,
#  so they belong here — defining them downstream constitutes type piracy and
#  triggers method-overwriting errors during precompilation when both packages
#  are loaded (e.g. in a PackageCompiler sysimage build).
#= =========================================================================== =#

"""
    all__grid_ggd

Union of all IMAS data dictionary `grid_ggd` types.
"""
const all__grid_ggd = Union{
    edge_profiles__grid_ggd{T},
    edge_sources__grid_ggd{T},
    edge_transport__grid_ggd{T},
    em_coupling__grid_ggd{T},
    ferritic__grid_ggd{T},
    mhd__grid_ggd{T},
    radiation__grid_ggd{T},
    runaway_electrons__grid_ggd{T},
    wall__description_ggd___grid_ggd{T},
} where {T}

"""
    all__space

Union of all `space` types that are attributes of `grid_ggd` objects in IMAS.
"""
const all__space = Union{
    distribution_sources__source___ggd___grid__space{T},
    distributions__distribution___ggd___grid__space{T},
    edge_profiles__grid_ggd___space{T},
    edge_sources__grid_ggd___space{T},
    edge_transport__grid_ggd___space{T},
    em_coupling__grid_ggd___space{T},
    equilibrium__grids_ggd___grid___space{T},
    ferritic__grid_ggd__space{T},
    mhd__grid_ggd___space{T},
    radiation__grid_ggd___space{T},
    runaway_electrons__grid_ggd___space{T},
    tf__field_map___grid__space{T},
    transport_solver_numerics__boundary_conditions_ggd___grid__space{T},
    wall__description_ggd___grid_ggd___space{T},
    waves__coherent_wave___full_wave___grid__space{T},
} where {T}

"""
    all__grid_subset

Union of all `grid_subset` types that are attributes of `grid_ggd` objects in IMAS.
"""
const all__grid_subset = Union{
    edge_profiles__grid_ggd___grid_subset{T},
    edge_sources__grid_ggd___grid_subset{T},
    edge_transport__grid_ggd___grid_subset{T},
    em_coupling__grid_ggd___grid_subset{T},
    ferritic__grid_ggd__grid_subset{T},
    mhd__grid_ggd___grid_subset{T},
    radiation__grid_ggd___grid_subset{T},
    runaway_electrons__grid_ggd___grid_subset{T},
    wall__description_ggd___grid_ggd___grid_subset{T},
} where {T}

# Resolve the grid_ggd instance referred to by `ids.path`, e.g.
# "edge_profiles/grid_ggd(1)" -> top_dd(ids).edge_profiles.grid_ggd[1]
function _resolve_ggd_path(ids::all__grid_ggd)
    path = getfield(ids, :path)
    ref_ids_name = Symbol(split(path, "/")[1])
    grid_ggd_ind = parse(Int, split(split(path, "(")[2], ")")[1])
    ref_ids = getfield(top_dd(ids), ref_ids_name)
    grid_ggd = getfield(ref_ids, :grid_ggd)
    return grid_ggd[grid_ggd_ind]
end

"""
    Base.getproperty(ids::all__grid_ggd, field::Symbol)

Link `grid_ggd` instances with each other: when a `grid_ggd` has `path` set to
another instance (e.g. `ids.radiation.grid_ggd[1].path = "edge_profiles/grid_ggd(1)"`),
field access transparently returns attributes of the referred instance
(`ids.radiation.grid_ggd[1].grid_subset[36]` reads
`ids.edge_profiles.grid_ggd[1].grid_subset[36]`).

Without `path` set (and for the `path` field itself) this behaves exactly like
the other `IDSvectorRawElement` types: direct field access, no processing.
"""
@inline function Base.getproperty(ids::all__grid_ggd, field::Symbol)
    if field !== :path && !ismissing(ids, :path)
        return getfield(_resolve_ggd_path(ids), field)
    end
    return getfield(ids, field)
end

"""
    get_subset_boundary_inds(
        space::all__space,
        subset::all__grid_subset,
    )::Vector{Int}

Return indices of boundary elements of a subset: objects (of one dimension
lower) that appear an odd number of times among the subset elements' boundaries.
"""
function get_subset_boundary_inds(
    space::all__space,
    subset::all__grid_subset,
)::Vector{Int}
    nD = subset.element[1].object[1].dimension
    if nD > 1  # Only 2D (edges) and 3D (cells) subsets have boundaries
        nD_objects = space.objects_per_dimension[nD].object
        elements = [nD_objects[ele.object[1].index] for ele in subset.element]
        boundary_inds = Int[]
        for ele in elements
            symdiff!(boundary_inds, [bnd.index for bnd in ele.boundary])
        end
        return boundary_inds
    end
    return Int[] # 1D (nodes) subsets have no boundary
end

"""
    add_subset_element!(
        subset::all__grid_subset,
        sn::Int,
        dim::Int,
        index::Union{Int,Vector{Int}},
        in_subset=(x...) -> true;
        kwargs...,
    )

Append element(s) with the given space number, dimension, and object index
(or indices) to a grid_subset, optionally filtered by `in_subset(; kwargs...)`.
"""
function add_subset_element!(
    subset::all__grid_subset,
    sn::Int,
    dim::Int,
    index::Int,
    in_subset=(x...) -> true;
    kwargs...,
)
    if in_subset(; kwargs...)
        dd_ind = length(subset.element) + 1
        resize!(subset.element, dd_ind)
        resize!(subset.element[dd_ind].object, 1)
        subset.element[dd_ind].object[1].space = sn
        subset.element[dd_ind].object[1].dimension = dim
        subset.element[dd_ind].object[1].index = index
    end
end

function add_subset_element!(
    subset::all__grid_subset,
    sn::Int,
    dim::Int,
    index::Vector{Int},
    in_subset=(x...) -> true;
    kwargs...,
)
    if in_subset(; kwargs...)
        dd_start_ind = length(subset.element) + 1
        resize!(subset.element, length(subset.element) + length(index))
        dd_stop_ind = length(subset.element)
        for (ii, dd_ind) in enumerate(dd_start_ind:dd_stop_ind)
            resize!(subset.element[dd_ind].object, 1)
            subset.element[dd_ind].object[1].space = sn
            subset.element[dd_ind].object[1].dimension = dim
            subset.element[dd_ind].object[1].index = index[ii]
        end
    end
end

"""
    get_subset_boundary(
        space::all__space,
        subset::all__grid_subset,
    )::all__grid_subset

Return a grid_subset holding the boundary of the subset provided (the dimension
of its elements is reduced by 1).
"""
function get_subset_boundary(
    space::all__space,
    subset::all__grid_subset,
)::all__grid_subset
    ret_subset = typeof(subset)()
    boundary_inds = get_subset_boundary_inds(space, subset)
    bnd_dim = subset.element[1].object[1].dimension - 1
    space_number = subset.element[1].object[1].space
    add_subset_element!(ret_subset, space_number, bnd_dim, boundary_inds)
    return ret_subset
end

"""
    Base.:∈(
        point::Tuple{Real,Real},
        subset_of_space::Tuple{all__grid_subset,all__space},
    )::Bool

Test whether an `(r, z)` point lies inside a grid_subset of a space:

    (5.5, 0.0) ∈ (subset_sol, space)
"""
function Base.:∈(
    point::Tuple{Real,Real},
    subset_of_space::Tuple{all__grid_subset,all__space},
)::Bool
    r, z = point
    subset, space = subset_of_space
    dim = getfield(getfield(getfield(subset, :element)[1], :object)[1], :dimension)
    opd = getfield(space, :objects_per_dimension)
    nodes = getfield(opd[1], :object)
    edges = getfield(opd[2], :object)
    if dim == 3
        subset_bnd = get_subset_boundary(space, subset)
    elseif dim == 2
        subset_bnd = subset
    elseif dim == 1
        for ele in getfield(subset, :element)
            node = nodes[getfield(getfield(ele, :object)[1], :index)]
            if node.geometry[1] == r && node.geometry[2] == z
                return true
            end
        end
        return false
    else
        error("Dimension ", dim, " is not supported yet.")
    end
    # Count number of times an upward going ray from (r,z) intersects the boundary
    count = 0
    for ele in getfield(subset_bnd, :element)
        edge = edges[getfield(getfield(ele, :object)[1], :index)]
        edge_nodes_r = zeros(2)
        edge_nodes_z = zeros(2)
        for (ii, node) in enumerate(getfield(edge, :nodes))
            edge_nodes_r[ii] = getfield(nodes[node], :geometry)[1]
            edge_nodes_z[ii] = getfield(nodes[node], :geometry)[2]
        end
        r_max = maximum(edge_nodes_r)
        r_min = minimum(edge_nodes_r)
        if r_min <= r < r_max
            z_max = maximum(edge_nodes_z)
            if z < z_max
                count += 1
            end
        end
    end
    # If it intersects an odd number of times, the point is inside the subset
    return isodd(count)
end
