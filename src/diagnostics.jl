using Base: @nospecializeinfer

"""
    SharedObjectGroup

A group of paths sharing the same array object.

# Fields
- `id::UInt`: The objectid of the shared array
- `paths::Vector{String}`: List of paths that share this object
"""
const SharedObjectGroup = NamedTuple{(:id, :paths),Tuple{UInt,Vector{String}}}

"""
    SharedObjectReport

Result of object identity diagnostic containing shared objects information.

# Fields
- `shared::Vector{SharedObjectGroup}`: Vector of shared object groups, each containing objectid and paths
- `total_arrays::Int`: Total number of unique arrays examined

# Access
```julia
report.shared[1].id      # objectid of first shared group
report.shared[1].paths   # paths sharing that object
length(report.shared)    # number of shared groups
```
"""
struct SharedObjectReport
    shared::Vector{SharedObjectGroup}
    total_arrays::Int
end

function Base.isempty(report::SharedObjectReport)
    return isempty(report.shared)
end

Base.length(report::SharedObjectReport) = length(report.shared)
Base.getindex(report::SharedObjectReport, i::Int) = report.shared[i]
Base.iterate(report::SharedObjectReport) = iterate(report.shared)
Base.iterate(report::SharedObjectReport, state) = iterate(report.shared, state)

# Compact single-line representation
function Base.show(io::IO, report::SharedObjectReport)
    n_shared = length(report.shared)
    n_paths = sum(length(g.paths) for g in report.shared; init=0)
    print(io, "SharedObjectReport(", n_shared, " shared, ", n_paths, " paths, ", report.total_arrays, " arrays)")
end

# Full detailed representation (REPL display)
function Base.show(io::IO, ::MIME"text/plain", report::SharedObjectReport)
    if isempty(report.shared)
        printstyled(io, "No shared objects detected"; color=:green)
        print(io, " (", report.total_arrays, " arrays examined)")
        return
    end

    n_groups = length(report.shared)
    n_paths = sum(length(g.paths) for g in report.shared)
    printstyled(io, "Shared objects detected"; color=:yellow, bold=true)
    print(io, " ($n_groups groups, $n_paths paths, $(report.total_arrays) arrays examined):\n")
    printstyled(io, "  Tip: Use `.=` instead of `=` for array assignment to avoid sharing\n"; color=:light_black)

    for (i, group) in enumerate(report.shared)
        printstyled(io, "\n  [$i] objectid=0x$(string(group.id, base=16)):\n"; color=:cyan)
        # Reverse order: show first-assigned path first
        for path in reverse(group.paths)
            println(io, "      - $path")
        end
    end
end

export SharedObjectReport, SharedObjectGroup

"""
    _collect_object_ids(ids) -> Dict{UInt, Vector{String}}

Internal function to collect all array objectids and their paths from an IDS tree.
Returns a dictionary mapping objectid to list of paths where that object appears.
"""
@nospecializeinfer function _collect_object_ids(@nospecialize(ids::Union{IDS,IDSvector,DD}))
    result = Dict{UInt,Vector{String}}()
    stack = Vector{Tuple{Any,String}}()

    # Initialize stack
    if ids isa IDSvector
        for i in eachindex(ids)
            push!(stack, (ids[i], location(ids[i])))
        end
    elseif ids isa DD
        push!(stack, (ids, "dd"))
    else
        push!(stack, (ids, location(ids)))
    end

    while !isempty(stack)
        (obj, path) = pop!(stack)

        # Skip if not an IDS type
        typeof(obj) <: IDS || continue

        # Get fields excluding private ones
        target_fields = filter(x -> x ∉ private_fields, fieldnames(typeof(obj)))

        for field in target_fields
            # Skip missing fields
            ismissing(obj, field) && continue

            field_path = path * "." * String(field)
            field_value = getfield(obj, field)

            if field_value isa IDSvector
                # IDSvector: traverse elements
                for i in eachindex(field_value)
                    push!(stack, (field_value[i], field_path * "[$i]"))
                end
            elseif field_value isa IDS
                # Nested IDS: add to stack
                push!(stack, (field_value, field_path))
            elseif field_value isa AbstractArray
                # Array field: record objectid
                id = objectid(field_value)
                push!(get!(result, id, String[]), field_path)
            end
        end
    end

    return result
end

"""
    diagnose_shared_objects(ids; include_empty=false) -> SharedObjectReport

Diagnose unintended object sharing in an IDS tree by detecting arrays that share
the same objectid at different paths.

This helps identify cases where `a = b` was used instead of `a .= b`, causing
multiple fields to reference the same underlying array.

# Arguments
- `ids::Union{IDS, IDSvector, DD}`: The IDS object tree to diagnose

# Keyword Arguments
- `include_empty::Bool=false`: If true, include empty arrays in the analysis

# Returns
- `SharedObjectReport`: Contains vector of shared object groups and total array count

# Example
```julia
dd = IMASdd.dd()
resize!(dd.core_profiles.profiles_1d, 1)
cp1d = dd.core_profiles.profiles_1d[1]
cp1d.grid.rho_tor_norm = range(0, 1, 10)
resize!(cp1d.ion, 3)

# Create unintended sharing (bad practice)
shared_temp = rand(10)
for ion in cp1d.ion
    ion.temperature = shared_temp  # All ions now share the same array!
end

# Diagnose - report is displayed automatically in REPL
report = diagnose_shared_objects(dd.core_profiles)

# Access results programmatically
report[1].id       # objectid of first shared group
report[1].paths    # paths sharing that object
length(report)     # number of shared groups

# Fix: use broadcast assignment
for ion in cp1d.ion
    ion.temperature .= new_values  # Each ion keeps its own array
end
```
"""
@nospecializeinfer function diagnose_shared_objects(@nospecialize(ids::Union{IDS,IDSvector,DD}); include_empty::Bool=false)::SharedObjectReport
    all_ids = _collect_object_ids(ids)

    # Filter empty arrays if requested
    if !include_empty
        filter!(all_ids) do (id, paths)
            !isempty(paths) || return false
            return true
        end
    end

    # Filter to only shared objects (2+ paths) and convert to Vector
    shared = SharedObjectGroup[]
    for (id, paths) in all_ids
        if length(paths) > 1
            push!(shared, (id=id, paths=paths))
        end
    end

    return SharedObjectReport(shared, length(all_ids))
end

export diagnose_shared_objects

document[:diagnose_shared_objects] = """
    diagnose_shared_objects(ids; include_empty=false)

Diagnose unintended object sharing in an IDS tree.

Detects cases where multiple paths in the IDS tree reference the same underlying
array object (same objectid). This typically indicates incorrect usage of `=`
instead of `.=` for array assignment.

## Common Issue

In Julia, `a = b` creates a reference to `b`, not a copy:
```julia
a = [1, 2, 3]
b = a        # b and a are the same object
b[1] = 999   # a[1] is now also 999!
```

## Safe Assignment

Use broadcast assignment `.=` to copy values while preserving object identity:
```julia
a = zeros(3)
b = [1, 2, 3]
a .= b       # a gets values from b, but remains a separate object
```

See also: [`SharedObjectReport`](@ref)
"""
