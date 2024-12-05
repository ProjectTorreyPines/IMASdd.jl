"""
    @custom_dd fields...

Generate a dd with only certain IDSs. For example:

    my_dd_type = @@custom_dd equilibrium core_profiles
    dd = my_dd_type()
"""
macro custom_dd(fields...)
    # Compute the hash of the field names
    hash_value = hash(join(string.(fields), ","))
    struct_name = Symbol("dd_", hash_value)

    # Base fields to include in all generated structs
    base_fields = [
        :(global_time::Float64),
        :(var"_aux"::Dict),
        :(var"_filled"::Set{Symbol}),
        :(var"_frozen"::Bool),
        :(var"_in_expression"::Vector{Symbol}),
        :(var"_ref"::Union{Nothing,dd}),
        :(var"_parent"::WeakRef)
    ]

    # Generate custom fields based on user-specified fields
    custom_fields = [:($(field)::$(field){T}) for field in fields]

    # Generate the constructor function
    field_inits = [:($(field){T}()) for field in fields]
    parent_inits = [:(setfield!(ids.$(field), :_parent, WeakRef(ids))) for field in fields]

    # Combine the struct definition and constructor function
    quote
        mutable struct $(esc(struct_name)){T} <: DD{T}
            $(custom_fields...)
            $(base_fields...)
        end

        function $(esc(struct_name)){T}() where {T}
            ids = $(esc(struct_name)){T}($(field_inits...), 0.0, Dict(), Set{Symbol}(), false, Symbol[], nothing, WeakRef(nothing))
            $(parent_inits...)
            return ids
        end

        $(esc(struct_name))() = $(esc(struct_name)){Float64}()

        $(esc(struct_name))
    end
end
