import IMASdd
using IMASdd: ulocation, IDS, IDSvector

# ====================================================
# Helper Functions
# ====================================================

function show_specialized_instances(method)
    try
        instances = filter(!isnothing, collect(method.specializations))
        if isempty(instances)
            printstyled("  ✓ No specializations\n", color=:green)
        else
            printstyled("  ✗ Found $(length(instances)) specializations:\n", color=:red)
            display(instances)
        end
    catch
        printstyled("  ✓ No specializations\n", color=:green)
    end
end

function show_specialized_ulocation_instances()
    ids_method = methods(ulocation, (IDS, Symbol))[2]  # ulocation(ids::IDS, field::Symbol)
    println("="^50)
    println("Method: ulocation(ids::IDS, field::Symbol)")
    show_specialized_instances(ids_method)
    println("="^50)
end

# ====================================================
# Tests: Expected NO Specializations
# ====================================================

## Test1
printstyled("\n\nTest 1: Baseline (no code executed)\n", bold=true)
show_specialized_ulocation_instances()

## Test2
printstyled("\n\nTest 2: After DD creation\n", bold=true)
dd = IMASdd.dd()
show_specialized_ulocation_instances()

## Test3
printstyled("\n\nTest 3: Direct ulocation() calls\n", bold=true)
ulocation(dd, :equilibrium)
ulocation(dd.equilibrium, :time_slice)
ulocation(dd.core_profiles, :profiles_1d)
show_specialized_ulocation_instances()

## Test4
printstyled("\n\nTest 4: Via getproperty()\n", bold=true)
eq = getproperty(dd, :equilibrium)
show_specialized_ulocation_instances()

## Test5
printstyled("\n\nTest 5: Wrapper with @nospecialize\n", bold=true)
function wrapper_nospecialize(@nospecialize(ids::IDS), field::Symbol)
    return ulocation(ids, field)
end
wrapper_nospecialize(dd.core_profiles, :profiles_1d)
show_specialized_ulocation_instances()

## Test6
printstyled("\n\nTest 6: Wrapper with invokelatest\n", bold=true)
@inline function wrapper_with_invokelatest(ids::IDS, field::Symbol)
    return Base.invokelatest(ulocation, ids, field)
end
wrapper_with_invokelatest(dd.core_profiles, :profiles_1d)
show_specialized_ulocation_instances()


# ====================================================
# Tests: EXPECTED Specializations (Anti-patterns)
# ====================================================
printstyled("\n\n⚠️  Unintended specializations WILL occur below\n\n", color=:yellow, bold=true)

## Test7
printstyled("\n\nTest 7: @nospecialize with where {T<:IDS}\n", bold=true)
function wrapper_with_typeparam(@nospecialize(ids::T), field::Symbol) where {T<:IDS}
    return ulocation(ids, field)
end
wrapper_with_typeparam(dd.equilibrium, :ids_properties)
show_specialized_ulocation_instances()

## Test8
printstyled("\n\nTest 8: Wrapper WITHOUT @nospecialize\n", bold=true)
function wrapper_pure(ids::IDS, field::Symbol)
    return ulocation(ids, field)
end
wrapper_pure(dd.core_profiles, :vacuum_toroidal_field)
show_specialized_ulocation_instances()

## Test9
printstyled("\n\nTest 9: @noinline without @nospecialize\n", bold=true)
@noinline function wrapper_noinline(ids::IDS, field::Symbol)
    return ulocation(ids, field)
end
wrapper_noinline(dd.core_profiles.global_quantities, :ion)
show_specialized_ulocation_instances()

## Test10
printstyled("\n\nTest 10: @constprop :none @noinline without @nospecialize\n", bold=true)
Base.@constprop :none @noinline function wrapper_constprop(ids::IDS, field::Symbol)
    return ulocation(ids, field)
end
wrapper_constprop(dd.core_sources, :time)
show_specialized_ulocation_instances()

## Test11
printstyled("\n\nTest 11: Practical case (loading HDF5 file)\n", bold=true)
@time dd_h5 = IMASdd.hdf2imas(joinpath(pkgdir(IMASdd), "sample", "omas_sample_with_attrs.h5"));
show_specialized_ulocation_instances()