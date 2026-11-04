module IMASddPythonCallExt

import IMASdd
import IMASdd: JSON
using PythonCall: Py, PyArray, pyconvert, pyimport, pyisinstance, pylist, pybuiltins

"""
    IMASdd.awkward_from_buffers(value::Py)

Rebuild an awkward array from the dict that `imas_composer.julia.julia_value` returns
(`form`, `length`, `buffers`), with zero-copy views into the numpy buffers.
"""
function IMASdd.awkward_from_buffers(value::Py)
    form = JSON.parse(pyconvert(String, value["form"]); dicttype=Dict{String,Any})
    buffers = Dict(key => PyArray(buffer) for (key, buffer) in pyconvert(Dict{String,Py}, value["buffers"]))
    return IMASdd.awkward_from_buffers(form, pyconvert(Int, value["length"]), buffers)
end

function IMASdd.imas_composer2imas(shot::Integer, fields::AbstractVector, @nospecialize(ids::IMASdd.DD)=IMASdd.dd_nospecialize();
    efit_tree::AbstractString="EFIT01", efit_run_id::AbstractString="",
    profiles_tree::AbstractString="ZIPFIT01", profiles_run_id::AbstractString="",
    crop_core_profiles::Bool=false)
    imas_composer = pyimport("imas_composer")
    # simple_load ignores its tree arguments when given a composer, so they go to the composer
    composer = imas_composer.ImasComposer(; efit_tree, efit_run_id, profiles_tree, profiles_run_id, crop_core_profiles)
    # fields overlap when an IDS and some of its fields are both asked for
    paths = unique(reduce(vcat, [composer_paths(composer, field) for field in fields]; init=String[]))

    data = pyimport("imas_composer.julia").simple_load_julia(pylist(paths), shot; composer)
    flat = Dict{String,Any}()
    for (path, value) in data.items()
        pyisinstance(value, imas_composer.NoData) && continue
        flat[pyconvert(String, path)] = julia_value(value)
    end
    return IMASdd.imas_composer_dict2imas(flat, ids)
end

# All fields imas_composer supports for an IDS
composer_paths(composer::Py, ids::Symbol) = pyconvert(Vector{String}, composer.get_supported_fields(string(ids)))

# A single field, which imas_composer must support
function composer_paths(composer::Py, path::AbstractString)
    path in pyconvert(Vector{String}, composer.get_supported_fields(split(path, ".")[1])) ||
        error("imas_composer does not support `$path`")
    return [String(path)]
end

# Julia value of one entry of simple_load_julia
function julia_value(value::Py)
    np = pyimport("numpy")
    if pyisinstance(value, pybuiltins.dict) # awkward buffers
        return IMASdd.awkward_from_buffers(value)
    elseif pyisinstance(value, np.ndarray)
        # PyArray does not map numpy unicode strings
        pyconvert(String, value.dtype.kind) == "U" && return julia_value(value.tolist())
        return PyArray(value)
    elseif pyisinstance(value, pybuiltins.list)
        return [julia_value(element) for element in value]
    elseif pyisinstance(value, np.generic)
        return pyconvert(Any, value.item())
    end
    return pyconvert(Any, value)
end

end
