module IMASddPythonCallExt

import IMASdd
import IMASdd: JSON
using PythonCall: Py, PyArray, pyconvert

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

end
