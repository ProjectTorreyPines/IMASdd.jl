using IMASdd
using PythonCall
using Libdl

println("python: ", PythonCall.C.CTX.exe_path)

for mod in ["numpy", "awkward", "MDSplus", "XRootD.client", "toksearch", "imas_composer"]
    m = pyimport(mod)
    println("OK   ", rpad(mod, 16), pyhasattr(m, "__file__") ? m.__file__ : "")
end

ak = pyimport("awkward")
np = pyimport("numpy")
imas_composer = pyimport("imas_composer")
julia_interface = pyimport("imas_composer.julia")

shot = 205055

# Small load through the Julia interface
data = julia_interface.simple_load_julia(pylist(["equilibrium.time", "equilibrium.time_slice.boundary.outline.r"]), shot)
outline_r = IMASdd.awkward_from_buffers(data["equilibrium.time_slice.boundary.outline.r"])
println("\nequilibrium.time: ", size(PyArray(data["equilibrium.time"])),
    "  boundary.outline.r: ", length(outline_r), " time slices, first has ", length(outline_r[1]), " points")

# Nested Julia copy of ak.to_list, the reference for awkward_from_buffers
to_julia(x::Py) = pyisinstance(x, pybuiltins.list) ? [to_julia(e) for e in x] : pyconvert(Any, x)

# Every awkward field of every IDS: buffers rebuilt in Julia must match ak.to_list
composer = imas_composer.ImasComposer()
n_checked, mismatches = 0, String[]
for ids in composer.ids_factory.list_ids()
    fields = composer.get_supported_fields(ids)
    t0 = time()
    values = imas_composer.simple_load(fields, shot, composer=composer)
    t_load = time() - t0

    t_convert, n_awkward = 0.0, 0
    for (path, value) in values.items()
        pyisinstance(value, ak.Array) || continue
        n_awkward += 1
        t0 = time()
        rebuilt = IMASdd.awkward_from_buffers(julia_interface.julia_value(value))
        t_convert += time() - t0
        isequal(rebuilt, to_julia(ak.to_list(ak.fill_none(value, np.nan)))) || push!(mismatches, pyconvert(String, path))
    end
    global n_checked += n_awkward
    println(rpad(pyconvert(String, ids), 24), lpad(n_awkward, 3), " awkward of ", lpad(length(fields), 3),
        " fields   load ", lpad(round(t_load; digits=1), 5), " s   convert ", lpad(round(t_convert * 1e3; digits=1), 6), " ms")
end
println("\nawkward fields rebuilt: ", n_checked - length(mismatches), "/", n_checked, " match ak.to_list")
foreach(path -> println("MISMATCH ", path), mismatches)

println("\nshared libraries Julia and Python both use:")
foreach(println, sort(filter(l -> occursin(r"libcurl|libssl|libcrypto|libaws-c-common", l), Libdl.dllist())))
