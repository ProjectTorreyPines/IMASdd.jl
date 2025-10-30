using PrecompileTools: @compile_workload

@compile_workload begin
	# call dd here to cache precompiled data structure
	empty_dd = IMASdd.dd()

	# File I/O
	dd_h5 = IMASdd.hdf2imas(joinpath(pkgdir(IMASdd), "sample", "omas_sample_with_attrs.h5"))
	dd_json = IMASdd.json2imas(joinpath(pkgdir(IMASdd), "sample", "omas_sample.json"))
	mktempdir() do tmpdir
		IMASdd.imas2hdf(dd_h5, joinpath(tmpdir, "imas_sample_copy.h5"))
		IMASdd.imas2json(dd_h5, joinpath(tmpdir, "imas_sample_copy.json"))
	end

	# Equality
	dd_h5 == dd_json

	# Deep copy
	dd_copy = deepcopy(dd_h5)

	# Compile REPL display methods (output to devnull to avoid clutter)
	show(devnull, MIME"text/plain"(), dd_h5;  maxdepth=99)
end