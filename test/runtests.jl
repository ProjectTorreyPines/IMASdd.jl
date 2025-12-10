# Check if specific test files are requested via ARGS
if !isempty(ARGS)
    for testfile in ARGS
        @info "Running test file: $testfile"
        include(testfile)
    end
else
    # Default behavior: run all tests
    include("runtests_concrete.jl")

    include("runtests_ids.jl")

    include("runtests_expressions.jl")

    include("runtests_time.jl")

    include("runtests_itp.jl")

    include("runtests_io.jl")

    include("runtests_io_extended.jl")

    include("runtests_findall.jl")

    include("runtests_filled.jl")

    include("runtests_frozen.jl")

    include("runtests_diagnostics.jl")
end
