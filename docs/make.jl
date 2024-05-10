using Documenter, IMASDD

# Call functions
open("src/api.md", "w") do f
    println(f, "# API Reference\n")
    for page in keys(IMASDD.document)
        if page == :Expressions
            continue
        end
        println(f, "## $page\n")
        println(f, "```@docs")
        for item in IMASDD.document[page]
            println(f, "$item")
        end
        println(f, "```")
    end
end

makedocs(;
    modules=[IMASDD],
    format=Documenter.HTML(),
    sitename="IMASDD",
    checkdocs=:none,
)

