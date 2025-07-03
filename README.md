[![CI](https://github.com/ProjectTorreyPines/IMASdd.jl/actions/workflows/runtests.yml/badge.svg)](https://github.com/ProjectTorreyPines/IMASdd.jl/actions/workflows/runtests.yml)
[![codecov](https://codecov.io/github/ProjectTorreyPines/IMASdd.jl/graph/badge.svg?token=H6OK3LEP60)](https://codecov.io/github/ProjectTorreyPines/IMASdd.jl)

# IMASdd.jl

`IMASdd.jl` is a Julia package that allows manipulating data according to the ITER Modeling and Analysis Suite (IMAS) data schema, also known as the ITER Physics Data Model (PDM) [Imbeaux NF 2015]. Importantly, `IMASdd.jl` does not use the native IMAS API, but instead implements everything natively in Julia.

## Data schema

IMAS structures data around nearly 80 hierarchically ordered Interface Data Structures (IDSs). These IDSs span various modeling topics like equilibrium, kinetic profiles, and sources, as well as experimental areas including magnets, diagnostics, and heating systems. Each IDS encapsulates the necessary data pertaining to its associated plasma or tokamak subsystem. Within an IDS, every quantity is clearly described, specifying units, coordinates, and numerical type. These standardized IDSs ensure consistent data exchange across code components and facilitate code coupling in fusion Tokamak integrated simulations.

> **Note:** To update the data structure:
>
> 1. Check that the quantity you want to store does not fit in the original IMAS ontology: [IMAS Ontology](https://gafusion.github.io/omas/schema.html)
> 2. Check again!
> 3. [Open an issue](https://github.com/ProjectTorreyPines/IMASdd.jl/issues) asking if anybody has any ideas.
> 4. If indeed there's no proper place to store your data, then think carefully about where things should go and edit the JSON files under `IMASdd/data_dictionary/data_structures_extra` accordingly. Commit your changes to the JSON files, and a GitHub action will take care of generating the associated `IMASdd/src/dd.jl` right in your working branch.

## Time

A standout feature of `IMASdd.jl` is its ability to easily manage IDSs that are non-homogeneous in time. This capability is crucial for facilitating the creation of comprehensive time-dependent simulations. The process is streamlined by introducing the concept of a `global_time`. When this global time of interest is defined, the data structure's API takes charge. Instead of the user having to manually decipher the time coordinate for each accessed element, the API determines it and accordingly interpolates (or updates, if writing) the data for that specific time.

## Expressions

A distinguishing feature of `IMASdd.jl` is its ability to lazily evaluate derived quantities within the data structure. Such a system of dynamic expressions ensures consistency and provides an elegant solution to the mismatching-interfaces problem, where preceding models might not furnish all the derived data needed by subsequent models.

## I/O

The `IMASdd.jl` package also retains the ability to interoperate with the original IMAS infrastructure by directly reading and writing HDF5 binary files using the native "tensorized" IMAS data format [Meneghini NF 2020]. In addition, `IMASdd.jl` supports reading and writing data in the hierarchical HDF5 and JSON ASCII format which are broadly used across different projects based on the OMAS Python library. The ability to I/O data in these data files does not depend on either the original IMAS infrastructure nor OMAS being installed.

Note that the error of uncertain quantities that in IMAS are stored in `_error_upper` fields, in `IMASdd.jl` they are stored as `__error`. However, when saved to file (whichever format) these are saved as `_error_upper`, thus ensuring full compatibility with both IMAS and OMAS.

## Online documentation

For more details, see the [online documentation](https://projecttorreypines.github.io/IMASdd.jl/dev).

![Docs](https://github.com/ProjectTorreyPines/IMASdd.jl/actions/workflows/make_docs.yml/badge.svg)
