# IMASDD.jl
`IMASDD.jl` is a Julia package that allows manipulating data according to the ITER Modeling and Analysis Suite (IMAS) data schema, also known as the ITER Physics Data Model (PDM) [Imbeaux NF 2015]. Importantly, `IMASDD.jl` does not use the native IMAS API, but instead implements everything natively in Julia.

## Data schema
IMAS structures data around nearly 80 hierarchically ordered Interface Data Structures (IDSs). These IDSs span various modeling topics like equilibrium, kinetic profiles, and sources, as well as experimental areas including magnets, diagnostics, and heating systems. Each IDS encapsulates the necessary data pertaining to its associated plasma or tokamak subsystem. Within an IDS, every quantity is clearly described, specifying units, coordinates, numerical type. These standardized IDSs to ensure consistent data exchange across code components, and facilitate code coupling in fusion Tokamak integrated simulations.

## Time
A standout feature of `IMASDD.jl` is its ability to easily manage IDSs that are non-homogeneous in time. This capability is crucial for facilitating the creation of comprehensive time-dependent simulations. The process is streamlined by introducing a concept of a `global_time`. When this global time of interest is defined, the data structure's API takes charge. Instead of the user having to manually decipher the time coordinate for each accessed element, the API determines it and accordingly interpolates (or updates, if writing) the data for that specific time. Moreover, for efficient management of extensive time series, `IMAS.jl` employs a memory-saving strategy: it stores only the differences between consecutive time slices within an array of structures, rather than the entirety of each slice.

## I/O
The `IMASDD.jl` package also retains the ability to interoperate with the original IMAS infrastructure by directly reading and writing HDF5 binary files using the native "tensorized" IMAS data format [Meneghini NF 2020]. In addition `IMASDD.jl` supports reading and writing data in the JSON ASCII format, which has proven to find broad adoption among different projects that use the OMAS Python library. The ability to I/O data in these data files does not depend on either the original IMAS infrastructure nor OMAS being installed

## Online documentation
For more details, see the [online documentation](https://projecttorreypines.github.io/IMASDD.jl/dev).

![Docs](https://github.com/ProjectTorreyPines/IMASDD.jl/actions/workflows/make_docs.yml/badge.svg)