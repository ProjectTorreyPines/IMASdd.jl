# imas_composer from Julia: environment recipe

This sets up a conda env with [imas_composer](https://github.com/GA-FDP/imas_composer) and
toksearch that IMASdd.jl can call in-process through PythonCall.jl. CondaPkg.jl does not
manage the Python env. You build it with micromamba, activate it, and start Julia inside it.

## Files

| file | purpose |
|---|---|
| `environment_julia_pins.yaml` | Overlay on top of imas_composer's `environment_dev.yaml` that caps the libraries Julia also loads |
| `Project.toml` / `Manifest.toml` | Julia env: IMASdd (this checkout) + PythonCall, CondaPkg backend set to `Null` |
| `smoke_test.jl` | Imports the stack, loads every IDS for one shot and checks the awkward → Julia conversion |

## Setup

You need micromamba (or mamba), Julia ≥ 1.11, and SSH access to the GA-FDP GitHub org (for
the toksearch submodule). The commands below assume `imas_composer` and `IMASdd.jl` are
cloned side by side.

```bash
git clone --recurse-submodules git@github.com:GA-FDP/imas_composer.git

# 1. Conda env: imas_composer's dev env + Julia compatibility pins
micromamba create -n imas_composer_julia \
    -f imas_composer/environment_dev.yaml \
    -f IMASdd.jl/deploy/container/environment_julia_pins.yaml
micromamba activate imas_composer_julia

# 2. Editable installs (toksearch is unreleased, so it comes from the submodule)
for d in imas_composer imas_composer/toksearch_submodule imas_composer/omas_submodule; do
    pip install --no-deps --no-build-isolation -e "$d"
done

# 3. CA bundle for the FDP tunnel (see below), stored in the env and applied on activation
micromamba env config vars set FDP_TUNNEL_CAINFO="$CONDA_PREFIX/ssl/cacert.pem"
micromamba deactivate && micromamba activate imas_composer_julia

# 4. Julia env
julia --project=IMASdd.jl/deploy/container -e 'using Pkg; Pkg.instantiate()'
julia --project=IMASdd.jl/deploy/container IMASdd.jl/deploy/container/smoke_test.jl
```

The smoke test should print `OK` for every import and the python path inside
`imas_composer_julia`. It then loads every IDS for one shot, which takes a few minutes
(magnetics and pf_active are the slow ones). For every awkward field it checks that
`IMASdd.awkward_from_buffers` rebuilds the same values as `ak.to_list`, and it should end
with `awkward fields rebuilt: N/N match ak.to_list`. If your account can't use the FDP
archive, an `HTTP 401` from the fdp transport followed by a fallback to the next server is
normal. An `SSL peer certificate ... not OK` error means step 3 is missing.

Always start Julia from inside the activated env. PythonCall picks up `python` from `PATH`,
and activation sets `FDP_TUNNEL_CAINFO`.

## Why the extra steps

Python runs inside the Julia process, so Julia and conda share one set of loaded shared
libraries. When both ship a library with the same soname, whichever copy loads first is
used by both sides.

- **AWS C libraries (`environment_julia_pins.yaml`).** IMASdd depends on HDF5.jl, and
  HDF5_jll loads the `aws_c_*_jll` libraries. Conda's pyarrow (pulled in by pyspark) links
  the same libraries, which keep the soname `.so.1.0.0` across versions. With unpinned
  conda versions, `import awkward` fails with
  `undefined symbol: aws_byte_cursor_next_split_on_cursor`. The overlay keeps conda's copies
  no newer than Julia's, which currently costs pyarrow 25 → 24. When IMASdd's HDF5_jll
  moves, update the bounds to the `aws_c_*_jll` versions in its `Manifest.toml`.
- **TLS (`FDP_TUNNEL_CAINFO`).** Julia's libcurl, which the FDP tunnel in mdsip-fdp also
  ends up using, is built without a default CA path. `FDP_TUNNEL_CAINFO` tells mdsip-fdp
  where conda's CA bundle is.
- **curl and OpenSSL are deliberately not pinned.** Pinning them downgrades mdsip-fdp and
  xrdcl-pelican-fdp. Conda's curl 8.23 / OpenSSL 3.6 consumers work against Julia's
  8.22 / 3.5.
