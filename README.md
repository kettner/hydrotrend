Hydrotrend
===

You can find the official version of HydroTrend source code here:
https://github.com/kettern/hydrotrend


Install a pre-built version of HydroTrend
-----------------------------------------

To install a pre-built version of this package with conda run:

```bash
$ conda install hydrotrend -c conda-forge
```

This will grab the latest version of *HydroTrend* from the
[*conda-forge*](https://conda-forge.org/) and install it into your
current environment.

Build and install from source
-----------------------------

To build and install from source:

```bash
$ mkdir _build && cd _build
$ cmake .. -DCMAKE_INSTALL_PREFIX=$PREFIX -DCMAKE_BUILD_TYPE=Release
$ make all -j4
$ make install
```

Run HydroTrend
--------------

Once HydroTrend is installed, use the `--help` option to get a brief overview
of how to run it,

```bash
$ hydrotrend --help
```
To run the test example (using this repository),
```bash
$ cd data/input
$ hydrotrend --in-dir=. --out-dir=. --prefix=HYDRO
```
