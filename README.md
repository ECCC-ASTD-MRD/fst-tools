# Description

Collection of tools to manipulate RPN standard files

# Components

  * [dbzono](src/dbzono/README.md)
  * [editfst](src/editfst/README.md)
  * [fstcomp](src/fstcomp/README.md)
  * [fstcompress](src/fstcompress/README.md)
  * [fstxml](src/fstxml/README.md)
  * [pgsm](src/pgsm/README.md)
  * [reflex](src/reflex/README.md)
  * [voir](src/voir/README.md)

# Compilation

## At CMC

### Build dependencies

- CMake 3.20+
- librmn

### Environment

Load the right environment, depending on the architecture you need.  This
will load the specified compiler and its parameters, and set the
`EC_CMAKE_MODULE_PATH` variable for the `cmake_rpn` modules.

- Example for ppp6/sc6 and icelake specific architecture:

```
. r.load.dot mrd/rpn/code-tools/latest/env/rhel-8-icelake-64@inteloneapi-2025.1.0
```

- Example for generic architecture on ppp6/sc6

```
. r.load.dot mrd/rpn/code-tools/latest/env/rhel-8-amd64-64@inteloneapi-2025.1.0
```

- Example for GNU on any architecture:

```
. r.load.dot mrd/rpn/code-tools/latest/env/gnu
```

### Build and install

```
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=${your_choice}
make -j 4
make install
```

## Outside CMC (external users)

### Build dependencies

- CMake 3.20+
- librmn with shared libraries (https://github.com/ECCC-ASTD-MRD/librmn/)

`cmake_rpn` is included as a git submodule.  Please clone with the
`--recurse --remote-submodules` options, or run `git submodule update --init
--remote` in the git repo after having cloned.

### Build and install

```
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=${your_choice} -Drmn_ROOT=${librmn_install_path}
make -j 4
make install
```
