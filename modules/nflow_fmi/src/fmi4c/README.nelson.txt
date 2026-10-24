fmi4c vendored SOURCE from https://github.com/robbr48/fmi4c.git
pinned commit: cab0bb19954e3ce63c7b8e49d5312b4e6df6ba5f
Trimmed to the USE_SYSTEM_ZIP=ON configuration (only the fmi4c_loadUnzippedFmu
path is used by NFlow; the archive/minizip path is intentionally omitted).
Compiled in-tree by the module build (both MSBuild and CMake) on all platforms/arches.
links the prebuilt static lib in (compiled in-tree from modules/nflow_fmi/src/fmi4c).
