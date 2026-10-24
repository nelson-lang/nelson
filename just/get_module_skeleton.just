# Get the module skeleton
get_module_skeleton: build::build
    cmake --build . -- get_module_skeleton
