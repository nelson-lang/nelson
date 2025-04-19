# Clean generated files
clean:
    #!/usr/bin/env bash
    if [ -f "CMakeCache.txt" ]; then
        cmake --build . --target clean
    fi
    rm -rf CMakeFiles CMakeCache.txt Makefile cmake_install.cmake
    rm -rf node_modules
    rm -rf .qt
    rm -rf build.ninja .ninja_log CPackageConfig.cmake CPackageSource.cmake
    rm -rf NelsonConfigVersion.cmake
