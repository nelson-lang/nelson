#!/usr/bin/env just --justfile

os := if `uname` == "Darwin" { "macOS" } else { `uname | tr '[:upper:]' '[:lower:]'` }

default:
    @just --list

clean:
    @echo "Clean all..."
    cmake --build . --target clean
    rm -rf CMakeFiles CMakeCache.txt Makefile cmake_install.cmake
    rm -rf node_modules
    rm -rf .qt
    rm -rf build.ninja .ninja_log CPackageConfig.cmake CPackageSource.cmake
    rm -rf NelsonConfigVersion.cmake

config *args=' -G "Unix Makefiles" .':
    @echo "Configure..."
    echo "Configure with args: {{args}}"
    cmake {{args}}

install:
    @echo "Installing..."
    cmake --install .

build:
    @echo "Building..."
    cmake --build . -- -j$(nproc)

build_help:
    @echo "Building helps..."
    cmake --build . -- buildhelp

get_module_skeleton: build
    @echo "Get module skeleton"
    cmake --build . -- get_module_skeleton

tests:
    @echo "Running tests..."
    ./bin/{{os}}/nelson --adv-cli -noipc --quiet -f ./tools/tests_all/runtests_all.m

minimal_tests:
    @echo "Running minimal tests..."
    cmake --build . -- tests_minimal

benchmarks:
    @echo "Running benchmarks..."
    cmake --build . -- benchmark_all

benchmarks_no_display:
    @echo "Running benchmarks without display..."
    cmake --build . -- -j $(nproc) benchmark_all_no_display

tests_no_display:
    @echo "Running tests without display..."
    cmake --build . -- -j $(nproc) tests_all_no_display    

prettier:
    @echo "Prettier"
    npm run prettier

package:
    @echo "Packaging..."
    cmake --build . -- package

update_version:
    @echo "Updating version..."
    python ./tools/update_version/update_version.py

setup_python_env:
    @echo "Setup python environment..."
    ./bin/{{os}}/nelson --cli --noipc --quiet -f ./tools/python_environment_CI/configurePythonEnvironment.m

setup_julia_env:
    @echo "Setup julia environment..."
    ./bin/{{os}}/nelson --cli  --noipc --quiet -f $GITHUB_WORKSPACE/tools/julia_environment_CI/configureJuliaEnvironment.m

start:
    ./bin/{{os}}/nelson
